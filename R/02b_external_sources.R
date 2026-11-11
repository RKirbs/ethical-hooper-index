# 02b_external_sources.R
# Parses the cached Basketball Reference and pbpstats.com files and links them to
# NBA person IDs (the key used by the play-by-play).
#   bbref_totals.parquet    official minutes / games / position per player-team-season
#   pbpstats.parquet        pbpstats player-season totals (cross-check + possessions)
#   data/final/player_id_map.csv   bbref_id <-> NBA personId (small, committed)
# Why: stats.nba.com is blocked from the author's network and pbpstats is
# incomplete before 2000-01, so official minutes and positions come from BBRef.

source("R/utils.R")
suppressPackageStartupMessages({ library(arrow); library(rvest) })

# ---- Basketball Reference ------------------------------------------------------------

# Parse one BBRef stats table into a data.table keyed by data-stat names. BBRef
# hides some tables inside HTML comments, so comment markers are stripped first.
parse_bbref_table <- function(file, table_id) {
  txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  txt <- gsub("<!--|-->", "", txt)
  h <- read_html(txt)
  rows <- html_elements(h, sprintf("table#%s tbody tr:not(.thead):not(.over_header)", table_id))
  if (!length(rows)) return(NULL)
  rbindlist(lapply(rows, function(r) {
    cells <- html_elements(r, "th, td")
    stat <- html_attr(cells, "data-stat")
    # some BBRef pages (e.g. playoff totals) still use the legacy column names
    legacy <- c(player = "name_display", team_id = "team_name_abbr", g = "games", gs = "games_started")
    hit <- stat %in% names(legacy); stat[hit] <- legacy[stat[hit]]
    v <- as.list(html_text(cells)); names(v) <- stat
    id <- html_attr(cells[stat == "name_display"], "data-append-csv")
    if (!length(id) || is.na(id)) return(NULL)
    v$bbref_id <- id
    as.data.table(v)
  }), fill = TRUE)
}

# Remove accents, punctuation and suffixes so "Luka Dončić" == "Luka Doncic"
norm_name <- function(x) {
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z ]", "", x))
  x <- gsub("\\b(jr|sr|ii|iii|iv)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}

to_num <- function(x) suppressWarnings(as.numeric(x))

# All RS + PO totals tables -> one data.table (rows per player x team, plus
# "2TM"/"TOT" combined rows for traded players, flagged with is_total).
build_bbref_totals <- function() {
  out <- list()
  for (y in SEASONS) for (po in c(FALSE, TRUE)) {
    f <- file.path(paths$bbref, sprintf(if (po) "NBA_%d_playoffs_totals.html" else "NBA_%d_totals.html", y + 1))
    if (!file.exists(f)) { warning("missing ", f); next }
    d <- parse_bbref_table(f, "totals_stats")
    if (is.null(d)) { warning("no table in ", f); next }
    d <- d[, .(bbref_id, name = name_display, team = team_name_abbr, pos,
               age = to_num(age), g = to_num(games), mp = to_num(mp),
               fga = to_num(fga), fg3a = to_num(fg3a), fta = to_num(fta),
               tov = to_num(tov), pts = to_num(pts))]
    d[, `:=`(season = season_label(y), season_type = if (po) "PO" else "RS",
             is_total = grepl("^\\d+TM$|^TOT$", team))]
    out[[length(out) + 1]] <- d
  }
  rbindlist(out, fill = TRUE)
}

# Season-level BBRef play-by-play + shooting tables (cross-check only)
build_bbref_crosscheck <- function() {
  rbindlist(lapply(SEASONS, function(y) {
    fp <- file.path(paths$bbref, sprintf("NBA_%d_play-by-play.html", y + 1))
    fs <- file.path(paths$bbref, sprintf("NBA_%d_shooting.html", y + 1))
    if (!file.exists(fp) || !file.exists(fs)) return(NULL)
    p <- parse_bbref_table(fp, "pbp_stats")
    s <- parse_bbref_table(fs, "shooting")
    p <- p[, .(bbref_id, team = team_name_abbr, mp = to_num(mp),
               bb_drawn_shooting = to_num(drawn_shooting), bb_and1 = to_num(and1s),
               bb_fouls_offensive = to_num(fouls_offensive))]
    ast_cols <- grep("pct_ast|ast", names(s), value = TRUE)
    s <- s[, c("bbref_id", "team_name_abbr", ast_cols), with = FALSE]
    setnames(s, "team_name_abbr", "team")
    m <- merge(p, s, by = c("bbref_id", "team"), all.x = TRUE)
    m[, season := season_label(y)]
  }), fill = TRUE)
}

# ---- pbpstats.com ----------------------------------------------------------------------

PBPSTATS_KEEP <- c("EntityId", "Name", "TeamAbbreviation", "GamesPlayed", "SecondsPlayed", "OffPoss",
                   "Usage", "FG2A", "FG3A", "FTA", "Turnovers", "Travels", "Offensive Fouls",
                   "Charge Fouls", "ThreePtShootingFoulsDrawn", "TwoPtShootingFoulsDrawn",
                   "2pt And 1 Free Throw Trips", "3pt And 1 Free Throw Trips",
                   "PtsUnassisted2s", "PtsUnassisted3s", "PtsAssisted2s", "PtsAssisted3s", "HeaveAttempts")

build_pbpstats <- function() {
  files <- list.files(paths$pbpstats, "\\.json$", full.names = TRUE)
  rbindlist(lapply(files, function(f) {
    d <- as.data.table(jsonlite::fromJSON(f)$multi_row_table_data)
    for (k in setdiff(PBPSTATS_KEEP, names(d))) d[, (k) := NA]
    d <- d[, ..PBPSTATS_KEEP]
    setnames(d, make.names(names(d)))
    m <- regmatches(basename(f), regexec("totals_(\\d{4}-\\d{2})_(RS|PO)", basename(f)))[[1]]
    d[, `:=`(personId = as.integer(EntityId), season = m[2], season_type = m[3])]
  }), fill = TRUE)
}

# ---- ID map: bbref_id <-> NBA personId ------------------------------------------------------
# Within each season, match normalised full names (shot-chart PLAYER_NAME vs BBRef
# name). Ambiguous or unmatched names fall back to the closest FGA among
# same-surname candidates. Every pair is then verified by FGA agreement.
build_id_map <- function(bb, pts) {
  nba_names <- rbindlist(lapply(SEASONS, function(y) {
    s <- fread(file.path(paths$bulk, sprintf("shotdetail_%d.csv", y)), select = c("PLAYER_ID", "PLAYER_NAME"), showProgress = FALSE)
    unique(s)[, season := season_label(y)]
  }))
  nba_names <- nba_names[, .(name = PLAYER_NAME[1]), .(personId = PLAYER_ID, season)]
  nba_fga <- pts[season_type == "RS", .(fga_nba = as.numeric(sum(fga + heaves_excl))), .(personId, season)]
  nba <- merge(nba_names, nba_fga, by = c("personId", "season"), all = TRUE)
  nba[, key := norm_name(name)]

  bbs <- bb[season_type == "RS", .SD[which.max(fcoalesce(mp, 0))], .(bbref_id, season)]   # combined row if traded
  bbs[, key := norm_name(name)]

  exact <- merge(bbs[, .(bbref_id, season, key, fga_bb = fga)], nba[, .(personId, season, key, fga_nba)],
                 by = c("season", "key"))
  exact <- exact[, .SD[which.min(abs(fcoalesce(fga_bb, 0) - fcoalesce(fga_nba, 0)))], .(bbref_id, season)]
  exact <- exact[, .SD[which.min(abs(fcoalesce(fga_bb, 0) - fcoalesce(fga_nba, 0)))], .(personId, season)]

  # Fallback: same season, same last token, FGA within 3 -> accept
  left_bb  <- bbs[!exact, on = .(bbref_id, season)]
  left_nba <- nba[!exact, on = .(personId, season)]
  last_tok <- function(k) sub(".* ", "", k)
  fb <- merge(left_bb[, .(bbref_id, season, lt = last_tok(key), fga_bb = fga, name_bb = name)],
              left_nba[, .(personId, season, lt = last_tok(key), fga_nba, name_nba = name)],
              by = c("season", "lt"), allow.cartesian = TRUE)
  fb <- fb[abs(fcoalesce(fga_bb, 0) - fcoalesce(fga_nba, 0)) <= 3][
    , .SD[1], .(bbref_id, season)][, .SD[1], .(personId, season)]

  map <- rbind(exact[, .(bbref_id, personId, season, fga_bb, fga_nba, method = "exact")],
               fb[, .(bbref_id, personId, season, fga_bb, fga_nba, method = "surname+fga")])
  map
}

build_external <- function() {
  bb  <- build_bbref_totals()
  pbs <- build_pbpstats()
  pts <- as.data.table(read_parquet(file.path(paths$interim, "player_team_season.parquet")))
  map <- build_id_map(bb, pts)
  write_parquet(bb,  file.path(paths$interim, "bbref_totals.parquet"))
  write_parquet(pbs, file.path(paths$interim, "pbpstats.parquet"))
  fwrite(unique(map[, .(bbref_id, personId)]), file.path(paths$final, "player_id_map.csv"))
  fwrite(map, "data/audit/id_map_detail.csv")
  xc <- build_bbref_crosscheck()
  if (nrow(xc)) write_parquet(xc, file.path(paths$interim, "bbref_crosscheck.parquet"))
  invisible(list(bb = bb, pbs = pbs, map = map))
}

if (sys.nframe() == 0) {
  r <- build_external()
  m <- r$map
  cat("ID map rows:", nrow(m), "| by method:\n"); print(m[, .N, method])
  cat("FGA agreement |diff|<=2:", round(mean(abs(m$fga_bb - m$fga_nba) <= 2, na.rm = TRUE), 4), "\n")
}

# ---- Clean player-season table --------------------------------------------------------------
# One row per player x season x season type: PBP counting stats (summed over teams)
# + official minutes / games / position / age from BBRef (combined row for traded
# players) + pbpstats offensive possessions (only where pbpstats is complete, 2000-01+).
build_player_season <- function() {
  pts <- as.data.table(read_parquet(file.path(paths$interim, "player_team_season.parquet")))
  bb  <- as.data.table(read_parquet(file.path(paths$interim, "bbref_totals.parquet")))
  pbs <- as.data.table(read_parquet(file.path(paths$interim, "pbpstats.parquet")))
  idm <- fread(file.path(paths$final, "player_id_map.csv"))

  num <- names(pts)[vapply(pts, is.numeric, TRUE) & !names(pts) %in% c("personId", "teamId")]
  ps <- pts[, c(lapply(.SD, sum), list(n_teams = uniqueN(teamId), teamIds = paste(unique(teamId), collapse = ","))),
            .(personId, season, season_type), .SDcols = num]
  ps <- merge(ps, pts[, .(player_pbp = player[.N]), .(personId)][, .SD[1], personId], by = "personId", all.x = TRUE)

  bbc <- bb[, .SD[which.max(fcoalesce(mp, 0))], .(bbref_id, season, season_type)]
  bbc <- merge(bbc, idm, by = "bbref_id")
  # primary team = team with most minutes (for team-effect analysis later)
  prim <- bb[is_total == FALSE, .SD[which.max(fcoalesce(mp, 0))], .(bbref_id, season, season_type)][
    , .(bbref_id, season, season_type, primary_team = team)]
  bbc <- merge(bbc, prim, by = c("bbref_id", "season", "season_type"), all.x = TRUE)
  ps <- merge(ps, bbc[, .(personId, season, season_type, bbref_id, player = name, pos, age, g, mp,
                          bb_fga = fga, bb_fta = fta, bb_tov = tov, bb_pts = pts, primary_team)],
              by = c("personId", "season", "season_type"), all.x = TRUE)
  pbs_agg <- pbs[, .(off_poss = sum(as.numeric(OffPoss), na.rm = TRUE)), .(personId, season, season_type)]
  ps <- merge(ps, pbs_agg, by = c("personId", "season", "season_type"), all.x = TRUE)
  ps[season_year(season) < 2000, off_poss := NA_real_]
  ps[, player := fcoalesce(player, player_pbp)][, player_pbp := NULL]
  ps[, pts := 2 * (fgm - fg3m) + 3 * fg3m + ftm]
  ps
}
