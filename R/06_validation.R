# 06_validation.R
# Validation of the Ethical Hooper Index inputs and outputs.
# Section 1 (Phase 2): cross-source checks of PBP-derived counts against
#   Basketball Reference (independent parse) and pbpstats.com (same raw PBP,
#   different parser). Section 2+ (Phase 5): tracking validation, face
#   validity and year-to-year stability are added later.

source("R/utils.R")
suppressPackageStartupMessages(library(arrow))

# Player-season PBP counts summed over teams (traded players have >1 team row)
load_player_season_counts <- function(type = "RS") {
  as.data.table(read_parquet(file.path(paths$interim, "player_season.parquet")))[season_type == type]
}

# Agreement summary for one metric: correlation, mean |diff|, share within tolerance
agree <- function(ours, theirs, tol) {
  ok <- !is.na(ours) & !is.na(theirs)
  data.table(n = sum(ok), r = round(cor(ours[ok], theirs[ok]), 4),
             mean_abs_diff = round(mean(abs(ours[ok] - theirs[ok])), 2),
             pct_within_tol = round(100 * mean(abs(ours[ok] - theirs[ok]) <= tol), 1), tol = tol)
}

# ---- 1a. Basketball Reference: random sample of 25 qualifying-ish player-seasons ----
crosscheck_bbref <- function(n = 25, seed = 2026) {
  ours <- load_player_season_counts("RS")
  xc   <- as.data.table(read_parquet(file.path(paths$interim, "bbref_crosscheck.parquet")))
  idm  <- fread(file.path(paths$final, "player_id_map.csv"))
  # one BBRef row per player-season (the combined row for traded players)
  xc <- xc[, .SD[which.max(fcoalesce(mp, 0))], .(bbref_id, season)][, c("mp", "team") := NULL]
  xc <- merge(xc, idm, by = "bbref_id")
  m  <- merge(ours, xc, by = c("personId", "season", "bbref_id"))
  m  <- m[mp >= 1000]
  set.seed(seed)
  samp <- m[sample(.N, n)]
  ast2 <- "pct_ast_fg2"
  ast3 <- "pct_ast_fg3"
  samp[, `:=`(
    our_drawn_shooting = trips_and1 + trips_three_shot + trips_two_shoot,
    our_pct_ast2 = 100 * (1 - fg2m_unast / pmax(fgm - fg3m, 1)),
    our_pct_ast3 = 100 * (1 - fg3m_unast / pmax(fg3m, 1)),
    bb_pct_ast2  = 100 * to_pct(get(ast2)),
    bb_pct_ast3  = 100 * to_pct(get(ast3)))]
  summary <- rbind(
    cbind(metric = "Shooting fouls drawn", samp[, agree(our_drawn_shooting, bb_drawn_shooting, 5)]),
    cbind(metric = "And-ones",             samp[, agree(trips_and1, bb_and1, 3)]),
    cbind(metric = "Offensive fouls",      samp[, agree(off_fouls, bb_fouls_offensive, 3)]),
    cbind(metric = "% of 2PM assisted",    samp[, agree(our_pct_ast2, bb_pct_ast2, 2)]),
    cbind(metric = "% of 3PM assisted",    samp[fg3m >= 20, agree(our_pct_ast3, bb_pct_ast3, 2)]))
  detail <- samp[, .(season, player, mp,
                     drawn_ours = our_drawn_shooting, drawn_bbref = bb_drawn_shooting,
                     and1_ours = trips_and1, and1_bbref = bb_and1,
                     offf_ours = off_fouls, offf_bbref = bb_fouls_offensive,
                     ast2_ours = round(our_pct_ast2, 1), ast2_bbref = round(bb_pct_ast2, 1))]
  list(summary = summary, detail = detail[order(season)])
}
to_pct <- function(x) { v <- suppressWarnings(as.numeric(x)); ifelse(v > 1, v / 100, v) }

# ---- 1b. pbpstats.com: every player-season where pbpstats is complete --------------------
crosscheck_pbpstats <- function(min_fga = 200) {
  ours <- load_player_season_counts("RS")
  pb   <- as.data.table(read_parquet(file.path(paths$interim, "pbpstats.parquet")))[season_type == "RS"]
  pb   <- pb[, lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)), .(personId, season),
             .SDcols = c("FG2A", "FG3A", "FTA", "Turnovers", "Travels", "Offensive.Fouls", "Charge.Fouls",
                         "ThreePtShootingFoulsDrawn", "X3pt.And.1.Free.Throw.Trips", "X2pt.And.1.Free.Throw.Trips",
                         "PtsUnassisted2s", "PtsUnassisted3s", "HeaveAttempts")]
  m <- merge(ours, pb, by = c("personId", "season"))[fga >= min_fga]
  m[, era := fifelse(season_year(season) < 2000, "1996-97 to 1999-00", "2000-01 to 2025-26")]
  m[, {
    rbind(
      cbind(metric = "FGA (incl. heaves)", agree(fga + heaves_excl, FG2A + FG3A, 2)),
      cbind(metric = "FTA",                agree(fta, FTA, 2)),
      cbind(metric = "Turnovers",          agree(tov, Turnovers, 2)),
      cbind(metric = "Travels",            agree(travels, Travels, 1)),
      cbind(metric = "Offensive fouls",    agree(off_fouls, Offensive.Fouls + Charge.Fouls, 2)),
      cbind(metric = "3PT shooting fouls drawn (3-shot trips + 3PT and-ones)",
            agree(trips_three_shot + and1_3pt, ThreePtShootingFoulsDrawn, 1)),
      cbind(metric = "And-ones (2PT + 3PT)", agree(trips_and1, X2pt.And.1.Free.Throw.Trips + X3pt.And.1.Free.Throw.Trips, 2)),
      cbind(metric = "Unassisted 2PM", agree(fg2m_unast, PtsUnassisted2s / 2, 2)),
      cbind(metric = "Unassisted 3PM", agree(fg3m_unast, PtsUnassisted3s / 3, 1)))
  }, era][order(era)]
}

# ---- 2. Year-to-year stability ----------------------------------------------------------------
# Correlation of each player's value in season t with season t+1 (both qualifying,
# regular season). A real stylistic trait should be sticky; pure noise would be ~0.
stability <- function() {
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))[qualified == TRUE & season_type == "RS"]
  ps[, yr := season_year(season)]
  cols <- c("ehi", "skill", "whistle", "smae", "difficulty", "selfcreate", "footwork",
            "excess_ftr", "threefoul", "and1jump", "ftshare")
  nxt <- ps[, c("personId", "yr", cols), with = FALSE][, yr := yr - 1]
  m <- merge(ps[, c("personId", "yr", cols), with = FALSE], nxt, by = c("personId", "yr"), suffixes = c("", "_next"))
  rbindlist(lapply(cols, function(cc) {
    ct <- cor.test(m[[cc]], m[[paste0(cc, "_next")]])
    data.table(measure = cc, r = round(ct$estimate, 3), ci_lo = round(ct$conf.int[1], 3),
               ci_hi = round(ct$conf.int[2], 3), pairs = nrow(m))
  }))
}

# ---- 3. Face validity: top / bottom 15 in selected seasons ------------------------------------
face_validity <- function(seasons = c("2000-01", "2010-11", "2018-19", "2025-26"), n = 15) {
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))[qualified == TRUE & season_type == "RS"]
  rbindlist(lapply(seasons, function(s) {
    d <- ps[season == s][order(-ehi)]
    rbind(d[1:n, .(season, end = "top", rank = 1:n, player, ehi = round(ehi, 1), archetype)],
          d[.N:(.N - n + 1), .(season, end = "bottom", rank = 1:n, player, ehi = round(ehi, 1), archetype)])
  }))
}

if (sys.nframe() == 0) {
  st <- stability(); print(st); fwrite(st, file.path(paths$final, "stability.csv"))
  fv <- face_validity(); fwrite(fv, file.path(paths$final, "face_validity.csv"))
}
