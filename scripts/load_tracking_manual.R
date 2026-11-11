# scripts/load_tracking_manual.R
# Tier 2 tracking validation from API pages saved by hand in a browser.
#
#   1. Rscript scripts/load_tracking_manual.R --urls
#        writes scripts/tracking_urls.md: 39 stats.nba.com API links
#        (drives, pull-up shooting, very-tight-defense shooting; 2013-14..2025-26)
#   2. Open each link in your browser (where nba.com/stats works) and save the
#      JSON (Cmd+S) into data/raw/tracking/manual/  -- file names don't matter;
#      each response says which season / measure it is.
#   3. Rscript scripts/load_tracking_manual.R
#        parses whatever is there and writes data/final/tracking_validation.csv,
#        which the Validation page picks up automatically.

source("R/utils.R")
suppressPackageStartupMessages(library(arrow))
manual_dir <- file.path(paths$tracking, "manual")
dir.create(manual_dir, showWarnings = FALSE, recursive = TRUE)

BASE <- "https://stats.nba.com/stats"
COMMON <- "College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=Totals&PlayerExperience=&PlayerPosition=&SeasonSegment=&SeasonType=Regular%20Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="

tracking_urls <- function() {
  rbindlist(lapply(2013:2025, function(y) {
    s <- season_label(y)
    data.table(season = s, measure = c("Drives", "PullUpShot", "Very tight defense (0-2 ft)"),
               url = c(sprintf("%s/leaguedashptstats?%s&PlayerOrTeam=Player&PtMeasureType=Drives&Season=%s", BASE, COMMON, s),
                       sprintf("%s/leaguedashptstats?%s&PlayerOrTeam=Player&PtMeasureType=PullUpShot&Season=%s", BASE, COMMON, s),
                       sprintf("%s/leaguedashplayerptshot?%s&CloseDefDistRange=0-2%%20Feet%%20-%%20Very%%20Tight&DribbleRange=&GeneralRange=&ShotClockRange=&ShotDistRange=&TouchTimeRange=&Period=0&Season=%s", BASE, COMMON, s)))
  }))
}

# Parse one saved NBA stats JSON into a data.table tagged with season + measure
parse_saved <- function(f) {
  # Browsers save a JSON response either raw or wrapped in an HTML page (<pre>...</pre>);
  # take everything from the first "{" to the last "}" so both work.
  txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  a <- regexpr("{", txt, fixed = TRUE); b <- max(gregexpr("}", txt, fixed = TRUE)[[1]])
  if (a < 0 || b < a) return(NULL)
  txt <- substr(txt, a, b)
  txt <- gsub("&quot;", "\"", gsub("&amp;", "&", txt, fixed = TRUE), fixed = TRUE)
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j) || is.null(j$resultSets)) return(NULL)
  rs <- j$resultSets[[1]]
  d <- rbindlist(lapply(rs$rowSet, function(r) as.list(sapply(r, function(v) if (is.null(v)) NA else v))))
  setnames(d, unlist(rs$headers))
  p <- j$parameters
  measure <- if (!is.null(p$PtMeasureType)) p$PtMeasureType else if (!is.null(p$CloseDefDistRange)) "VeryTight" else NA
  d[, `:=`(season = p$Season, measure = measure)]
  d
}

# Correlation with 95% CI as a one-row table
cor_row <- function(label, x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 10) return(NULL)
  ct <- cor.test(x[ok], y[ok])
  data.table(check = label, r = round(ct$estimate, 3), ci_lo = round(ct$conf.int[1], 3),
             ci_hi = round(ct$conf.int[2], 3), n = sum(ok))
}

build_tracking_validation <- function() {
  files <- list.files(manual_dir, "\\.json$|\\.txt$|\\.html?$", full.names = TRUE)
  tr <- rbindlist(lapply(files, parse_saved), fill = TRUE)
  if (!nrow(tr)) stop("no saved tracking files found in ", manual_dir)
  message("parsed ", length(files), " files: ", paste(unique(paste(tr$season, tr$measure)), collapse = ", "))
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))[
    season_type == "RS" & qualified == TRUE & season_year(season) >= 2013]
  num <- function(x) suppressWarnings(as.numeric(x))
  out <- list()
  dr <- tr[measure == "Drives", .(personId = as.integer(PLAYER_ID), season, drive_fta = num(DRIVE_FTA))]
  if (nrow(dr)) {
    m <- merge(ps, dr, by = c("personId", "season"))
    m[, nondrive_fta_share := (fta - drive_fta) / fta]
    out[[1]] <- cor_row("Excess FT rate vs share of FTA NOT from drives", m$excess_ftr_raw, m$nondrive_fta_share)
  }
  pu <- tr[measure == "PullUpShot", .(personId = as.integer(PLAYER_ID), season, pu_efg = num(PULL_UP_EFG_PCT), pu_fga = num(PULL_UP_FGA))]
  if (nrow(pu)) {
    m <- merge(ps, pu[pu_fga >= 100], by = c("personId", "season"))
    out[[2]] <- cor_row("Shot-making above expected vs pull-up eFG% (100+ pull-ups)", m$smae_raw, m$pu_efg)
  }
  vt <- tr[measure == "VeryTight", .(personId = as.integer(PLAYER_ID), season, vt_efg = num(EFG_PCT), vt_fga = num(FGA))]
  if (nrow(vt)) {
    m <- merge(ps, vt[vt_fga >= 100], by = c("personId", "season"))
    out[[3]] <- cor_row("Shot-making above expected vs eFG% on very tightly contested shots (100+)", m$smae_raw, m$vt_efg)
  }
  res <- rbindlist(out)
  fwrite(res, file.path(paths$final, "tracking_validation.csv"))
  print(res)
}

if (sys.nframe() == 0) {
  if ("--urls" %in% commandArgs(trailingOnly = TRUE)) {
    u <- tracking_urls()
    writeLines(c("# Tracking data: links to save from your browser", "",
                 "Open each link where nba.com/stats works, then save the page (Cmd+S, format: Page Source / JSON)",
                 "into `data/raw/tracking/manual/`. Then run `Rscript scripts/load_tracking_manual.R`.", "",
                 sprintf("- %s · %s: <%s>", u$season, u$measure, u$url)), "scripts/tracking_urls.md")
    message("wrote scripts/tracking_urls.md (", nrow(u), " links)")
  } else build_tracking_validation()
}
