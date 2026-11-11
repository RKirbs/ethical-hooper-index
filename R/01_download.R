# 01_download.R
# Downloads and caches every raw data source into data/raw/. Safe to re-run:
# anything already on disk is skipped, so nothing is ever downloaded twice.
#
# Sources (see NOTES.md, Phase 0, for why each was chosen):
#   1. shufinskiy/nba_data bulk archives: PlayByPlayV3 + shot detail, 1996-97..2025-26,
#      regular season + playoffs; plus the V2 2012-13 file used to patch 16 missing games.
#   2. pbpstats.com player season totals (minutes, possessions, usage, foul splits).
#   3. Basketball Reference league pages (positions; cross-check tables).
# Tier 2 tracking data lives in scripts/download_tracking.R (stats.nba.com is
# blocked from the author's network, so it is run separately).

source("R/utils.R")

# ---- 1. Bulk play-by-play + shot detail --------------------------------------------
# Fetch one .tar.xz archive from the bulk repo and extract its CSV.
get_bulk <- function(name) {
  csv <- file.path(paths$bulk, paste0(name, ".csv"))
  if (file.exists(csv)) return(invisible(csv))
  tarf <- file.path(paths$bulk, paste0(name, ".tar.xz"))
  cached_get(sprintf("https://github.com/shufinskiy/nba_data/raw/main/datasets/%s.tar.xz", name), tarf, pause = 1)
  utils::untar(tarf, exdir = paths$bulk)
  unlink(tarf)
  invisible(csv)
}

download_bulk <- function() {
  names <- c(outer(c("nbastatsv3_", "nbastatsv3_po_", "shotdetail_", "shotdetail_po_"), SEASONS, paste0))
  for (n in c(names, "nbastats_2012")) get_bulk(n)
  message("bulk archives: ", length(list.files(paths$bulk, "\\.csv$")), " CSVs present")
}

# ---- 2. pbpstats.com player totals ---------------------------------------------------
# One JSON per season x season type; ~230 player-level fields each.
download_pbpstats <- function() {
  for (y in SEASONS) for (st in c("Regular Season", "Playoffs")) {
    dest <- file.path(paths$pbpstats, sprintf("totals_%s_%s.json", season_label(y), if (st == "Playoffs") "PO" else "RS"))
    url  <- sprintf("https://api.pbpstats.com/get-totals/nba?Season=%s&SeasonType=%s&Type=Player",
                    season_label(y), utils::URLencode(st, reserved = TRUE))
    cached_get(url, dest, pause = 2)
  }
  message("pbpstats files: ", length(list.files(paths$pbpstats)))
}

# ---- 3. Basketball Reference league pages ----------------------------------------------
# Season totals (position), play-by-play (fouls drawn, and-ones) and shooting
# (% assisted) pages, regular season. BBRef asks for <= 20 requests/minute, so we
# pause 4 s between requests (15/minute).
download_bbref <- function(pages = c("totals", "play-by-play", "shooting")) {
  for (y in SEASONS) for (pg in pages) {
    dest <- file.path(paths$bbref, sprintf("NBA_%d_%s.html", y + 1, pg))
    cached_get(sprintf("https://www.basketball-reference.com/leagues/NBA_%d_%s.html", y + 1, pg), dest, pause = 4)
  }
  # Playoff totals: official playoff minutes (pbpstats is incomplete before 2000-01)
  for (y in SEASONS) {
    dest <- file.path(paths$bbref, sprintf("NBA_%d_playoffs_totals.html", y + 1))
    cached_get(sprintf("https://www.basketball-reference.com/playoffs/NBA_%d_totals.html", y + 1), dest, pause = 4)
  }
  message("bbref files: ", length(list.files(paths$bbref)))
}

if (sys.nframe() == 0) {   # run everything when called via Rscript
  download_bulk()
  download_pbpstats()
  download_bbref()
}
