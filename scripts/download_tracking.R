# scripts/download_tracking.R
# Tier 2 (validation-only) NBA player-tracking data, 2013-14 -> 2025-26, regular season.
#
# WHY THIS IS A SEPARATE SCRIPT: stats.nba.com times out from the author's home
# network (see NOTES.md, Phase 0). Run this from any network where nba.com/stats
# loads in a browser (campus Wi-Fi, phone hotspot):
#
#     Rscript scripts/download_tracking.R
#
# It writes one small CSV per season x measure to data/raw/tracking/ and skips
# anything already downloaded, so it can be stopped and resumed. ~91 requests,
# ~5-10 minutes with polite 3-second pauses.

source("R/utils.R")
suppressPackageStartupMessages(library(hoopR))

TRACK_SEASONS <- 2013:2025
DEF_RANGES <- c("0-2 Feet - Very Tight", "2-4 Feet - Tight", "4-6 Feet - Open", "6+ Feet - Wide Open")

# Save the first table hoopR returns to `dest`, unless it already exists.
fetch <- function(dest, f, ...) {
  if (file.exists(dest)) return(invisible(TRUE))
  for (i in 1:3) {
    r <- tryCatch(f(...), error = function(e) NULL)
    Sys.sleep(3)
    if (length(r) && nrow(r[[1]]) > 0) { fwrite(r[[1]], dest); message("ok   ", basename(dest)); return(invisible(TRUE)) }
    Sys.sleep(5 * i)
  }
  message("FAIL ", basename(dest)); invisible(FALSE)
}

for (y in TRACK_SEASONS) {
  s <- season_label(y)
  for (m in c("Drives", "PullUpShot", "CatchShoot")) {
    fetch(file.path(paths$tracking, sprintf("pt_%s_%s.csv", tolower(m), s)), nba_leaguedashptstats,
          season = s, season_type = "Regular Season", pt_measure_type = m,
          player_or_team = "Player", per_mode = "Totals")
  }
  for (d in DEF_RANGES) {
    fetch(file.path(paths$tracking, sprintf("closedef_%s_%s.csv", gsub("[^0-9a-z]+", "", tolower(substr(d, 1, 8))), s)),
          nba_leaguedashplayerptshot, season = s, season_type = "Regular Season",
          close_def_dist_range = d, per_mode = "Totals")
  }
}
message("tracking files: ", length(list.files(paths$tracking, "\\.csv$")), " / ", length(TRACK_SEASONS) * 7)
