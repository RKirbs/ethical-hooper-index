# run_all.R
# Rebuilds the entire project, raw data to website, with one command:
#
#     Rscript run_all.R            # full rebuild (first run downloads ~330 MB + 180 web pages)
#     Rscript run_all.R --site     # only re-render README + site from data/final/
#
# Steps are idempotent: downloads are cached in data/raw/ and skipped when present.
# The xFG step (03) is the slow one (~25 min on an 8-core laptop).

args <- commandArgs(trailingOnly = TRUE)
site_only <- "--site" %in% args

run <- function(script) {
  message("\n==> ", script)
  t0 <- Sys.time()
  status <- system2("Rscript", script)
  if (status != 0) stop(script, " failed (exit ", status, ")")
  message("    done in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}

if (!site_only) {
  for (s in c("R/01_download.R", "R/02_clean_pbp.R", "R/02b_external_sources.R", "R/03_shots_xfg_model.R",
              "R/04_player_metrics.R", "R/05_index_build.R", "R/06_validation.R",
              "R/07_rule_change_did.R", "R/08_eras_anomalies.R")) run(s)
}
run("scripts/build_readme.R")

# Quarto CLI: use QUARTO_PATH if set, otherwise whatever `quarto` is on PATH
quarto <- Sys.getenv("QUARTO_PATH", unset = Sys.which("quarto"))
if (!nzchar(quarto)) stop("Quarto CLI not found: install from https://quarto.org or set QUARTO_PATH")
message("\n==> quarto render analysis/")
status <- system2(quarto, c("render", "analysis"))
if (status != 0) stop("quarto render failed")
message("\nSite written to docs/")
