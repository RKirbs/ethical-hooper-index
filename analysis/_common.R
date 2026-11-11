# _common.R: shared setup for every page of the site.
# Pages only read the small committed outputs in data/final/ and the figures,
# so the site renders from a fresh clone without the raw data.
suppressPackageStartupMessages({
  library(data.table); library(gt); library(reactable); library(htmltools)
})
# repo root = the folder holding the .here marker (here::here() would stop at
# analysis/ because it treats _quarto.yml as a project root)
root <- rprojroot::find_root(rprojroot::has_file(".here"))

# Public repository (set once the GitHub repo exists; used for links on every page)
REPO_URL <- "https://github.com/RKirbs/ethical-hooper-index"
fin <- function(x) fread(file = file.path(root, "data", "final", x), encoding = "UTF-8")

careers  <- fin("ehi_careers.csv")[order(rank)]
seasons  <- fin("ehi_player_seasons.csv")
rs       <- seasons[season_type == "RS"]
po       <- seasons[season_type == "PO"]
trends   <- fin("league_trends.csv")
did      <- fin("did_results.csv")
es       <- fin("event_study.csv")
stab     <- fin("stability.csv")
sens     <- fin("sensitivity.csv")
xfg_eval <- fin("xfg_eval.csv")

# ---- formatting helpers (all numbers in prose are computed, never typed) ----
f0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
f1 <- function(x) formatC(x, format = "f", digits = 1)
f2 <- function(x) formatC(x, format = "f", digits = 2)
f3 <- function(x) formatC(x, format = "f", digits = 3)
pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%")
sgn <- function(x, d = 2) formatC(x, format = "f", digits = d, flag = "+")
pval <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", formatC(p, format = "f", digits = 3))
fig <- function(name) file.path("figures", paste0(name, ".png"))

# Index weights, read from the pipeline so the site can never drift from the code
src_env <- new.env()
local({
  lines <- readLines(file.path(root, "R", "05_index_build.R"))
  keep <- grep("^(SKILL_W|SKILL_SIGN|WHISTLE_W|WHISTLE_SIGN|AXIS_W) <-", lines, value = TRUE)
  eval(parse(text = keep), envir = src_env)
})
SKILL_W <- src_env$SKILL_W; WHISTLE_W <- src_env$WHISTLE_W; AXIS_W <- src_env$AXIS_W

COMP_LABELS <- c(
  smae = "Shot-making above expected", selfcreate = "Self-creation",
  difficulty = "Shot difficulty", footwork = "Clean footwork (travels + offensive fouls, reversed)",
  excess_ftr = "Excess free-throw rate", threefoul = "3PT shooting-foul rate",
  ftshare = "Free-throw share of points", and1jump = "Perimeter and-one rate")

# Shared reactable styling
rt_theme <- reactableTheme(
  borderColor = "#e1e0d9", stripedColor = "#f6f6f3", highlightColor = "#eef4fc",
  headerStyle = list(fontWeight = 700, color = "#52514e", borderBottom = "2px solid #c3c2b7"),
  searchInputStyle = list(width = "100%"))

ehi_color <- function(value) {
  # diverging: red (whistle) <- neutral -> blue (ethical); text stays ink-coloured
  v <- pmin(pmax((value - 50) / 30, -1), 1)
  if (is.na(v)) return(NULL)
  col <- if (v >= 0) grDevices::colorRamp(c("#f0efec", "#86b6ef"))(v) else grDevices::colorRamp(c("#f0efec", "#f2a6a5"))(-v)
  list(background = grDevices::rgb(col[1], col[2], col[3], maxColorValue = 255), fontWeight = 600)
}
