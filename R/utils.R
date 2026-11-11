# utils.R
# Shared paths, helpers, palette and ggplot2 theme for the Ethical Hooper Index.
# Every other script sources this file first so that paths, season labels and
# chart styling are defined exactly once.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

# ---- Paths ------------------------------------------------------------------
# All paths are relative to the project root (scripts are run from the root).
paths <- list(
  raw      = "data/raw",
  bulk     = "data/raw/nba_data",       # shufinskiy/nba_data CSV archives
  pbpstats = "data/raw/pbpstats",       # pbpstats.com JSON cache
  bbref    = "data/raw/bbref",          # Basketball Reference HTML cache
  tracking = "data/raw/tracking",       # NBA tracking dashboards (Tier 2)
  interim  = "data/interim",            # parquet intermediates (gitignored)
  final    = "data/final",              # small committed outputs
  figures  = "figures"
)
invisible(lapply(paths, dir.create, showWarnings = FALSE, recursive = TRUE))

# ---- Season helpers -----------------------------------------------------------
SEASONS <- 1996:2025   # season start years: 1996 = 1996-97 ... 2025 = 2025-26

# Start year -> "1996-97" label (the NBA's own season string format)
season_label <- function(y) sprintf("%d-%02d", y, (y + 1) %% 100)

# "1996-97" label -> start year 1996
season_year <- function(lbl) as.integer(substr(lbl, 1, 4))

# Era assignment from the project spec (section 10). Returns an ordered factor.
ERA_LEVELS <- c("Hand-Check Era", "Freedom-of-Movement Era",
                "Pace-and-Space Era", "Post-Crackdown Era")
era_of <- function(y) {
  factor(fcase(y <= 2003, ERA_LEVELS[1],
               y <= 2012, ERA_LEVELS[2],
               y <= 2020, ERA_LEVELS[3],
               default = ERA_LEVELS[4]),
         levels = ERA_LEVELS, ordered = TRUE)
}

# ---- Polite HTTP with on-disk caching -------------------------------------------
# Download `url` to `dest` once; later calls reuse the cached file. Sleeps
# `pause` seconds after each real request and retries with exponential backoff,
# so we never hammer a free community API or Basketball Reference.
cached_get <- function(url, dest, pause = 3, tries = 4, timeout = 60) {
  if (file.exists(dest) && file.size(dest) > 0) return(invisible(dest))
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  for (i in seq_len(tries)) {
    ok <- tryCatch({
      h <- curl::new_handle(timeout = timeout, useragent = "ethical-hooper-index research project (R)")
      curl::curl_download(url, dest, handle = h, quiet = TRUE)
      TRUE
    }, error = function(e) { message("  attempt ", i, " failed: ", conditionMessage(e)); FALSE })
    Sys.sleep(pause)
    if (ok && file.size(dest) > 0) return(invisible(dest))
    if (file.exists(dest)) unlink(dest)
    Sys.sleep(pause * 2^i)
  }
  stop("Failed to download after ", tries, " tries: ", url)
}

# ---- Statistics helpers ---------------------------------------------------------
# z-score within groups (used for within-season standardisation)
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Weighted mean ignoring NAs
wmean <- function(x, w) { ok <- !is.na(x) & !is.na(w); sum(x[ok] * w[ok]) / sum(w[ok]) }

# ---- Palette ----------------------------------------------------------------------
# Validated reference palette (dataviz skill, light mode). Categorical slots are
# used in fixed order; scatter plots use at most 3 coloured classes + neutral gray,
# because only the first three slots stay distinguishable for colour-blind
# readers when every pair can sit next to every other pair.
pal <- list(
  surface   = "#fcfcfb",
  ink       = "#0b0b0b",
  ink2      = "#52514e",
  muted     = "#898781",
  grid      = "#e1e0d9",
  axis      = "#c3c2b7",
  neutral   = "#f0efec",
  blue      = "#2a78d6",   # slot 1
  orange    = "#eb6834",   # slot 2
  aqua      = "#1baf7a",   # slot 3
  yellow    = "#eda100",   # slot 4
  magenta   = "#e87ba4",   # slot 5
  green     = "#008300",   # slot 6
  violet    = "#6250d6",   # slot 7
  red       = "#e34948",   # slot 8
  gray_cat  = "#a3a29c"    # de-emphasised category
)
pal_cat <- unname(unlist(pal[c("blue", "orange", "aqua", "yellow", "magenta", "green", "violet", "red")]))

# Archetype colours + shapes (shape = secondary encoding so colour is never alone)
ARCHETYPES <- c("Ethical Hooper", "Whistle Artist", "Downhill Bully", "Grinder")
archetype_colors <- setNames(c(pal$blue, pal$orange, pal$aqua, pal$gray_cat), ARCHETYPES)
archetype_shapes <- setNames(c(16, 17, 15, 1), ARCHETYPES)

# Diverging scale for EHI-style values: red (whistle) <- gray -> blue (ethical)
scale_fill_ehi <- function(...) scale_fill_gradient2(low = pal$red, mid = pal$neutral, high = pal$blue, ...)
scale_color_ehi <- function(...) scale_color_gradient2(low = pal$red, mid = pal$neutral, high = pal$blue, ...)

# Era boundary / rule-change annotations used on every time-series chart
ERA_BREAKS <- c(2004, 2013, 2021)           # first season of eras 2-4
RULE_CHANGES <- data.table(year = c(2004, 2021),
                           label = c("2004-05: hand-check removed",
                                     "2021-22: non-basketball moves"))

# ---- Theme ---------------------------------------------------------------------------
# One minimal theme for every figure: recessive grid and axes, left-aligned
# title/subtitle (the subtitle states the takeaway), readable at phone width.
theme_ehi <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "") %+replace%
    theme(
      plot.background   = element_rect(fill = pal$surface, color = NA),
      panel.background  = element_rect(fill = pal$surface, color = NA),
      panel.grid.major  = element_line(color = pal$grid, linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      axis.line.x       = element_line(color = pal$axis, linewidth = 0.4),
      axis.ticks        = element_blank(),
      axis.text         = element_text(color = pal$ink2, size = rel(0.85)),
      axis.title        = element_text(color = pal$ink2, size = rel(0.9)),
      plot.title        = element_text(color = pal$ink, face = "bold", size = rel(1.25),
                                       hjust = 0, margin = margin(b = 4)),
      plot.subtitle     = element_text(color = pal$ink2, size = rel(0.95), hjust = 0,
                                       lineheight = 1.15, margin = margin(b = 10)),
      plot.caption      = element_text(color = pal$muted, size = rel(0.75), hjust = 0,
                                       margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_text(color = pal$ink2, size = rel(0.85)),
      legend.text       = element_text(color = pal$ink2, size = rel(0.85)),
      strip.text        = element_text(color = pal$ink, face = "bold", hjust = 0),
      plot.margin       = margin(14, 16, 10, 14)
    )
}

CAPTION <- "Data: NBA Stats play-by-play & shot charts (via shufinskiy/nba_data), pbpstats.com, Basketball Reference | Analysis: Rob Kirby"

# Save a figure to figures/ as PNG at a phone-friendly default size
save_fig <- function(plot, name, width = 8, height = 5.5, dpi = 200) {
  f <- file.path(paths$figures, paste0(name, ".png"))
  ggsave(f, plot, width = width, height = height, dpi = dpi, device = ragg::agg_png, bg = pal$surface)
  invisible(f)
}
