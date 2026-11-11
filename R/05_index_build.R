# 05_index_build.R
# Builds the Ethical Hooper Index from the shrunk components in player_metrics:
#   1. z-score every component within season x season type (qualifiers only),
#      which neutralises era-wide shifts (hand-check removal, 3PT boom, whistle trends)
#   2. Skill = weighted mean of Axis A z's; Whistle = weighted mean of Axis B z's
#   3. EHI = Skill - Whistle, re-standardised within season and put on a
#      0-100 scale (50 = average qualifier, 15 points = 1 SD)
#   4. Archetype quadrants, career and peak ratings, sensitivity analysis
# Outputs (small, committed): data/final/ehi_player_seasons.csv,
#   data/final/ehi_careers.csv, data/final/sensitivity_*.csv, figures/qualification_hist.png

source("R/utils.R")
suppressPackageStartupMessages(library(arrow))

# ---- Weights (justification in methodology.qmd / NOTES.md) -----------------------------
# Sign: +1 means "higher raw value = more of this axis".
SKILL_W <- c(smae = 0.45, selfcreate = 0.25, difficulty = 0.15, footwork = 0.15)
SKILL_SIGN <- c(smae = 1, selfcreate = 1, difficulty = 1, footwork = -1)
WHISTLE_W <- c(excess_ftr = 0.40, threefoul = 0.25, ftshare = 0.20, and1jump = 0.15)
# Axis weights in the final index. Rob's definition: an ethical hooper "doesn't have
# to be the most skillful", the core is NOT living at the line, so whistle-avoidance
# outweighs skill (decision logged in NOTES.md, Phase 6).
AXIS_W <- c(skill = 0.4, whistle = 0.6)
WHISTLE_SIGN <- c(excess_ftr = 1, threefoul = 1, and1jump = 1, ftshare = 1)
COMPONENTS <- c(names(SKILL_W), names(WHISTLE_W))

# z-score within season x type using qualifiers' mean/sd, applied to everyone
z_within <- function(dt, cols) {
  for (cc in cols) {
    st <- dt[qualified == TRUE, .(m = mean(get(cc), na.rm = TRUE), s = sd(get(cc), na.rm = TRUE)), group]
    # capped at +/-4 SD so one extreme component cannot dominate a composite
    dt[st, on = "group", paste0("z_", cc) := pmin(pmax((get(cc) - i.m) / i.s, -4), 4)]
  }
  dt
}

# Composite for one weighting scheme; returns EHI on the 0-100 scale
compute_ehi <- function(dt, sw = SKILL_W, ww = WHISTLE_W) {
  zs <- as.matrix(dt[, paste0("z_", names(sw)), with = FALSE]) %*% (SKILL_SIGN[names(sw)] * sw) / sum(sw)
  zw <- as.matrix(dt[, paste0("z_", names(ww)), with = FALSE]) %*% (WHISTLE_SIGN[names(ww)] * ww) / sum(ww)
  tmp <- data.table(group = dt$group, q = dt$qualified, s = as.numeric(zs), w = as.numeric(zw))
  # standardise each axis within season first, so the 0.4 / 0.6 axis weights are
  # real weights rather than artefacts of how correlated each axis's components are
  st <- tmp[q == TRUE, .(ms = mean(s), ss = sd(s), mw = mean(w), sw = sd(w)), group]
  tmp[st, on = "group", `:=`(skill = (s - i.ms) / i.ss, whistle = (w - i.mw) / i.sw)]
  tmp[, raw := AXIS_W[["skill"]] * skill - AXIS_W[["whistle"]] * whistle]
  st2 <- tmp[q == TRUE, .(m = mean(raw), sd = sd(raw)), group]
  tmp[st2, on = "group", ehi := pmin(pmax(50 + 15 * (raw - i.m) / i.sd, 0), 100)]
  tmp[, .(ehi, skill, whistle)]
}

assign_archetype <- function(skill, whistle) {
  factor(fcase(skill >= 0 & whistle < 0, "Ethical Hooper",
               skill >= 0 & whistle >= 0, "Whistle Artist",
               skill < 0 & whistle >= 0, "Downhill Bully",
               default = "Grinder"), levels = ARCHETYPES)
}

# Career table: >= 4 qualifying RS seasons; minutes-weighted EHI; peak = best
# minutes-weighted 3-consecutive-qualifying-season stretch
build_careers <- function(ps, ehi_col = "ehi", min_seasons = 4) {
  q <- ps[qualified == TRUE & season_type == "RS"]
  setorder(q, personId, season)
  q[, .(
    player = player[.N], seasons = .N,
    first = season[1], last = season[.N],
    career_ehi = wmean(get(ehi_col), mp),
    peak3_ehi = if (.N >= 3) max(vapply(1:(.N - 2), function(i) wmean(get(ehi_col)[i:(i + 2)], mp[i:(i + 2)]), 0)) else NA_real_,
    skill = wmean(skill, mp), whistle = wmean(whistle, mp),
    mp = sum(mp), fga = sum(fga)), personId][seasons >= min_seasons]
}

# ---- Sensitivity analysis ------------------------------------------------------------------
# Re-run the composite under alternative weights and compare career rankings
# with the baseline: Spearman rho and overlap of the top / bottom 25.
pca_weights <- function(dt, cols, sign) {
  X <- as.matrix(dt[qualified == TRUE & season_type == "RS", paste0("z_", cols), with = FALSE])
  X <- sweep(X[complete.cases(X), ], 2, sign[cols], `*`)
  l <- prcomp(X, scale. = TRUE)$rotation[, 1]
  l <- l * sign(sum(l))
  setNames(pmax(l, 0.02) / sum(pmax(l, 0.02)), cols)
}

sensitivity <- function(ps, base_careers) {
  schemes <- list(
    "Equal weights" = list(sw = setNames(rep(1, 4), names(SKILL_W)), ww = setNames(rep(1, 4), names(WHISTLE_W))),
    "PCA weights"   = list(sw = pca_weights(ps, names(SKILL_W), SKILL_SIGN), ww = pca_weights(ps, names(WHISTLE_W), WHISTLE_SIGN)))
  for (cc in names(SKILL_W))   schemes[[paste("Drop", cc)]] <- list(sw = SKILL_W[names(SKILL_W) != cc], ww = WHISTLE_W)
  for (cc in names(WHISTLE_W)) schemes[[paste("Drop", cc)]] <- list(sw = SKILL_W, ww = WHISTLE_W[names(WHISTLE_W) != cc])

  top_ids <- function(c, n = 25, top = TRUE) c[order(if (top) -career_ehi else career_ehi)][1:n, personId]
  res <- rbindlist(lapply(names(schemes), function(nm) {
    alt <- copy(ps)
    alt[, c("ehi", "skill", "whistle") := compute_ehi(alt, schemes[[nm]]$sw, schemes[[nm]]$ww)]
    ac <- build_careers(alt)
    m <- merge(base_careers[, .(personId, b = career_ehi)], ac[, .(personId, a = career_ehi)], by = "personId")
    sm <- merge(ps[qualified == TRUE & season_type == "RS", .(personId, season, b = ehi)],
                alt[qualified == TRUE & season_type == "RS", .(personId, season, a = ehi)], by = c("personId", "season"))
    data.table(scheme = nm,
               spearman_careers = round(cor(m$a, m$b, method = "spearman"), 3),
               spearman_seasons = round(cor(sm$a, sm$b, method = "spearman"), 3),
               top25_overlap_pct = 4 * length(intersect(top_ids(base_careers), top_ids(ac))),
               bottom25_overlap_pct = 4 * length(intersect(top_ids(base_careers, top = FALSE), top_ids(ac, top = FALSE))),
               weights = paste(c(sprintf("%s=%.2f", names(schemes[[nm]]$sw), schemes[[nm]]$sw / sum(schemes[[nm]]$sw)),
                                 sprintf("%s=%.2f", names(schemes[[nm]]$ww), schemes[[nm]]$ww / sum(schemes[[nm]]$ww))), collapse = " "))
  }))
  res
}

# ---- Qualification histogram (for the Phase 4 checkpoint and methodology) -------------------------
plot_qualification <- function(ps) {
  d <- ps[season_type == "RS" & mp >= min_minutes(season, "RS")]
  ggplot(d, aes(fga36)) +
    geom_histogram(binwidth = 0.5, boundary = 0, fill = pal$blue, color = pal$surface, linewidth = 0.3) +
    geom_vline(xintercept = 12, color = pal$ink, linewidth = 0.5, linetype = "dashed") +
    annotate("text", x = 12.3, y = Inf, vjust = 1.5, hjust = 0, size = 3.4, color = pal$ink2,
             label = sprintf("12 FGA/36 cutoff: %s of %s seasons qualify",
                             format(d[fga36 >= 12, .N], big.mark = ","), format(nrow(d), big.mark = ","))) +
    labs(title = "Who counts as a 'hooper'?",
         subtitle = "Field-goal attempts per 36 minutes, regular-season player-seasons\nwith 1,000+ minutes (prorated in shortened seasons)",
         x = "FGA per 36 minutes", y = "Player-seasons", caption = CAPTION) +
    theme_ehi()
}

build_index <- function() {
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_metrics.parquet")))
  ps <- z_within(ps, COMPONENTS)
  ps[, c("ehi", "skill", "whistle") := compute_ehi(ps)]
  ps[, archetype := assign_archetype(skill, whistle)]
  careers <- build_careers(ps)
  careers[, archetype := assign_archetype(skill, whistle)]
  careers[, rank := frank(-career_ehi, ties.method = "min")]
  list(ps = ps, careers = careers[order(rank)])
}

if (sys.nframe() == 0) {
  r <- build_index()
  ps <- r$ps; careers <- r$careers
  write_parquet(ps, file.path(paths$interim, "player_index.parquet"))
  keep <- c("personId", "player", "season", "season_type", "era", "primary_team", "pos", "age", "g", "mp", "fga",
            "qualified", "ehi", "skill", "whistle", "archetype",
            paste0(COMPONENTS, "_raw"), COMPONENTS, paste0("z_", COMPONENTS))
  out <- ps[qualified == TRUE, ..keep]
  num <- names(out)[vapply(out, is.double, TRUE)]
  out[, (num) := lapply(.SD, round, 4), .SDcols = num]
  fwrite(out[order(season_type, season, -ehi)], file.path(paths$final, "ehi_player_seasons.csv"))
  fwrite(careers[, lapply(.SD, function(x) if (is.double(x)) round(x, 3) else x)], file.path(paths$final, "ehi_careers.csv"))
  save_fig(plot_qualification(ps), "qualification_hist")
  sens <- sensitivity(ps, careers)
  fwrite(sens, file.path(paths$final, "sensitivity.csv"))
  print(sens[, !"weights"])
}
