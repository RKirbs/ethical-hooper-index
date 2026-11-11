# 07_rule_change_did.R
# Natural experiment: the NBA's 2021-22 crackdown on "non-basketball moves"
# (abrupt sideways jumps into defenders, launching backward, arm hooks).
#
# Design: difference-in-differences / event study on regular-season player-seasons.
#   pre  = 2018-19 .. 2020-21     post = 2021-22 .. 2025-26
#   treatment intensity = each player's pre-period 3PT shooting-foul rate
#     (continuous, standardised) and a binary top-quartile version
#   two-way fixed effects (player + season), SEs clustered by player (fixest)
# Robustness: (a) placebo rule date 2019-20 using only pre-rule seasons,
#   (b) balanced panel (players present in both periods), (c) age^2 control
#   (linear age is collinear with player + season FE), (d) treatment measured
#   in a separate earlier window (2016-17..2017-18) so regression to the mean
#   cannot mechanically create the effect, (e) treatment = pre-period EHI
#   Whistle Score (does the index itself predict who lost the whistle?).
# Outputs: data/final/did_results.csv, data/final/event_study.csv, figures/event_study_*.png

source("R/utils.R")
suppressPackageStartupMessages({ library(arrow); library(fixest) })

OUTCOMES <- c(excess_ftr_raw = "Excess FT rate", threefoul_raw = "3PT shooting fouls per 100 3PA",
              ftshare_raw = "FT share of points", pts75 = "Points per 75 possessions", ts = "True shooting %")
PANEL_MIN_MP <- 500   # outcome sample: any season with 500+ min (not FGA-based, to avoid selecting on outcomes)

# Panel of player-seasons with outcomes, 2016-17 .. 2025-26
load_panel <- function() {
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))[season_type == "RS"]
  ps[, yr := season_year(season)]
  ps <- ps[yr >= 2016 & mp >= PANEL_MIN_MP]
  ps[, `:=`(pts75 = 75 * pts / off_poss, ts = pts / (2 * (fga + 0.44 * fta)), age2 = (age - 27)^2)]
  ps[, post := as.integer(yr >= 2021)]
  ps
}

# Pre-period treatment: pooled 3PT shooting-foul rate over a window, among
# players who were qualifying scorers in that window. Pooled counts are less
# noisy than averaging seasonal rates.
treatment_from <- function(ps_all, years) {
  w <- ps_all[season_year(season) %in% years & season_type == "RS"]
  hoopers <- unique(w[qualified == TRUE, personId])
  t <- w[personId %in% hoopers, .(
    pre_threefoul = 100 * sum(trips_three_shot + and1_3pt) / sum(fg3a + trips_three_shot),
    pre_whistle = wmean(whistle, mp), pre_3pa = sum(fg3a)), personId][pre_3pa >= 100]
  t[, treat_c := (pre_threefoul - mean(pre_threefoul)) / sd(pre_threefoul)]
  t[, treat_b := as.integer(pre_threefoul >= quantile(pre_threefoul, 0.75))]
  t[, treat_w := (pre_whistle - mean(pre_whistle)) / sd(pre_whistle)]
  t
}

# Two-way FE DiD for one outcome and one treatment variable
did <- function(d, y, tvar, controls = "") {
  f <- as.formula(sprintf("%s ~ %s:post %s | personId + season", y, tvar, controls))
  m <- feols(f, data = d, cluster = ~personId)
  ct <- coeftable(m)
  ct <- ct[grep(":post$", rownames(ct)), ]   # the treatment x post term, not any control
  data.table(outcome = OUTCOMES[[y]], estimate = ct[1], se = ct[2], p = ct[4],
             ci_lo = ct[1] - 1.96 * ct[2], ci_hi = ct[1] + 1.96 * ct[2], n = nobs(m),
             players = uniqueN(d[!is.na(get(y)), personId]))
}

# Event study: treatment x season interactions, 2020-21 as reference
event_study <- function(d, y, tvar) {
  m <- feols(as.formula(sprintf("%s ~ i(season, %s, ref = '2020-21') | personId + season", y, tvar)),
             data = d, cluster = ~personId)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")
  ct[, season := sub("season::(.*):.*", "\\1", term)]
  rbind(ct[, .(season, estimate = Estimate, se = `Std. Error`)], data.table(season = "2020-21", estimate = 0, se = 0))[
    , `:=`(outcome = OUTCOMES[[y]], ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se)][order(season)]
}

plot_event_study <- function(es, title, subtitle) {
  ggplot(es, aes(season, estimate, group = 1)) +
    geom_hline(yintercept = 0, color = pal$axis) +
    geom_vline(xintercept = "2021-22", color = pal$muted, linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = pal$blue, alpha = 0.15) +
    geom_line(color = pal$blue, linewidth = 0.7) +
    geom_point(color = pal$blue, size = 2.4) +
    annotate("text", x = "2021-22", y = Inf, label = " rule change", hjust = 0, vjust = 1.5, size = 3.2, color = pal$ink2) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = "Effect per 1 SD of pre-period\n3PT foul-drawing (vs 2020-21)", caption = CAPTION) +
    theme_ehi() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

run_did <- function() {
  ps_all <- as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))
  panel <- load_panel()
  tr_main  <- treatment_from(ps_all, 2018:2020)
  tr_early <- treatment_from(ps_all, 2016:2017)

  main <- merge(panel[yr >= 2018], tr_main, by = "personId")
  both <- main[, if (any(post == 1) && any(post == 0)) .SD, personId]
  early <- merge(panel[yr >= 2018], tr_early, by = "personId")
  # placebo: pretend the rule arrived in 2019-20, using only pre-rule seasons
  plac <- merge(panel[yr %in% 2016:2020], treatment_from(ps_all, 2016:2018), by = "personId")
  plac[, post := as.integer(yr >= 2019)]

  specs <- list(
    "Main: continuous treatment"       = list(d = main,  t = "treat_c", c = ""),
    "Main: binary top-quartile"        = list(d = main,  t = "treat_b", c = ""),
    "Balanced panel"                   = list(d = both,  t = "treat_c", c = ""),
    "Age control"                      = list(d = main,  t = "treat_c", c = "+ age2"),
    "Treatment from 2016-18 (no RTM)"  = list(d = early, t = "treat_c", c = ""),
    "Treatment = pre EHI Whistle Score" = list(d = main, t = "treat_w", c = ""),
    "Placebo: fake rule in 2019-20"    = list(d = plac,  t = "treat_c", c = ""))
  res <- rbindlist(lapply(names(specs), function(nm) {
    s <- specs[[nm]]
    rbindlist(lapply(names(OUTCOMES), function(y) did(s$d, y, s$t, s$c)))[, spec := nm]
  }))
  setcolorder(res, "spec")
  es <- rbindlist(lapply(names(OUTCOMES), function(y) event_study(main, y, "treat_c")))
  list(res = res, es = es, n_players = uniqueN(main$personId), n_treated = uniqueN(main[treat_b == 1, personId]))
}

if (sys.nframe() == 0) {
  r <- run_did()
  fwrite(r$res[, lapply(.SD, function(x) if (is.double(x)) signif(x, 4) else x)], file.path(paths$final, "did_results.csv"))
  fwrite(r$es, file.path(paths$final, "event_study.csv"))
  message("players: ", r$n_players, " | top-quartile treated: ", r$n_treated)
  print(r$res[, .(spec, outcome, estimate = signif(estimate, 3), se = signif(se, 3), p = signif(p, 3), n)])
  save_fig(plot_event_study(r$es[outcome == OUTCOMES[["threefoul_raw"]]],
                            "3PT foul-drawers drew fewer fouls on threes after 2021-22...",
                            "...but a placebo test shows much of this drop is regression to the mean.\nChange in 3PT shooting fouls per 100 3PA per 1 SD of 2018-21 foul-drawing"),
           "event_study_threefoul")
  save_fig(plot_event_study(r$es[outcome == OUTCOMES[["ts"]]][, `:=`(estimate = 100 * estimate, ci_lo = 100 * ci_lo, ci_hi = 100 * ci_hi)],
                            "Foul-drawers lost efficiency after the crackdown",
                            "Change in true shooting % (points) per 1 SD of 2018-21 3PT foul-drawing, vs 2020-21") +
             labs(y = "TS% points per 1 SD of pre-period\n3PT foul-drawing (vs 2020-21)"),
           "event_study_ts")
}
