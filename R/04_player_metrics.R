# 04_player_metrics.R
# Computes every Ethical Hooper Index component for each player x season x
# season type, applies empirical-Bayes shrinkage, and writes
# data/interim/player_metrics.parquet.
#
# Axis A (Skill):    SMAE, shot difficulty, self-creation, clean footwork
# Axis B (Whistle):  excess FT rate, 3PT shooting-foul rate, jumper and-one
#                    share, FT share of points
# Why shrinkage: a 1,000-minute season still has only ~40 three-shot fouls or
# ~60 and-ones at most, so raw rates for small samples produce fake extremes.
# Every rate is pulled toward its season mean in proportion to its noise.

source("R/utils.R")
suppressPackageStartupMessages({ library(arrow); library(fixest) })

# Qualification rules (min_minutes, QUAL_FGA36) live in R/utils.R

# ---- Empirical Bayes helpers -------------------------------------------------------------

# Beta-binomial EB for k successes out of n trials. The Beta prior is fit by
# method of moments on qualifying players in the same group (season x type);
# every player's rate is replaced by its posterior mean (k + a) / (n + a + b).
eb_beta <- function(k, n, group, qual) {
  dt <- data.table(k = k, n = n, g = group, q = qual)
  dt[, p := fifelse(n > 0, k / n, NA_real_)]
  pri <- dt[q & n > 0, {
    m <- weighted.mean(p, n)
    # observed variance minus expected binomial noise = true-talent variance
    v_obs <- sum(n * (p - m)^2) / sum(n)
    v_noise <- m * (1 - m) * mean(1 / n)
    v_true <- max(v_obs - v_noise, 1e-6 * m * (1 - m))
    ab <- m * (1 - m) / v_true - 1
    .(a = m * ab, b = (1 - m) * ab)
  }, g]
  dt[pri, on = "g", `:=`(a = i.a, b = i.b)]
  dt[, (k + a) / (n + a + b)]
}

# Normal-normal EB for a continuous per-player estimate x with sampling
# variance se2: shrink toward the group mean by tau2 / (tau2 + se2).
eb_normal <- function(x, se2, group, qual, w = 1 / se2) {
  dt <- data.table(x = x, se2 = se2, g = group, q = qual, w = w)
  pri <- dt[q & is.finite(x) & is.finite(se2), {
    m <- weighted.mean(x, w)
    tau2 <- max(var(x) - mean(se2), 0.05 * var(x))
    .(m = m, tau2 = tau2)
  }, g]
  dt[pri, on = "g", `:=`(m = i.m, tau2 = i.tau2)]
  dt[, fifelse(is.finite(x), m + tau2 / (tau2 + se2) * (x - m), m)]
}

# ---- Build components --------------------------------------------------------------------------
build_player_metrics <- function() {
  ps <- as.data.table(read_parquet(file.path(paths$interim, "player_season.parquet")))
  sx <- as.data.table(read_parquet(file.path(paths$interim, "shots_xfg.parquet")))

  # Shot-level aggregates: actual vs expected points per FGA (heaves already excluded)
  sx[, pts_fg := made * fifelse(is3, 3, 2)]
  sa <- sx[, .(n_xfg = .N, pts_fg = sum(pts_fg), xpts = sum(xpts),
               resid_var = var(pts_fg - xpts),           # per-shot noise for SMAE shrinkage
               x_efg = mean(xpts / 2), x_efg_var = var(xpts / 2)),
           .(personId, season, season_type)]
  ps <- merge(ps, sa, by = c("personId", "season", "season_type"), all.x = TRUE)

  ps <- ps[!is.na(mp) & mp > 0 & fga > 0]
  ps[, `:=`(
    group = paste(season, season_type),
    era = era_of(season_year(season)),
    fga36 = 36 * fga / mp,
    scoring_att = fga + 0.44 * fta + tov,
    usage36 = 36 * (fga + 0.44 * fta + tov) / mp,     # usage proxy (see NOTES.md)
    pos = fifelse(is.na(pos) | !pos %in% c("PG", "SG", "SF", "PF", "C"), "SF", pos),
    fta_nt = fta - fta_tech)]
  ps[, qualified := mp >= min_minutes(season, season_type) & fga36 >= QUAL_FGA36]

  # ---- Axis A: Skill -------------------------------------------------------------------------
  # 1. Shot-making above expected: (actual - expected FG points) per FGA
  ps[, smae_raw := (pts_fg - xpts) / n_xfg]
  ps[, smae := eb_normal(smae_raw, resid_var / n_xfg, group, qualified)]
  # 2. Shot difficulty: 1 - expected eFG of the attempts (higher = harder shots)
  ps[, difficulty_raw := 1 - x_efg]
  ps[, difficulty := eb_normal(difficulty_raw, x_efg_var / n_xfg, group, qualified)]
  # 3. Self-creation: share of made FG that were unassisted
  ps[, selfcreate_raw := (fg2m_unast + fg3m_unast) / fgm]
  ps[, selfcreate := eb_beta(fg2m_unast + fg3m_unast, fgm, group, qualified)]
  # 4. Clean footwork (negative input): travels + offensive fouls per 100 scoring attempts
  ps[, footwork_raw := 100 * (travels + off_fouls) / scoring_att]
  ps[, footwork := 100 * eb_beta(travels + off_fouls, round(scoring_att), group, qualified)]

  # ---- Axis B: Whistle -----------------------------------------------------------------------
  # 1. Excess FT rate: FTA/FGA minus the FT rate predicted by shot diet, usage and
  #    position. Fit per era on qualifiers (FGA-weighted) with season x type fixed effects.
  # Only free throws from SHOOTING fouls count (spec refinement, see NOTES.md):
  # intentional fouls (hack-a-big, end-of-game fouling of good FT shooters) are
  # the defence's strategy, not the shooter's whistle-seeking.
  ps[, `:=`(ftr_raw = fta_shooting / fga, rim_sh = fga_rim / fga, paint_sh = fga_paint / fga,
            mid_sh = fga_mid / fga, c3_sh = fga_c3 / fga, fg3_sh = fg3a / fga)]
  ps[, ftr_pred := NA_real_]
  for (e in levels(ps$era)) {
    fit <- feols(ftr_raw ~ rim_sh + paint_sh + mid_sh + c3_sh + fg3_sh + usage36 + i(pos, ref = "SF") | group,
                 data = ps[era == e & qualified == TRUE], weights = ~fga)
    ps[era == e, ftr_pred := predict(fit, newdata = ps[era == e])]
  }
  # FTA arrive mostly in pairs, so sampling variance of FTA/FGA ~ 2 * rate / FGA
  ps[, excess_ftr_raw := ftr_raw - ftr_pred]
  ps[, excess_ftr := eb_normal(excess_ftr_raw, 2 * pmax(ftr_raw, 0.05) / fga, group, qualified)]
  # 2. 3PT shooting-foul rate: (3-shot trips + 3PT and-ones) per 100 three-point attempts,
  #    where attempts include the fouled misses that are not logged as FGA
  ps[, threefoul_n := fg3a + trips_three_shot]
  ps[, threefoul_raw := fifelse(threefoul_n > 0, 100 * (trips_three_shot + and1_3pt) / threefoul_n, NA_real_)]
  ps[, threefoul := 100 * eb_beta(trips_three_shot + and1_3pt, threefoul_n, group, qualified)]
  # 3. Perimeter and-one rate: and-ones on mid-range + 3PT jumpers per 100 such
  #    attempts. (Spec deviation, see NOTES.md: the spec's "share of and-ones on
  #    jumpers" correlated 0.56 with jumper share of FGA, i.e. it measured shot
  #    diet, not foul-baiting. Restricting to perimeter zones also keeps lane
  #    floaters, coded as "jump shots", out of it.)
  perim_zones <- c("Mid-Range", "Left Corner 3", "Right Corner 3", "Above the Break 3")
  tr <- as.data.table(read_parquet(file.path(paths$interim, "ft_trips.parquet")))
  a1 <- tr[personId > 0, .(
    and1_perim   = sum(trip_type == "and1" & and1_zone %in% perim_zones),
    ftm_shooting = sum(fcoalesce(trip_ftm, 0L)[trip_type %in% c("and1", "three_shot", "two_shot_shooting")])),
    .(personId, season, season_type)]
  ps <- merge(ps, a1, by = c("personId", "season", "season_type"), all.x = TRUE)
  ps[is.na(and1_perim), `:=`(and1_perim = 0L, ftm_shooting = 0L)]
  ps[, perim_fga := fga_mid + fg3a]
  ps[, and1jump_raw := fifelse(perim_fga > 0, 100 * and1_perim / perim_fga, NA_real_)]
  ps[, and1jump := 100 * eb_beta(and1_perim, perim_fga, group, qualified)]
  # 4. FT share of points (supporting metric, low weight). Approximate binomial
  #    with n = scoring attempts so low-volume players are shrunk harder.
  ps[, ftshare_raw := ftm_shooting / pmax(pts, 1)]
  ps[, ftshare := eb_beta(round(ftshare_raw * scoring_att), round(scoring_att), group, qualified)]

  ps[]
}

if (sys.nframe() == 0) {
  pm <- build_player_metrics()
  write_parquet(pm, file.path(paths$interim, "player_metrics.parquet"))
  q <- pm[qualified == TRUE]
  message("qualified player-seasons: RS ", q[season_type == "RS", .N], ", PO ", q[season_type == "PO", .N])
  comps <- c("smae", "difficulty", "selfcreate", "footwork", "excess_ftr", "threefoul", "and1jump", "ftshare")
  print(round(cor(q[season_type == "RS", ..comps], use = "pairwise"), 2))
}
