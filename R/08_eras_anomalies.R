# 08_eras_anomalies.R
# Era highlights and programmatically detected anomalies (nothing hand-picked).
#   Eras:      top/bottom 10 per era with data-driven blurbs, era quadrant chart,
#              league trend lines with rule changes annotated, signature quadrant
#   Anomalies: regular-season -> playoff whistle change, biggest post-2021 drops,
#              archetype switchers, "ethical despite the volume", |z| > 3 outliers,
#              team effects (player + team fixed effects)
# Outputs: data/final/era_*.csv, data/final/anomaly_*.csv, figures/*.png

source("R/utils.R")
source("R/05_index_build.R")   # weights, component names
suppressPackageStartupMessages({ library(arrow); library(ggrepel); library(fixest); library(patchwork) })

load_index <- function() as.data.table(read_parquet(file.path(paths$interim, "player_index.parquet")))

# ---- Blurbs -------------------------------------------------------------------------------------
# Plain-English phrase for each component, by direction (good for EHI / bad for EHI)
PHRASES <- list(
  smae       = c(good = "shot-making well above expectation", bad = "shot-making below expectation"),
  selfcreate = c(good = "a high share of self-created makes",  bad = "mostly assisted scoring"),
  difficulty = c(good = "a diet of tough shots",                bad = "an easy shot diet"),
  footwork   = c(good = "rarely called for travels or charges", bad = "frequent travels and offensive fouls"),
  excess_ftr = c(good = "few free throws beyond what the shot diet predicts", bad = "free throws well beyond what the shot diet predicts"),
  threefoul  = c(good = "almost never fouled on threes",        bad = "frequent 3PT shooting fouls drawn"),
  ftshare    = c(good = "little scoring from the line",         bad = "heavy scoring from the line"),
  and1jump   = c(good = "few perimeter and-ones",               bad = "many perimeter and-ones"))

# Each component's signed contribution to EHI (positive = pushes EHI up)
contrib_weights <- function() {
  c(AXIS_W[["skill"]] * SKILL_W / sum(SKILL_W) * SKILL_SIGN[names(SKILL_W)],
    -AXIS_W[["whistle"]] * WHISTLE_W / sum(WHISTLE_W) * WHISTLE_SIGN[names(WHISTLE_W)])
}

# One-sentence blurb naming the two components that drove a rating
make_blurb <- function(zs, top = TRUE) {
  cw <- contrib_weights()
  contrib <- cw[names(zs)] * unlist(zs)
  pick <- names(sort(if (top) -contrib else contrib))[1:2]
  dir <- if (top) "good" else "bad"
  parts <- vapply(pick, function(p) sprintf("%s (%+.1f SD)", PHRASES[[p]][[dir]], zs[[p]]), "")
  paste0(toupper(substr(parts[1], 1, 1)), substr(parts[1], 2, nchar(parts[1])), " and ", parts[2], ".")
}

# ---- Eras ---------------------------------------------------------------------------------------
# Player-era rating: minutes-weighted mean over >= 2 qualifying RS seasons in that era
era_ratings <- function(ps, min_seasons = 2) {
  q <- ps[qualified == TRUE & season_type == "RS"]
  zc <- paste0("z_", COMPONENTS)
  r <- q[, c(list(player = player[.N], seasons = .N, ehi = wmean(ehi, mp), skill = wmean(skill, mp),
                  whistle = wmean(whistle, mp), mp = sum(mp)),
             lapply(.SD, function(z) wmean(z, mp))), .(personId, era), .SDcols = zc][seasons >= min_seasons]
  setnames(r, zc, COMPONENTS)
  r[, archetype := assign_archetype(skill, whistle)]
  r
}

era_top_bottom <- function(er, n = 10) {
  rbindlist(lapply(levels(er$era), function(e) {
    d <- er[era == e]
    top <- d[order(-ehi)][1:n][, `:=`(end = "Top", rank = .I)]
    bot <- d[order(ehi)][1:n][, `:=`(end = "Bottom", rank = .I)]
    out <- rbind(top, bot)
    out[, blurb := vapply(seq_len(.N), function(i) make_blurb(as.list(out[i, COMPONENTS, with = FALSE]), end[i] == "Top"), "")]
    out[, .(era, end, rank, player, seasons, ehi = round(ehi, 1), skill = round(skill, 2),
            whistle = round(whistle, 2), archetype, blurb)]
  }))
}

# Quadrant chart: Skill on x, Whistle on y INVERTED so "ethical" is up-and-right
quadrant_plot <- function(d, label_ids, title, subtitle, facet = FALSE) {
  pad <- function(r) r + c(-1, 1) * 0.06 * diff(r)
  xl <- pad(range(d$skill, na.rm = TRUE)); yl <- pad(range(d$whistle, na.rm = TRUE))
  p <- ggplot(d, aes(skill, whistle)) +
    annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0, fill = pal$blue, alpha = 0.06) +
    geom_hline(yintercept = 0, color = pal$axis) + geom_vline(xintercept = 0, color = pal$axis) +
    geom_point(aes(color = archetype, shape = archetype), size = 1.8, alpha = 0.75) +
    geom_text_repel(data = d[personId %in% label_ids], aes(label = player), size = 2.9, color = pal$ink,
                    min.segment.length = 0, segment.color = pal$muted, segment.size = 0.25,
                    max.overlaps = 30, seed = 1, box.padding = 0.3) +
    scale_y_reverse(limits = rev(yl)) + scale_x_continuous(limits = xl) +
    scale_color_manual(values = archetype_colors, drop = FALSE, name = NULL) +
    scale_shape_manual(values = archetype_shapes, drop = FALSE, name = NULL) +
    annotate("text", x = xl[2], y = yl[1], label = "ETHICAL HOOPERS", hjust = 1, vjust = 1, size = 3, color = pal$ink2, fontface = "bold") +
    annotate("text", x = xl[2], y = yl[2], label = "WHISTLE ARTISTS", hjust = 1, vjust = 0, size = 3, color = pal$ink2, fontface = "bold") +
    annotate("text", x = xl[1], y = yl[2], label = "DOWNHILL BULLIES", hjust = 0, vjust = 0, size = 3, color = pal$ink2, fontface = "bold") +
    annotate("text", x = xl[1], y = yl[1], label = "GRINDERS", hjust = 0, vjust = 1, size = 3, color = pal$ink2, fontface = "bold") +
    labs(title = title, subtitle = subtitle, x = "Skill Score (SD vs season peers) →",
         y = "← more whistle    Whistle Score (SD)    less whistle →", caption = CAPTION) +
    theme_ehi() + theme(legend.position = "none")
  if (facet) p <- p + facet_wrap(~era, ncol = 2)
  p
}

# Notable labels: extremes on each axis/EHI plus the highest-minute players
pick_labels <- function(d, n_ext = 6, n_star = 10) {
  unique(c(d[order(-ehi)][1:n_ext, personId], d[order(ehi)][1:n_ext, personId],
           d[order(-skill)][1:3, personId], d[order(-mp)][1:n_star, personId]))
}

# League trend small multiples (one measure per panel -- no dual axes)
trend_plot <- function(ps) {
  q <- ps[season_type == "RS"]
  lg <- q[, .(`Shooting-foul FTA per FGA` = sum(fta_shooting) / sum(fga),
              `3PT shooting fouls per 100 3PA` = 100 * sum(trips_three_shot + and1_3pt) / sum(fg3a + trips_three_shot),
              `Unassisted share of made FG` = sum(fg2m_unast + fg3m_unast) / sum(fgm),
              `Travels + offensive fouls per 100 attempts` = 100 * sum(travels + off_fouls) / sum(fga + 0.44 * fta + tov)),
          season][, yr := season_year(season)]
  fwrite(lg[order(yr)], file.path(paths$final, "league_trends.csv"))   # for inline numbers on the site
  long <- melt(lg, id.vars = c("season", "yr"))
  ggplot(long, aes(yr, value)) +
    geom_vline(xintercept = RULE_CHANGES$year, color = pal$muted, linetype = "dashed", linewidth = 0.4) +
    geom_line(color = pal$blue, linewidth = 0.7) + geom_point(color = pal$blue, size = 1.2) +
    facet_wrap(~variable, ncol = 2, scales = "free_y") +
    scale_x_continuous(breaks = c(1996, 2004, 2013, 2021), labels = c("1996-97", "2004-05", "2013-14", "2021-22")) +
    labs(title = "How the whistle changed, 1996-97 to 2025-26",
         subtitle = "League-wide regular-season rates. Dashed lines: 2004-05 hand-check removal and\n2021-22 'non-basketball move' crackdown",
         x = NULL, y = NULL, caption = CAPTION) +
    theme_ehi(11) + theme(panel.spacing = unit(1.2, "lines"))
}

# ---- Anomalies ----------------------------------------------------------------------------------

# Regular season -> playoffs whistle change: shooting-foul FTA per FGA, net of the
# league-wide RS->PO change that season, summed over a player's playoff runs
playoff_whistle <- function(ps, min_po_fga = 300, min_runs = 3) {
  rs <- ps[season_type == "RS" & qualified == TRUE, .(personId, season, player, rs_fta = fta_shooting, rs_fga = fga)]
  po <- ps[season_type == "PO", .(personId, season, po_fta = fta_shooting, po_fga = fga)]
  m <- merge(rs, po, by = c("personId", "season"))
  lg <- m[, .(lg_delta = sum(po_fta) / sum(po_fga) - sum(rs_fta) / sum(rs_fga)), season]
  m <- merge(m, lg, by = "season")
  out <- m[, .(player = player[.N], po_runs = .N, po_fga = sum(po_fga),
               rs_ftr = sum(rs_fta) / sum(rs_fga), po_ftr = sum(po_fta) / sum(po_fga),
               lg_delta = weighted.mean(lg_delta, po_fga)), personId][po_fga >= min_po_fga & po_runs >= min_runs]
  out[, net_change := (po_ftr - rs_ftr) - lg_delta]
  out[, pct_change := 100 * net_change / rs_ftr]
  out[order(net_change)]
}

# Biggest individual Whistle Score drop from 2018-21 to 2021-26
post_rule_drop <- function(ps) {
  q <- ps[season_type == "RS" & qualified == TRUE]
  q[, period := fifelse(season_year(season) %in% 2018:2020, "pre", fifelse(season_year(season) >= 2021, "post", NA_character_))]
  w <- q[!is.na(period), .(whistle = wmean(whistle, mp), threefoul = wmean(threefoul_raw, mp),
                           ftr = sum(fta_shooting) / sum(fga), n = .N, player = player[.N]), .(personId, period)]
  d <- dcast(w, personId ~ period, value.var = c("whistle", "threefoul", "ftr", "n"))
  d <- merge(d, w[, .(player = player[1]), personId], by = "personId")
  d <- d[!is.na(whistle_pre) & !is.na(whistle_post) & n_pre >= 2 & n_post >= 2]
  d[, whistle_change := whistle_post - whistle_pre]
  d[order(whistle_change)]
}

# Archetype switchers: >= 6 qualifying seasons; count quadrant changes season to
# season and the distance between first-3 and last-3 season positions
archetype_switchers <- function(ps) {
  q <- ps[season_type == "RS" & qualified == TRUE][order(personId, season)]
  q[, if (.N >= 6) .(
    player = player[.N], seasons = .N,
    switches = sum(archetype[-1] != archetype[-.N]),
    first = as.character(archetype[1]), last = as.character(archetype[.N]),
    shift = sqrt((mean(tail(skill, 3)) - mean(head(skill, 3)))^2 + (mean(tail(whistle, 3)) - mean(head(whistle, 3)))^2)),
    personId][order(-shift)]
}

# "Ethical despite the volume": top-decile usage proxy within season AND EHI >= 65
ethical_volume <- function(ps) {
  q <- ps[season_type == "RS" & qualified == TRUE]
  q[, usage_pct := frank(usage36) / .N, season]
  q[usage_pct >= 0.9 & ehi >= 65, .(season, player, ehi = round(ehi, 1), usage36 = round(usage36, 1),
                                     skill = round(skill, 2), whistle = round(whistle, 2))][order(-ehi)]
}

# |z| > 3 on any component, with the raw value and sample size for diagnosis
outliers <- function(ps) {
  q <- ps[season_type == "RS" & qualified == TRUE]
  rbindlist(lapply(COMPONENTS, function(cc) {
    q[abs(get(paste0("z_", cc))) > 3, .(season, player, component = cc, z = round(get(paste0("z_", cc)), 2),
                                        raw = round(get(paste0(cc, "_raw")), 4), shrunk = round(get(cc), 4),
                                        mp, fga)]
  }))[order(-abs(z))]
}

# Team effects: EHI ~ player FE + team FE (franchise), regular season qualifiers.
# The team FE says how a player's EHI moves when on that franchise vs elsewhere.
FRANCHISE <- c(SEA = "OKC", NJN = "BRK", VAN = "MEM", NOH = "NOP", NOK = "NOP", CHH = "NOP",
               CHA = "CHO", WSB = "WAS")
team_effects <- function(ps) {
  q <- ps[season_type == "RS" & qualified == TRUE & !is.na(primary_team)]
  q[, franchise := fifelse(primary_team %in% names(FRANCHISE), FRANCHISE[primary_team], primary_team)]
  m <- feols(ehi ~ 1 | personId + franchise, data = q, cluster = ~personId)
  fe <- fixef(m)$franchise
  data.table(franchise = names(fe), effect = as.numeric(fe) - mean(fe),
             player_seasons = as.integer(table(q$franchise)[names(fe)]))[order(-effect)]
}

if (sys.nframe() == 0) {
  ps <- load_index()
  er <- era_ratings(ps)
  tb <- era_top_bottom(er); fwrite(tb, file.path(paths$final, "era_top_bottom.csv"))
  fwrite(er[, .(personId, era, player, seasons, ehi = round(ehi, 1), skill = round(skill, 3),
                whistle = round(whistle, 3), archetype)], file.path(paths$final, "era_ratings.csv"))
  save_fig(quadrant_plot(er, unlist(lapply(levels(er$era), function(e) pick_labels(er[era == e], 4, 5))),
                         "Ethical hoopers in every era",
                         "Each dot is a player's average in that era (2+ qualifying seasons). Up-and-right = skill without the whistle",
                         facet = TRUE), "quadrant_eras", width = 10, height = 10)
  save_fig(trend_plot(ps), "league_trends", width = 9, height = 7)

  car <- fread(file.path(paths$final, "ehi_careers.csv"))
  car[, `:=`(archetype = factor(archetype, levels = ARCHETYPES), ehi = career_ehi)]
  save_fig(quadrant_plot(car, pick_labels(car, 8, 14), "The Ethical Hooper map, 1996-97 to 2025-26",
                         sprintf("%d careers (4+ qualifying seasons). Up-and-right: skill-driven scoring without leaning on the whistle", nrow(car))),
           "quadrant_signature", width = 9, height = 8.5)

  fwrite(playoff_whistle(ps), file.path(paths$final, "anomaly_playoff_whistle.csv"))
  fwrite(post_rule_drop(ps), file.path(paths$final, "anomaly_post_rule_drop.csv"))
  fwrite(archetype_switchers(ps), file.path(paths$final, "anomaly_switchers.csv"))
  fwrite(ethical_volume(ps), file.path(paths$final, "anomaly_ethical_volume.csv"))
  fwrite(outliers(ps), file.path(paths$final, "anomaly_outliers.csv"))
  fwrite(team_effects(ps), file.path(paths$final, "anomaly_team_effects.csv"))
  message("era + anomaly outputs written")
}
