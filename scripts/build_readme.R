# scripts/build_readme.R
# Writes README.md from data/final/, so every number in the README is computed
# from the same outputs as the website (no hand-typed results).

suppressPackageStartupMessages(library(data.table))
fin <- function(x) fread(file = file.path("data", "final", x), encoding = "UTF-8")

REPO_URL <- "https://github.com/RKirbs/ethical-hooper-index"
SITE_URL <- "https://rkirbs.github.io/ethical-hooper-index/"

careers <- fin("ehi_careers.csv")[order(rank)]
rs <- fin("ehi_player_seasons.csv")[season_type == "RS"]
stab <- fin("stability.csv"); did <- fin("did_results.csv"); tr <- fin("league_trends.csv"); xg <- fin("xfg_eval.csv")
w_ts <- did[spec == "Treatment = pre EHI Whistle Score" & outcome == "True shooting %"]
pl_ts <- did[spec == "Placebo: fake rule in 2019-20" & outcome == "True shooting %"]
f1 <- function(x) formatC(x, format = "f", digits = 1); f2 <- function(x) formatC(x, format = "f", digits = 2)
f0 <- function(x) formatC(x, format = "d", big.mark = ",")
top5 <- paste(careers[1:5, player], collapse = ", ")
bot5 <- paste(careers[order(career_ehi)][1:5, player], collapse = ", ")
tf <- tr[season %in% c("2020-21", "2021-22")][[3]]

readme <- c(
"# The Ethical Hooper Index",
"",
"**Which NBA scorers, 1996-97 to 2025-26, generate offense through skill rather than the whistle?** A reproducible R + Quarto analysis of every shot, foul and free throw in thirty seasons of NBA play-by-play.",
"",
sprintf("**Live site:** %s", SITE_URL),
"",
"![The Ethical Hooper map: Skill Score vs Whistle Score for every qualifying career](figures/quadrant_signature.png)",
"",
"## What is an ethical hooper?",
"",
"Basketball slang for a player who scores and dominates through pure skill, clean footwork, natural shot-making and fluid movement, rather than trickery, flopping or foul-baiting: no rule manipulation, an aesthetic playstyle, and low free-throw reliance relative to scoring volume.",
"",
"Measured here as **high shot-making relative to shot difficulty, self-created offense, clean footwork, and low dependence on free throws not explained by an aggressive, rim-attacking shot diet.**",
"",
"> This is a **stylistic** metric built only from on-court data. It is not a moral judgment of any person; drawing fouls is legal and often smart basketball.",
"",
"## Key findings",
"",
sprintf("- **Most ethical careers:** %s. **Most whistle-dependent:** %s.", top5, bot5),
sprintf("- **A persistent style:** a player's EHI correlates r = %s with the next season's (%s pairs).", f2(stab[measure == "ehi", r]), f0(stab[measure == "ehi", pairs])),
sprintf("- **The 2021-22 \"non-basketball move\" crackdown** cut league three-point shooting fouls from %s to %s per 100 attempts in one season. Players with higher pre-rule Whistle Scores lost %s points of true shooting per SD afterward (p = %s); a placebo rule date shows no effect (p = %s).",
        f2(tf[1]), f2(tf[2]), f2(-100 * w_ts$estimate), formatC(w_ts$p, format = "f", digits = 3), formatC(pl_ts$p, format = "f", digits = 3)),
sprintf("- **%s qualifying player-seasons and %s careers** ranked; full tables in [`data/final/`](data/final/).", f0(nrow(rs)), f0(nrow(careers))),
"",
"## Method in brief",
"",
"1. **Data:** NBA.com play-by-play and shot charts 1996-97 to 2025-26 (regular season + playoffs) from the [`shufinskiy/nba_data`](https://github.com/shufinskiy/nba_data) archive; official minutes and positions from Basketball Reference; possessions from pbpstats.com.",
sprintf("2. **Expected FG model:** xgboost on shot location, distance, shot family, motion, clock and season (never shooter identity); held-out 2025-26 log-loss %s vs %s for a zone-average baseline; cross-fitted by game.",
        formatC(xg[model == "xgboost xFG", logloss], format = "f", digits = 3), formatC(xg[model == "Zone average (2024-25)", logloss], format = "f", digits = 3)),
"3. **Skill Score:** shot-making above expected, self-creation (unassisted makes), shot difficulty, clean footwork (travels + offensive fouls).",
"4. **Whistle Score:** excess free-throw rate (beyond what shot diet, usage and position predict), three-point shooting-foul rate, free-throw share of points, perimeter and-one rate.",
"5. **Empirical-Bayes shrinkage**, within-season z-scores, then **EHI = 0.4 × Skill − 0.6 × Whistle** on a 0–100 scale (50 = average qualifier).",
"6. **Validation:** cross-checks vs Basketball Reference and pbpstats, year-to-year stability, sensitivity to weights, and a difference-in-differences / event study of the 2021-22 rule change with placebo and regression-to-the-mean checks.",
"",
"Full details: the [Methodology](analysis/methodology.qmd) page and the running decision log in [`NOTES.md`](NOTES.md).",
"",
"## Reproduce it",
"",
"Requirements: R ≥ 4.5 and the [Quarto CLI](https://quarto.org) (≥ 1.11).",
"",
"```r",
"# from the repository root",
"install.packages(\"renv\")",
"renv::restore()          # installs the exact package versions in renv.lock",
"```",
"",
"```sh",
"Rscript run_all.R          # full rebuild: download, clean, model, index, site (~45 min)",
"Rscript run_all.R --site   # only re-render README + site from the committed data/final/",
"```",
"",
"Raw and intermediate data (`data/raw/`, `data/interim/`, ~4 GB) are not committed; `R/01_download.R` recreates them from the public sources, caching everything so nothing downloads twice.",
"",
"## Repository structure",
"",
"```",
"R/01_download.R            cached downloads (bulk PBP, pbpstats, Basketball Reference)",
"R/02_clean_pbp.R           shots, free-throw trips, player counting stats",
"R/02b_external_sources.R   official minutes/positions, ID map, player-season table",
"R/03_shots_xfg_model.R     expected-FG model + evaluation",
"R/04_player_metrics.R      the eight components + empirical-Bayes shrinkage",
"R/05_index_build.R         z-scores, composite, archetypes, careers, sensitivity",
"R/06_validation.R          cross-source checks, stability, face validity",
"R/07_rule_change_did.R     2021-22 rule change: DiD + event study",
"R/08_eras_anomalies.R      eras, anomalies, signature charts",
"R/utils.R                  paths, helpers, palette, ggplot theme",
"analysis/                  Quarto site source (rendered to docs/)",
"data/final/                small committed outputs (CSV)",
"data/audit/                data-audit and QA tables",
"figures/                   all charts (PNG)",
"scripts/                   README builder, optional tracking-data loader",
"NOTES.md                   decision log, data quirks, open questions",
"```",
"",
"## Limitations",
"",
"- No defender-distance data before 2013-14: xFG knows where and how a shot was taken, not how contested it was.",
"- Shooting fouls on missed twos can't be located on the court; jump-shot foul-baiting on twos shows up only through and-ones and the excess-FT residual.",
"- Officiating varies by crew and season; within-season standardization removes league-wide shifts only.",
"- NBA player-tracking validation (Tier 2) is pending: the NBA stats API blocks automated access.",
"- The index measures playing style, not character.",
"",
"## Data sources & credits",
"",
"- NBA.com play-by-play and shot detail via [shufinskiy/nba_data](https://github.com/shufinskiy/nba_data)",
"- [Basketball Reference](https://www.basketball-reference.com) (Sports Reference LLC): minutes, positions, cross-check tables",
"- [pbpstats.com](https://www.pbpstats.com): possessions and cross-check totals",
"- R packages: data.table, arrow, xgboost, fixest, ggplot2, ggrepel, gt, reactable, and more (see `renv.lock`)",
"",
"## Author",
"",
"**Rob Kirby**, Economics / Finance / Business Analytics. Questions and feedback welcome via GitHub issues.",
"",
"Licensed under the [MIT License](LICENSE)."
)
writeLines(readme, "README.md")
cat("README.md written\n")
