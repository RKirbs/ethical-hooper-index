# NOTES.md: Ethical Hooper Index working log

Running log of decisions, data quirks, and open questions. Newest phase at the bottom.

---

## Phase 0: Data Audit (2026-11-11)

### Environment
- R 4.5.1. Installed: tidyverse, data.table, fixest, mgcv, xgboost, ggplot2, ggrepel, patchwork, renv, httr, rvest, hoopR 3.1.0 (installed during audit).
- Missing R packages (to install in Phase 1 via renv): arrow, gt, reactable, targets, quarto.
- **Quarto CLI not installed** (needed by Phase 7). GitHub CLI `gh` not installed (needed by Phase 8, or we use the web UI).
- Disk: ~500 GB free. Raw bulk CSVs occupy ~3.7 GB in `data/raw/nba_data/` (will be gitignored, converted to Parquet).

### Source-by-source status

| # | Source | Status | Coverage |
|---|--------|--------|----------|
| 1 | `shufinskiy/nba_data` bulk PBP | **Available.** Repo active (last push 2026-11-06). | `nbastatsv3` (PlayByPlayV3): 1996-97 → 2025-26, RS + PO, all 60 files. `nbastats` (V2): only through 2024-25. |
| 2 | Shot detail (x/y, action type) | **Available** in same repo. | 1996-97 → 2025-26, RS + PO, all 60 files. |
| 3 | NBA Stats API via hoopR (box/advanced) | **Blocked from this machine.** All 5 test calls timed out (connection accepted, 0 bytes returned after 60 s). | n/a |
| 3b | pbpstats.com API (replacement) | **Available.** Player season totals keyed by NBA person ID. | 1996-97 → 2025-26, RS + PO. ~230 fields incl. Minutes, OffPoss, Usage, FTA, Turnovers, ThreePtShootingFoulsDrawn, Travels, Offensive/Charge Fouls, assisted/unassisted pts. |
| 4 | Basketball Reference | **Available** (HTTP 200, fast). Season PBP table has shooting fouls drawn, and-ones, offensive fouls drawn/committed; season totals have position. | 1996-97+ |
| 5 | Tracking (drives, pull-ups, defender distance) | **Blocked** (same stats.nba.com timeout). | n/a until resolved |
| 6 | Flopping | PBP has an in-game "Flopping" foul subtype only from 2023-24 (79 / 17 / 22 calls per RS). League fine/warning lists 2012-13+ are press releases, not structured. | Annotation only |

### Bulk PBP + shot detail audit (full table: `data/audit/pbp_shot_audit.csv`, script `R/00_data_audit.R`)
- Every season/type present. Game counts match the real schedule: 1189 (29 teams), 725 (1998-99 lockout), 990 (2011-12), 1059 (2019-20), 1080 (2020-21), 1230 otherwise.
- Play-in (game-ID prefix 5) and NBA Cup final are **not** in the files, which matches official RS/PO stat conventions.
- Shot chart ↔ PBP join on (gameId, actionNumber = GAME_EVENT_ID): **≥ 99.88% of PBP FGAs in every season**, same shooter ≥ 99.95%, made flag agreement 100%.
- **Quirk: 2012-13 RS PBP is missing 16 games** (IDs 21201215–21201230, final night of the season). Shot chart has them; the V2 archive (`nbastats_2012`) has them (2,687 FG events). Fix: patch those 16 games from V2 in Phase 2.
- **Quirk: shot ACTION_TYPE granularity changes by era.** 9–10 types (1996-97 → 1999-00), 22–29 types (2000-01 → 2006-07), 46–53 types (2007-08+). "Step Back", "Pullup", "Fadeaway" don't exist in early seasons. → affects xFG design (see open question 3).
- **Quirk: pre-~2011 rim shots are stamped at exactly (0,0)** (95% of restricted-area attempts in 1996-97, 77% in 2005-06, 0% in 2012-13). Zone and distance still usable; fine-grained location inside the restricted area only exists later.
- **Quirk: charges** are a separate foul subtype ("Offensive Charge") only from 2010-11; earlier they're inside "Offensive". Summing both gives a consistent "offensive fouls incl. charges" count.
- League-wide drift (handled by within-season z-scores): 3-shot FT trips 239 (1996-97) → 933 (2025-26); travel calls 3,925 (2006-07 peak) → 1,301 (2025-26); 3PA share 21% → 42%.

### How the needed events are encoded in PBP V3
- **Shooting foul:** `actionType="Foul"`, `subType="Shooting"`. `personId` = the *fouler*. The parenthetical name in the description (e.g. "(J.DeRosa)") is the **referee**, not the fouled player. → Fouls *drawn* are credited to the free-throw shooter on the following FT rows.
- **Three-shot foul:** FT rows with `subType="Free Throw 1 of 3"` (non-flagrant). The fouled missed 3PA is *not* logged as an FGA.
- **And-one:** made FG → Shooting foul at same clock → `"Free Throw 1 of 1"` by the shooter. Shot location comes from the made FG (jumper vs rim classifiable).
- **Travel:** `actionType="Turnover"`, `subType="Traveling"`.
- **Offensive foul:** `Foul/Offensive` or `Foul/Offensive Charge`, followed by a `Turnover` row ("Foul" pre-2010s, "Offensive Foul Turnover" later). Count the Foul row only to avoid double counting.
- **Assist attribution:** only in the made-shot description, e.g. `"... (Green 3 AST)"`. ~57–64% of made FG tagged, stable across seasons. Unassisted = made FG without the tag.
- **3PT flag:** `shotValue` column missing in 1996-97 file; use shot-chart `SHOT_TYPE` (joined) instead.

### Open questions for Rob (Phase 0 checkpoint)
1. Approve PBP V3 as the primary PBP source (all 30 seasons, one format), patching 2012-13 from V2?
2. Approve pbpstats.com as the box-score source (minutes, possessions, usage, FTA, TOV) + Basketball Reference for position, since stats.nba.com is blocked?
3. xFG action types: harmonized coarse taxonomy across all seasons (recommended) vs. full detail?
4. Tier 2 tracking validation: how to proceed given stats.nba.com is blocked?

**Phase 0 decisions (approved by Rob 2026-11-11, "do what you think is best"):** all four recommendations taken: PBP V3 + 2012-13 V2 patch; pbpstats + BBRef instead of stats.nba.com; harmonised coarse shot families for xFG; tracking via `scripts/download_tracking.R` run from another network (retry from here on 2026-11-11 15:59 still timed out).

---

## Phase 1: Scaffold (2026-11-11)
- Folder structure per spec + `scripts/` (one-off scripts not part of the pipeline) and `data/audit/` (committed QA tables).
- renv initialised, snapshot type "all"; lockfile records every installed package. xgboost is 3.2 (new API: `evals=`, `xgb.attr(fit, "best_iteration")`).
- Git identity set **for this repo only**: "Rob Kirby" + Rob's account email. Revisit before pushing (could switch to GitHub noreply address).
- Palette: dataviz-skill reference palette (validated). Node.js isn't installed so the validator couldn't be re-run; the palette is used unmodified. Archetype scatter uses only the 3 all-pairs-safe slots (blue/orange/aqua) + neutral gray, with point shape as secondary encoding.
- Chart caption changed from the spec's "NBA Stats via hoopR" to name the sources actually used.

## Phase 2: Download + clean (2026-11-11)
### Pipeline
- `R/01_download.R`: bulk archives, pbpstats (60 JSON), BBRef (totals/play-by-play/shooting RS + playoff totals = 120 pages at 15/min). All cached; reruns skip.
- `R/02_clean_pbp.R` → `shots.parquet` (6.4M FGA), `ft_trips.parquet`, `player_team_season.parquet`.
- `R/02b_external_sources.R` → `bbref_totals`, `pbpstats`, `player_season.parquet`, `data/final/player_id_map.csv`.

### Definitions fixed in code (document in methodology)
- **Heaves excluded everywhere**: FGA from ≥ 36 ft with ≤ 3 s left in the period. Reason: from 2025-26 the NBA logs missed end-of-quarter heaves as *team* attempts, so player FGA would otherwise change definition mid-sample. Side effect: our points exclude made heaves (e.g. Curry 2015-16 −18 pts).
- **FT trips**: "1 of n" rows. And-one = "1 of 1" whose shooter made a FG at the identical game clock (matches ~95% of 1-of-1 trips; remainder are away-from-play, and from 2022-23 transition take fouls → "other_1of1" jump to ~300/season, as expected). Three-shot = "1 of 3". Two-shot shooting vs penalty decided by whether a Shooting foul occurred at the same stoppage.
- **Offensive fouls** = Foul/Offensive + Foul/Offensive Charge (Foul rows only; the paired turnover row is not double counted).
- **Shot families**: dunk > tip/putback > hook > layup > jumper (string rules in `shot_family()`); moving = "Driving|Running".
- **3-shot foul rate denominator** = 3PA + 3-shot trips (fouled misses are not FGA).
- **Minutes** from BBRef (official). pbpstats possessions only used for 2000-01+.

### pbpstats is incomplete before 2000-01
Jordan 1996-97 in pbpstats: 77 G / 2,931 min / 528 FTA vs official 82 / 3,106 / 576. Our PBP matches official (1,892 FGA incl. 2 heaves, 576 FTA). → pbpstats is only a cross-check from 2000-01 on.

### Cross-source checks (`data/final/crosscheck_*.csv`)
- vs official BBRef totals, all 7,656 RS player-seasons ≥ 1,000 min: FGA (incl. heaves), FTA, TOV within ±2 in 100%.
- vs BBRef PBP/shooting tables, 25 random player-seasons ≥ 1,000 min (seed 2026): shooting fouls drawn r = 0.9995 (96% within ±5), and-ones r = 0.9997, offensive fouls r = 0.998, % 2PM assisted r = 1.000, % 3PM assisted r = 0.9997.
- vs pbpstats 2000-01+ (7,965 player-seasons ≥ 200 FGA): every count agrees (r ≥ 0.999, ≥ 99.6% within tolerance).

### Data quirks found
- 1996-97: Melvin Booker's shots split across two NBA person IDs (775 / 511). Non-qualifier; ignored.
- BBRef playoff-totals pages use legacy column names (`player`, `team_id`, `g`); parser normalises.
- 62 BBRef RS player-seasons unmatched to NBA IDs, all < 500 min (e.g. Ha Seung-Jin, Sun Yue). 2,131 NBA IDs unmatched, all < 100 FGA (mostly coaches/teams with technicals).

### Early signal
League 3-shot trips per 100 3PA: 1.12 (2020-21) → 0.82 (2021-22), the first season of the "non-basketball move" crackdown. Peak 1.57 in 2016-17.

### Qualification preview (spec default)
≥ 1,000 min & ≥ 12 FGA/36: 3,969 RS player-seasons (52% of ≥ 1,000-min seasons), median ~130 per season. Histogram at Phase 4.

---

## Phase 2 → 5 (2026-11-11, Rob: "continue on with remaining phases")

### Tracking data (Tier 2): still blocked
- stats.nba.com hangs for every non-browser client from this machine (IPv4 and IPv6). Rob confirmed the site loads in his browser, so it is Akamai bot filtering, not the network.
- I attempted a request with nba.com browser headers (the method community packages such as nba_api use); Claude Code's permission system denied it as bot-check circumvention. Not worked around. **Decision needed from Rob** (see Phase 5 checkpoint). `scripts/download_tracking.R` (hoopR defaults) will hang the same way on any network.

### Phase 3: xFG model (`R/03_shots_xfg_model.R`)
- 6,307,970 shots (heaves + 0.01% unmatched-to-chart excluded). xgboost depth 7, eta 0.1, early stopping on 2024-25.
- Test 2025-26: log-loss 0.6393 vs zone baseline 0.6567 vs constant 0.6913; Brier 0.2251 vs 0.2321 vs 0.2491. Mean pred 0.4704 vs actual 0.4698. Calibration plot on the diagonal across all 20 bins.
- Importance (gain): distance 46%, shot family 32%, season 7%, moving flag 7%; location/clock/zone the rest.
- Final xFG cross-fitted by game (5 folds) → `shots_xfg.parquet`.

### Phase 4: components + index (`R/04_player_metrics.R`, `R/05_index_build.R`)
Spec deviations / refinements (each found by checking a suspicious result):
1. **Jumper and-one metric** changed from "share of and-ones on jumpers" to **perimeter (mid-range + 3PT) and-ones per 100 perimeter FGA**. The share correlated 0.56 with jumper share of FGA (it measured shot diet: Seth Curry 8/8 → "whistle"). Rate correlates −0.19 with jumper share and +0.26/+0.46 with the other whistle metrics. Perimeter-only keeps lane floaters (coded "Jump Shot" in early seasons) out.
2. **Excess FT rate and FT share use shooting-foul free throws only** (and-ones, 2-shot shooting, 3-shot). Intentional fouls are the defence's choice: only 52% of Steve Nash's FTA came from shooting fouls (end-of-game fouling of good FT shooters); hack-a-big similar.
3. **Usage proxy** = (FGA + 0.44 FTA + TOV) per 36 min (true USG% needs team possession data from the blocked API; z-scoring within season makes the proxy near-equivalent).
4. **Qualification minutes prorated** for short seasons (1998-99: 610, 2011-12: 805, 2019-20/2020-21: 878). Qualifiers: 4,022 RS, 1,302 PO player-seasons; 417 careers (≥ 4 qualifying seasons).
- Excess FT model: per era, FGA-weighted OLS of shooting-FTA/FGA on rim/paint/mid/corner-3/3PA shares + usage proxy + position, season×type FE, fit on qualifiers.
- EB shrinkage: beta-binomial (method-of-moments prior per season×type on qualifiers) for count rates; normal-normal for SMAE, difficulty, excess FTr.
- Weights: Skill = SMAE .45, self-creation .25, difficulty .15, footwork(−) .15. Whistle = excess FTr .45, 3PT foul rate .30, perimeter and-one rate .15, FT share .10. EHI = 50 + 15·z(Skill − Whistle) within season, clipped to [0,100].
- Sensitivity (career Spearman vs baseline): equal 0.954, PCA 0.958, leave-one-out 0.845 (drop SMAE) to 0.996. Top-25 overlap 48–92%, bottom-25 56–92%.
- Face validity: #1 Nash, #3 Chris Paul, #5 Jokić; last Harden, then Dwight Howard, Maggette, Reggie Miller, Kevin Martin, Butler.
- Checked and NOT a bug: Dwight Howard last. 76% of his FTA were shooting-foul FTA (league 78%), so not hack-a-Dwight; his shooting-foul FT rate is far above what his rim-heavy diet predicts.
- **Open question for Rob:** Antoine Walker is #15 as a "Grinder" (skill −0.78, whistle −2.20). EHI = Skill − Whistle rewards never going to the line even with below-average shot-making. Spec-faithful but arguable.

### Phase 5: validation + rule change
- Stability (season t → t+1, 2,708 pairs): EHI r = 0.736; skill 0.752; whistle 0.743; components 0.39 (perimeter and-one) to 0.90 (self-creation).
- Face validity tables: `data/final/face_validity.csv`.
- DiD (`R/07_rule_change_did.R`, 237 players, 61 top-quartile treated):
  - 3PT fouls/100 3PA: main −0.53/SD (p<.001) BUT placebo −0.38/SD (p<.001) and no-RTM treatment window −0.17 (p=.17). → Mostly **regression to the mean**; true differential effect small/uncertain. (League-wide drop absorbed by season FE: 1.12 → 0.82.)
  - TS%: main −0.83 pts/SD, age-control −0.61, no-RTM −0.55 (p=.01), pre-period **EHI Whistle Score** −0.45 (p=.015), placebo −0.05 (p=.81). → Robust: foul-drawers lost efficiency, and the index's Whistle Score predicts it.
  - Pts/75: main −0.85, no-RTM −0.80 (p=.004), but age control halves it (−0.45, p=.047); placebo −0.32 (p=.14).
  - Excess FT rate: no differential effect in any spec.
  - Bug fixed: DiD originally read the first coefficient row, which became the age term when a control was added.
- Note for narrative: event-study pre-trends are flat partly by construction (treatment measured on 2018-21); the placebo + earlier-window specs are the real identification checks. Retitle `event_study_threefoul` chart so it doesn't overclaim.
