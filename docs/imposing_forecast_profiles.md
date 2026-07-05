# Imposing forecast profiles (conditional forecasting)

The core forecasting move: pin certain variables to judgemental paths and let
MARTIN solve everything else around them. The imposition differs by variable —
you might fix the **whole path** of the global variables but only the
**next quarter** of CPI and unemployment. Both, in one solve, via
`solve_martin(profiles = ...)`.

```r
source("setup.R")
db <- read_fixture()
db <- extend_exogenous(db, end_quarter = "2028Q2")   # exogenous paths to the horizon
base <- solve_martin(db, horizon = c("2015Q1", "2028Q2"))

profiles <- rbind(
  # a FULL PATH for a global variable — every forecast quarter:
  data.frame(variable = "WY",  quarter = quarter_seq("2026Q2", "2028Q2"),
             value = my_world_gdp_path),
  # a 1-QUARTER-AHEAD nowcast for CPI and unemployment:
  data.frame(variable = "PTM", quarter = "2026Q2", value = my_cpi_nowcast),
  data.frame(variable = "LUR", quarter = "2026Q2", value = my_unemp_nowcast)
)

fc <- solve_martin(db, horizon = c("2015Q1", "2028Q2"), profiles = profiles)
```

The runnable demo is `scripts/10_forecast_profiles.R` (it runs on the bundled
fixture so it works offline; committed output is in `results/profiles/`).

---

## How it works

`profiles` is a tidy `data.frame(variable, quarter, value)` in each variable's
**native units** — the level the database stores (`WY` is a world-GDP index,
`PTM`/`P` are CPI index levels, `LUR`/`NCR` are rates in per cent). For each
variable, `solve_martin()`:

1. **splices** the supplied values into the model database, and
2. **`Exogenize`s** that variable over the **span of its own quarters** — bimets
   then holds it at those values and solves its equation elsewhere.

Because each variable gets its own window, a full global path and a
single-quarter CPI pin coexist in one coherent solve. Outside its window a
variable is fully endogenous again, so a 1-quarter nowcast **propagates and then
decays** under the model's own dynamics rather than being held flat.

Values are absolute, so they are normally built **relative to a baseline** solve
(baseline value × a factor, or baseline ± a delta) — see the demo.

### The two patterns

| | Global variables | CPI / unemployment |
|---|---|---|
| examples | `WY`, `WP`, `WPCOM`, `WRR`, `WR2R` | `PTM` (trimmed-mean CPI), `P`, `LUR` |
| window | the **entire** forecast (e.g. 2026Q2–2028Q2) | usually **one quarter** (a nowcast) |
| after the window | n/a (imposed throughout) | **solves freely** — model-determined |
| why | these are inputs you form a view on directly | you know the near-term reading; let the model take it from there |

### What it does to the rest of the model

The imposed values are not cosmetic — they flow through every equation that
reads them. From `scripts/10_forecast_profiles.R` (WY +2% and WPCOM +10% held
across the forecast; PTM +0.4% and LUR +0.3 ppt imposed for the first quarter
only), deviations from baseline:

```
var      2026Q2   ...      2028Q2
WY      +2.000%           +2.000%     <- global, held every quarter
WPCOM  +10.000%          +10.000%     <- global, held every quarter
TOT     +1.163%           +4.687%     <- terms of trade rise
RTWI    -1.779%           +2.052%     <- real exchange rate
Y       +0.025%           +1.074%     <- GDP
PTM     +0.400%           -0.021%     <- pinned Q1 only, then free
LUR     +0.300p           +0.112p     <- pinned Q1 only, then free
NCR     -0.236p           -0.542p     <- cash rate (Taylor rule) responds
```

`PTM`/`LUR` hit their imposed value exactly in the first quarter and then move
away from it — that is the model solving them, not the imposition.

---

## Notes & gotchas

- **Contiguous windows.** Each variable's quarters must be a contiguous run
  (they define one `Exogenize` window). Impose disjoint quarters in separate
  solves.
- **Native units / levels.** Provide the level the database holds. To impose a
  *growth rate* on a price/quantity, convert to the level first
  (`value = lag_level * (1 + rate)`), as the demo does for `WY`/`WPCOM`.
- **Global variables are behavioural here.** In this bimets port `WY`/`WP`/
  `WPCOM`/`WRR` are AR processes, so imposing a path means `Exogenize`-ing them
  — which `profiles` does automatically. (Truly exogenous variables are just
  read from the database, so a profile on one is spliced in without an
  `Exogenize` window.)
- **Composes with add-factors.** `profiles` (impose a level path) and
  `adjustments` (perturb an equation's residual) can be used together in the
  same `solve_martin()` call.
- **Check convergence.** As always, read `attr(fc, "convergence")`.
- **`profiles` vs `exogenize`.** `exogenize` holds variables at a *baseline
  tibble's* path over a *single, shared* range; `profiles` is the general
  conditioning tool — per-variable windows and the values supplied inline.
