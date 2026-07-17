# Data refinements: stock seeds and point-in-time vintages

Two remaining data items from the review roadmap and the income-side scope. Both
are honestly assessed here rather than half-built, because each is a *data
availability* limitation, not a modelling gap.

---

## 1. Stock-level seeds (govt debt `BG`, net foreign liabilities `VNFL`)

The fiscal and external stock accounting builds **flows** from real ABS data
(net lending from the reconciled fiscal block; the current account from real BoP
income). Only the **starting level** of each stock is a seed, and the model
accumulates the real flows forward from it.

**Status: calibrated proxies, which are realistic.**

| Stock | Seed | Real level | Verdict |
|---|---|---|---|
| `VNFL` (net foreign liabilities) | `nfl_seed` per cent of GDP (pass ~55) | Australia's net IIP ~55-60% of GDP | the proxy is on the money |
| `BG` (government debt) | `fiscal_bg_target` (default 30%) | Commonwealth+state net debt ~30-45% over the demo window | realistic |

**Why not the exact ABS series.** A clean quarterly **general-government net
debt** series is not published in the national accounts (ABS 5512.0 GFS is
*annual*; the AOFM publishes gross CGS on issue, not consolidated net debt). The
**net IIP level** exists in ABS 5302.0 but under opaque, sub-component series IDs
that did not resolve cleanly. Because the seeds only set the *level* (the changes
are already real) and the calibrated proxies match the real levels, this is a
low-value refinement. The path to do it properly: add the net-IIP-level series
(5302.0) for `VNFL` and a Chow-Lin'd GFS net-debt series (5512.0, annual) for
`BG`, then seed the stock *history* from them rather than the target ratio (the
same pattern the reconciled fiscal block already uses for the debt jump-off).

---

## 2. Point-in-time vintages (review rec D1) — a prospective-data task

The review's D1: "Build a true point-in-time vintage store and a vintage-aware
jump-off." SIBYL's `vintage` column is currently a **download-date stamp**, not a
point-in-time reconstruction, so a past round is not byte-reproducible against
the data as it stood at the time.

**Status: cannot be retro-fitted; it is a forward-accumulation task.** This is a
genuine data-infrastructure workstream, not something completable in one pass,
because the *historical* archives mostly do not exist to fetch:

| Source | Point-in-time availability | What's feasible |
|---|---|---|
| **FRED** (US world proxies) | **Yes** -- ALFRED archives, via `fredr`'s `realtime_start`/`realtime_end`. | A vintage-aware FRED fetch *is* buildable now and would make the FRED subset reproducible. |
| **ABS** (the bulk of MARTIN) | **No** historical real-time archive published. | Vintages can only be **accumulated prospectively** (snapshot each release going forward) -- or sourced from an external real-time database if one exists. |
| **RBA** | Limited. | Same: prospective accumulation. |

**Honest conclusion.** A *complete* point-in-time store is not achievable
retrospectively for the ABS/RBA series that dominate MARTIN — the real-time data
simply was not archived. The deliverable D1 actually implies is:

1. **Now:** a vintage-aware **FRED** fetch (ALFRED `realtime_*`) for the world
   proxies — the one source with a real archive — plus pinning `renv.lock`
   (already done) and the frozen 2019Q3 fixture for the reference path.
2. **Going forward:** snapshot every ABS/RBA release into the parquet cache keyed
   by *true* vintage date, so that *future* rounds become byte-reproducible even
   if past ones cannot be.
3. The add-factor handover (`fitted + af = actual`) should re-pin against the
   as-of data once (1)/(2) exist — the mechanism is ready; only the vintaged data
   is missing.

This is logged as the honest scope rather than implemented, because steps (2)/(3)
require data that accrues over time, and step (1) only covers the FRED minority
of the database. It does not block any current functionality — every feature and
the frozen reference path are unaffected.

---

## 3. Pre-1978 labour backcast (review rec R2) — investigated, not wired in

`series_catalogue.csv`'s `LUR` row already documents an intended fallback
("falls back to UR_hist when missing") that was never implemented — the
Modellers' Database backcast this refers to. The EViews import (`import_data.prg`)
sources five pre-1978 labour series from the June 2011 ABS "Modellers' Database"
feature article (catalogue number `1364.0.15.003`):

| ABS series ID | Target MARTIN var | Description |
|---|---|---|
| `A2454521V` | `ur_hist` | Unemployment rate backcast |
| `A2454517C` | `pop_hist` | Population backcast |
| `A2454568C` | `lhpp_hist` | Hours per person backcast |
| `A2454516A` | `le_farm_hist` | Farm employment backcast |
| `A2454518F` | `le_nonfarm_hist` | Non-farm employment backcast |

**Status: not added to `series_catalogue.csv` — the fetch plumbing does not
tolerate this cat_no failing gracefully, verified offline.**

Two checks, both doable without hitting the network:

1. **`readabs`'s own catalogue directory doesn't know this cat_no.**
   `read_abs(cat_no = ...)` resolves catalogue numbers against the package's
   bundled lookup table (`readabs:::abs_lookup_table`, ~300 rows of modern
   ABS Time Series Directory catalogues). `1364.0.15.003` — the ABS's old
   pre-2010 "Modellers' Database" feature-article numbering scheme — is not
   in it:
   ```r
   > readabs:::abs_lookup_table |> nrow()
   [1] 300
   > any(grepl("1364", readabs:::abs_lookup_table$catalogue))
   [1] FALSE
   ```
   That is strong evidence `read_abs(cat_no = "1364.0.15.003")` cannot
   resolve at all today (the release predates the modern catalogue-slug
   system readabs scrapes) and would error rather than return rows.

2. **`R/fetch_abs.R` has no per-catalogue error isolation.** `fetch_abs()`
   groups the requested rows `by_cat <- split(source_rows, source_rows$source_table)`
   and loops with `purrr::imap()` — there is no `tryCatch` around the
   individual `readabs::read_abs(cat_no = cat_no, ...)` call. A single dead
   `cat_no` throws out of that loop, out of `fetch_abs()`, out of
   `fetch_source()`, and is only caught by `update_data()`'s *per-source*
   (not per-catalogue) `tryCatch`. The blast radius is therefore not "the 5
   new series fall back to the fixture" — it is **the entire `abs` source
   for that run** (100+ series) silently reverting to
   [`merge_with_fallback()`](../R/merge.R)'s fixture backstop, because
   `to_martin_database()` never receives *any* abs-sourced data once
   `fetch_abs()` has thrown.

   `merge_with_fallback()` does mean the pipeline never crashes and the
   frozen fixture path is unaffected either way — but "every live ABS
   series silently goes stale on every run" is not the graceful, contained
   degradation the R2 recipe calls for, so per the explicit instruction to
   skip the catalogue change if the plumbing can't isolate the failure,
   this is documented here instead of wired in.

**The path to do it properly** (either would satisfy the "verify graceful
tolerance" bar this time):

- Confirm the *modern* ABS Time Series Directory home for these five
  historical labour series (if the Modellers' Database was folded into a
  current catalogue, e.g. under 6202.0 or 6291.0.55.001's historical
  revisions) and catalogue them with a `source_table` `read_abs(cat_no=)`
  can actually resolve; or
- Harden `fetch_abs()` to fetch per-catalogue-group inside its own
  `tryCatch` (mirroring `update_data()`'s per-source tolerance one level
  down) so one dead `cat_no` degrades only its own rows, then catalogue
  these five via the existing `readabs::read_abs(series_id = ...)` direct
  path already used standalone by `fetch_job_vacancies()` (which does not
  depend on the cat_no catalogue lookup at all).

Either changes `R/fetch_abs.R`, which this lane does not own — noted here
as the follow-up recipe rather than attempted.
