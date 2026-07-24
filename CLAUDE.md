# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

The Marine Heatwave Tracker: an R pipeline that downloads NOAA OISST v2.1 sea-surface
temperature data, detects marine heatwaves (MHW) and cold spells (MCS) against two
climatology baselines (1982-2011 and 1991-2020), and serves the results through an
interactive Shiny/leaflet map. Everything is R; there is no other language in the stack.

## Running things

There is no build/test/lint tooling (no `renv`, no CI, no test framework) — this is a
personal research/production pipeline run by hand or via cron, not a package. "Running"
a script means `source("script.R")` from an R console with the working directory set to
the repo root (except `shiny/*.R`, which run with working directory `shiny/`).

- `MHW_daily.R` — the production entry point, run once daily via cron. Source it with
  `source("MHW_daily.R")` **from a terminal R session, never from RStudio Server** — the
  header explicitly warns that RStudio Server breaks NetCDF write privileges for the
  files this script updates in place. It downloads new OISST data, updates event/category
  NetCDF files, regenerates daily global summary files, runs `MHW_annual_summary.R`, then
  commits and pushes the repo (`git commit -a`, `git pull`, `git push`) as its last step.
- `MHW_daily_functions.R` — all functions used by `MHW_daily.R`; sourced by it, not run
  directly.
- `MHW_daily_test.R` — ad hoc/manual exploration snippets for poking at the daily
  pipeline and functions; not an automated test suite.
- `MHW_annual_summary.R` — builds per-year summary figures/data from the daily category
  files; invoked automatically by `MHW_daily.R` once there's a week of data into the new
  year, or can be sourced standalone to rebuild/backfill annual summaries.
- `MHW_database.R` — one-time database/setup script (OISST/CCI/CMC grid alignment,
  historical backfill). Not part of the daily run; only re-run deliberately.
- `data/extract_spatial.R` — ad hoc script for pulling bespoke time/place extracts and
  converting to spatial (raster/sf) formats on request.
- `data/published/published.R` — one-off processing of results from published papers
  (e.g. Oliver et al. 2018) into a format the Tracker can display.
- `metadata/metadata.R` — builds/loads the shared metadata objects (OISST grid
  coordinates, ocean/ice/no-ice masks, colour palettes, base map, cross-product coordinate
  matching). Sourced by the pipeline scripts.
- Shiny app: `cd shiny && R -e "shiny::runApp()"` (or open in RStudio and click Run App).
  `global.R` runs once per app start; it hard-fails via `stop()` if the `OISST`, `event`,
  `thresh`, or `cat_clim` data folders aren't present alongside the app — these are
  populated by rsync from the pipeline output, not by the scripts in this repo, and are
  gitignored (see `shiny/.gitignore`).

Because most data directories (`data/OISST`, `shiny/OISST`, `shiny/event`, `shiny/thresh`,
`shiny/cat_clim`, etc.) are gitignored and populated by the daily pipeline / rsync, a
fresh checkout of this repo cannot run the daily pipeline or the Shiny app end-to-end
without that data already present on the host.

## Architecture

**Pipeline → static result files → Shiny app.** The daily script and the Shiny app never
run in the same process; they're connected only through files on disk (NetCDF, `.Rds`,
`.Rda`, `.tif`). Any change to the shape/naming of an output file in the pipeline must be
matched in `shiny/global.R`, `shiny/functions.R`, or `shiny/server.R`, which read those
files by convention (path patterns, filename-embedded dates) rather than an explicit
schema.

**Data flow (see `MHW_daily.R` section headers 1-7 for the authoritative sequence):**
1. Download new NOAA OISST NetCDF files (final + preliminary) from NCEI, merge into
   per-longitude-slice NetCDF files (`../data/OISST/oisst-avhrr-v02r01.ts.1440.nc` and
   related). Preliminary data covers the most recent ~2 weeks before being superseded by
   final data.
2. Compute/update MHW and MCS events and daily categories per longitude slice, against
   both climatology baselines, using the `heatwave3` package (migrated from `heatwaveR`;
   see "Updates" in README.md — this migration is still in progress, and `cat_lon`-based
   files are marked deprecated in favour of files under `event`/`thresh`).
3. Roll per-longitude results into global daily category files (`cat_clim_global_daily`)
   under `../data/cat_clim/<year>/`. The per-day `.Rda`/`.Rds` files here (read by
   `shiny/server.R`'s `downloadMapData()` for the map-data download feature) are only
   backfilled from the `heatwave3` migration date forward (2026-07-08) — `.tif` files for
   the same directory go back further. A download date range that reaches earlier than the
   `.Rda` backfill start will error with "cannot open the connection" in `readRDS_date()`.
4. Sanity-check that no calendar day is missing from the processed date index
   (`current_dates`), aborting with `stop()` if so.
5. Recompute annual summaries (`MHW_annual_summary.R`) once enough of the new year's data
   exists.
6. Commit and push the repo (data files tracked in git — mainly small summary/metadata
   files, not the large NetCDF stores).
7. Housekeeping notes (kept as commented-out code, not automated).

**NetCDF chunking is write-optimised, not read-optimised.** The per-longitude store files
(`OISST/oisst-avhrr-v02r01.ts.<lon>.nc`, `thresh/MHW_*_clim.nc`, `event/MHW_*_events.nc`)
are chunked to make the daily pipeline's append-one-day-across-all-lats write pattern
cheap — e.g. the OISST `sst[lat,lon,time]` variable is chunked `[720,1,1]` (one chunk =
all 720 lats for a single day). This is the opposite of what `shiny/functions.R`'s
`sst_seas_thresh_ts()` needs (one lat's full ~16,000-day time series): HDF5 must
decompress every chunk in the file to pull out a single lat, since a chunk can't be
partially read. The `event/*_events.nc` files are worse — chunked as a single chunk
spanning the *entire* `event` dimension, so extracting one lat's events means
decompressing the whole per-longitude event table regardless of how few events that lat
has. This is why `pixelData()`'s per-click cost in `shiny/server.R` scales with total
file size (i.e. with how MHW/MCS-active that longitude has been), not with the size of
the actual answer. Confirmed via `ncdf4::nc_open()` on the actual files (2026-07-24) —
not visible from reading the R code alone, since chunking is a file-level HDF5 property
set at creation time, not a script parameter.

**Longitude-slice partitioning.** OISST is a 0.25° global grid (1440 longitude steps ×
720 latitude steps, see `lon_OISST`/`lat_OISST` in `metadata/metadata.R` and
`shiny/global.R`). Nearly all heavy computation is parallelised (`doParallel`,
`plyr::l_ply`/`ldply` with `.parallel = TRUE`) across the 1440 longitude slices, each
slice living in its own file (`../data/thresh`, `../data/event`, `../data/cat_lon`).
When editing pipeline functions, preserve this per-slice file layout — downstream code
assumes one file per longitude index.

**Dual climatology baselines.** Every MHW/MCS computation is run twice, once against
1982-2011 and once against 1991-2020 (`base_years` parameter threaded through most
pipeline functions). File and variable naming consistently encodes which baseline a
result belongs to — match this convention when adding new derived outputs. Historical
CCI/CMC-product comparisons additionally use a 1992-2018 baseline (see `figures/`
filenames).

**MHW vs MCS symmetry.** Marine heatwaves (MHW) and marine cold spells (MCS) are computed
and stored as parallel, largely symmetric pipelines (separate `event`/`thresh`
directories, separate colour palettes `MHW_colours`/`MCS_colours`, an `MHW` boolean
argument threaded through shared functions like `event_annual_state()`). MCS has a fifth
"Ice" category that MHW does not.

**Shiny app structure** (`shiny/`):
- `global.R` — one-time setup per app instance: loads packages, locates data files,
  builds colour palettes and static lon/lat vectors, determines `current_dates`. Its
  `ice_proj <- raster::raster("OISST/ice_proj.tif")` call is the single biggest line in
  app startup (~3.5-4s of a ~5s cold start) — but the cost is the `raster` package's
  one-time namespace/GDAL initialisation on its *first* use in the R process, not the
  205KB tif file itself (a second `raster::raster()` call on any file, in the same
  session, takes ~0.02-0.07s). Don't mistake this for a slow file and try to shrink/
  recompress `ice_proj.tif` — that won't move the number. The tax is unavoidable as long
  as something calls `raster::raster()`; it just happens to land on whichever call runs
  first. Migrating off `raster` (e.g. to `terra`, which `leaflet::addRasterImage` also
  supports) would be the way to actually remove it.
- `functions.R` — non-reactive helper functions (e.g. `sst_seas_thresh_ts()` for
  building a single-pixel time series by reading directly from the per-longitude
  NetCDF/threshold files).
- `ui.R` — `bslib`/`bsicons` page layout: summary value boxes, a `leaflet` map card with
  a sidebar of controls (date, layer, climatology baseline, category filters), tabs for
  comparison/summary/about.
- `server.R` — reactive logic wiring the controls to map layers, value boxes, and modal
  time-series plots (`plotly`). Organised by output/feature area, not by input.
- `style.css` — app styling.

**Products beyond OISST.** `MHW_database.R` and `data/published/published.R` also handle
CCI and CMC satellite SST products, cross-matched onto the OISST grid via k-nearest-
neighbour (`FNN::knnx.index`, see `X_OISST_coords()` in `metadata/metadata.R`) so results
from different native resolutions can be compared/displayed on the same pixel grid.

## Conventions to preserve when editing

- Scripts begin with a header comment naming the file and enumerating its numbered
  sections (e.g. `# 1: Setup`, `# 2: ...`). Keep this pattern when adding sections.
- `.libPaths(c("~/R-packages", .libPaths()))` at the top of scripts points at this
  server's custom package library — don't remove it.
- Long-running/parallel sections are commented with rough wall-clock timings (e.g.
  `# ~2 seconds per cycle`, `# 10 minutes on 50 cores`) gathered from the production
  server. Preserve/update these when you materially change a hot loop — they're load-bearing
  for deciding core counts and whether to babysit a run.
- Each data subdirectory (`data/`, `metadata/`, `shiny/`) has its own `.gitignore` rather
  than one top-level file, deliberately (see README.md) — respect that split rather than
  consolidating.
- Manual/backfill invocations are kept as commented-out code directly beneath the
  production call they vary (see the bottom of `MHW_daily.R` and `MHW_daily_test.R`).
  Follow this pattern for new one-off maintenance snippets rather than deleting them
  after use.
