# 🌊 Marine Heatwave Tracker

The **Marine Heatwave Tracker** downloads, processes, and visualises the daily global
occurrence of marine heatwaves (MHWs) and marine cold spells (MCSs). Everything is written
in **R**, there is no other language in the stack.

The results are served through an interactive [Shiny](https://shiny.posit.co/) app built
around a [leaflet](https://leafletjs.com/) map. 📖 Detailed instructions on how to use the
app are in the app's own **"About"** tab — this file only documents the codebase.

> 🔗 The live app and citation details are at
> [marineheatwaves.org/tracker.html](http://www.marineheatwaves.org/tracker.html)

---

## 🧠 How it works

1. **Download** — pull new [NOAA OISST v2.1](https://www.ncdc.noaa.gov/oisst) sea-surface
   temperature data (final + preliminary) and merge it into per-longitude NetCDF files.
2. **Detect** — run MHW/MCS event detection per longitude slice against **two**
   climatology baselines (**1982–2011** and **1991–2020**) using the
   [`heatwave3`](https://github.com/robwschlegel/heatwave3) package (a migration from
   `heatwaveR`, currently in progress. See [Updates](#-updates) below).
3. **Roll up** — combine per-longitude results into global daily category files.
4. **Sanity-check** — abort if any calendar day is missing from the processed archive.
5. **Summarise** — rebuild annual summary figures/data once enough of a new year exists.
6. **Publish** — commit and push the (small, git-tracked) result files, ready to be
   synced to the Shiny app.

🖥️ The pipeline and the Shiny app never run in the same process, they only talk to each
other through files on disk (NetCDF, `.Rds`, `.Rda`, `.tif`).

⚙️ Heavy computation is parallelised across OISST's 1440 longitude slices
(`future` + `furrr`), each slice living in its own file.

---

## 📁 File structure

```
MHWapp/
├── MHW_daily.R                 # 🕑 Production entry point, run once daily via cron
├── MHW_daily_functions.R       # 🧰 All functions used by MHW_daily.R
├── MHW_daily_test.R            # 🧪 Ad hoc/manual scratch code for poking at the daily pipeline
├── MHW_annual_summary.R        # 📊 Builds per-year summary figures/data
├── MHW_database.R              # 🗄️ Deprecated one-time database build/backfill script
├── CITATION                    # 📝 How to cite the Tracker
├── LICENSE.md                  # ⚖️ MIT License, applies to this whole repo
├── README.md                   # 👋 That's this file
│
├── data/
│   ├── extract_spatial.R       # ✂️ Ad hoc bespoke time/place extracts → raster/sf formats
│   ├── annual_summary/         # 📦 Per-year MHW/MCS summaries (OISST, CCI, CMC products;
│   │                           #    both climatology baselines) as .Rds/.csv
│   └── published/
│       ├── published.R         # Processes results from MHW-related publications
│       └── Oliver_2018.Rds     # Results from Oliver et al. 2018 (Nature)
│
├── figures/                    # 🖼️ Pre-rendered annual summary figures (.png/.eps)
│                               #    for OISST, CCI, and CMC products
│
├── metadata/
│   ├── metadata.R               # Builds/loads shared metadata (grid coords, masks,
│   │                            # colour palettes, base map, cross-product matching)
│   ├── final_dates.Rdata        # Dates of finalised OISST data
│   ├── prelim_dates.Rdata       # Dates of preliminary OISST data (~last 2 weeks)
│   ├── map_base.Rdata           # A ggplot2-friendly global map (not used by the Tracker)
│   ├── OISST_ocean_coords.Rdata # All OISST pixel coordinates over ocean
│   ├── OISST_no_ice_coords.Rdata# OISST pixel coordinates with no ice present
│   ├── OISST_leaf_coords.Rdata  # Leaflet-friendly OISST coordinates
│   ├── OISST_ice_coords.tif     # Ice mask raster
│   └── lon_lat_OISST_area.RData # Per-pixel surface area
│
└── shiny/                       # 🗺️ The MHW Tracker Shiny app
    ├── global.R                 # One-time setup per app instance
    ├── functions.R              # Non-reactive helper functions (e.g. pixel time series)
    ├── server.R                 # Reactive server logic (map, value boxes, plots)
    ├── ui.R                     # bslib/leaflet page layout
    └── style.css                # App styling
```

📌 A few things worth knowing:

- Large data folders (`data/OISST`, `shiny/OISST`, `shiny/event`, `shiny/thresh`,
  `shiny/cat_clim`, …) are **gitignored** and populated by the daily pipeline / rsync —
  a fresh checkout of this repo can't run the pipeline or the app end-to-end without
  that data already present on the host.
- Each data-bearing folder (`data/`, `metadata/`, `shiny/`) has its **own** `.gitignore`
  rather than one top-level file. Tthis is deliberate for more flexible control over
  what's excluded where.
- Every script starts with a header comment naming numbered sections — check there first
  for a script-specific breakdown.

---

## 📖 Citation

See the [`CITATION`](CITATION) file, or cite via Zenodo DOI
[10.5281/zenodo.3787872](https://doi.org/10.5281/zenodo.3787872).

## ⚖️ License

[MIT](LICENSE.md) — applies to every file in this repository.

---

## 🆕 Updates

> 🚧 **In progress:** migrating the detection engine from `heatwaveR` to `heatwave3`.
> File/variable naming under `event`/`thresh` reflects the new package; `cat_lon`-based
> files are deprecated in favour of it. Expect commented-out legacy code with
> explanatory notes scattered through the codebase until this settles.

* **July 24th, 2026**
  * Refactored the multicore/parallel structure of the daily pipeline (with Claude's
    help) and sped up the app by targeting its slowest portions — no material change to
    file structure. A lot of legacy code is temporarily left commented out with notes,
    to be deleted once the changes prove stable.

* **July 23rd, 2026**
  * Activated `heatwave3` loading code

* **June 29th, 2026**
  * Removed the `terra` package from startup (an odd external-file issue) and moved the
    "About" section to its own accordion with a modal launch button; reactivated the
    time-series panel and related code

* **June 15th, 2026**
  * Rolled back `heatwave3` use in the front-end for now, pending further integration
    work; general clean-up of redundant code

* **June 11th, 2026**
  * Set the ice mask off by default

* **June 6th, 2026**
  * Began integrating the new `category_daily3()` function into the back-end and app

* **May 29th–30th, 2026**
  * First daily output produced using `heatwave3` instead of `heatwaveR`, with all
    downstream file-structure changes propagated to the UI
  * Corrected a MCS bug that was using MHW threshold values
  * Removed the old Shiny modules (now published separately on Zenodo)

* **May 28th, 2026**
  * Began integrating `heatwave3` into the full stack

* **July 29th, 2023**
  * A large series of improvements to the UI and back-end brought this project up to v2.0

* **June 23rd, 2020**
  * Fixed a bug in the main pipeline caused by a change in the default behaviour of
    `dplyr::right_join()` where joining in NA pixels for landmass areas were now being
    inserted at the bottom of the data.frame instead of in their correctly ordered place.
    This is why the world looked like it was melting.

* **June 1st, 2020**
  * Added 'About' text to the revamped 'Comparison' and 'Summary' tabs

* **May 28th, 2020**
  * A tweak to the daily pipeline

* **May 11th, 2020**
  * Added `metadata.R` and `MHW_database.R` scripts

* **May 6th, 2020**
  * Made a couple of pipeline improvements
  * Released as v1.0.1

* **May 5th, 2020**
  * The Tracker now uses OISST v2.1 from 2016 onward
  * The annual summaries from 2016 forwards have been updated accordingly
  * The v2.0 annual summaries are stored in `/annual_summaries/v2.0/`
  * Added regional analysis links for several Fishforecast pages around Northern Europe
  * Released v1.0.0 of the Marine Heatwave Tracker on GitHub
  * DOI created through Zenodo: 10.5281/zenodo.3787873

* **May 1st, 2020**
  * The MHW Tracker is now covered under the MIT License for open source software
  * Cleaned up the code base

* **April 30th, 2020**
  * Added summaries for the count of MHWs/pixel/year
  * Updated the UI so these files can be downloaded

* **April 6th, 2020**
  * Added colourbar for red-blue layers
  * Updated 'About' section with info for map layers
  * Removed static time series modal tab
  * Added download for time series and lolliplot figures
  * Minor bug fix to event filtering for modal panel figures

* **April 4th, 2020**
  * Added Oliver et al. 2018 MHW trend layers

* **March 2nd, 2020**
  * Changed the control panel position to accommodate the Chrome web browser
  * Added functionality to download the anomaly data

* **March 1st, 2020**
  * Added anomaly layer to leaflet map

* **February 7th, 2020**
  * Improvements to Summary tab UI

* **February 6th, 2020**
  * UI even more mobile friendly with option to hide controls
  * Users may download map data for a given range of dates

* **February 5th, 2020**
  * Users may now change the map background (provider tiles)
  * Improved popups
  * Added static plot and lolliplot tabs to pixel plotting modal window
  * Becoming increasingly mobile friendly

* **February 3rd, 2020**
  * Added an animation menu and functionality
  * Added a summary tab that allows users to choose pre-rendered annual summaries and
    download the figures

* **September 23rd, 2019**
  * When one clicks over the eastern Pacific or Mediterranean a link will now be in the
    popup that allows the user to go to another website that provides a regional
    analysis of MHWs there

* **July 22nd, 2019**
  * Added more error trapping

* **July 17th, 2019**
  * Some minor improvements

* **June 6th, 2019**
  * Added the climatology period used to the about section

* **May 31st, 2019**
  * Attempting to connect Google Analytics

* **May 29th, 2019**
  * Corrected typo in About section

* **May 28th, 2019**
  * Edited the text in the About section
  * Added a link to the press release

* **May 27th, 2019**
  * Changed use of `plyr::ldply` to `plyr::l_ply` as this was causing issues

* **May 23rd, 2019**
  * Corrected a couple of typos in the welcome message
  * Changed welcome popup to a modal window for more stability

* **May 21st, 2019**
  * `MHW_daily.R` now running at 22:00 AST via a cron job
  * Reduced verbosity of output so that logs are more readable

* **May 16th, 2019**
  * Added coloured category filtering buttons
  * Added info popup on start up
  * Fixed weakness in logic trapping for daily updating script

* **May 15th, 2019**
  * Playing with coloured button options
  * Added category colours to time series plot
  * Removed category legend from map

* **March 18th, 2019**
  * Updated the animation script to create chosen animations of infamous MHWs

* **March 8th, 2019**
  * More aggressive error trapping
  * Preventing access to pre-1985 data as these are apparently error prone

* **March 6th, 2019**
  * Added logic breaks to catch when the NOAA data are not up on the ERDDAP server
  * Added screen to filter out erroneously high OISST data
  * Added functionality to download preliminary data
  * Updated pipeline to create new app files based on final data
  * Preliminary data now a live part of the MHW results
  * These changes are accounted for in the `about` section

* **February 14th, 2019**
  * Some tweaks to the daily scripts
  * Added a "Why should I care?" section in the about panel
  * Added a break in the category screening that should help with speed

* **February 13th, 2019**
  * Two bug fixes
  * A CSS overhaul
  * Project-wide improvements to UI
  * Edited about section to reflect changes

* **February 12th, 2019**
  * Updates to about section
  * Added a bit more safety to daily script

* **February 11th, 2019**
  * Addressed heatspike issue
  * Changed data syncing with app so no git pulling is required
  * Smoothed out bugs in daily updating script
  * Minor about section edits

* **February 7th, 2019**
  * More snag list

* **February 6th, 2019**
  * Working through the snag list

* **February 5th, 2019**
  * Massive update to the About section

* **January 31st, 2019**
  * More optimising of daily script

* **January 30th, 2019**
  * Daily script complete and all 2018 data processed

* **January 29th, 2019**
  * Began writing the daily script

* **January 25th, 2019**
  * Solved the raster wrap-around issue
  * Fixed/added several UI features

* **January 24th, 2019**
  * Added full geom_flame visual output
  * Expanded on label information

* **January 10th, 2019**
  * Finished first draft of UI
  * Finished NetCDF back-end
  * Finished native R back-end
  * Experimenting with an entirely plotly based product

* **January 9th, 2019**
  * Work on shiny UI
  * Work on NetCDF database

* **January 8th, 2019**
  * More work on efficient SQL database construction
  * Testing NetCDF lon slices as a viable option for daily OISST data

* **January 4th, 2019**
  * Experimenting with visualising options
  * Massive overhaul to the foundation of the app

* **January 3rd, 2019**
  * Playing around with SQLite data appending workflow

* **December 18th, 2018**
  * Played around with mapping options
  * All of 2017 data now live

* **December 17th, 2018**
  * Created basic SQL databases to deal with RAM issues

* **December 10th, 2018**
  * Sorted out a basic NetCDF workflow
  * Integrated NetCDF files into app
  * Changed file pathways

* **December 5th, 2018**
  * Scaled the data back to one month again
  * Removed rate of onset and decline and introduced cumulative intensity
  * Working on NetCDF data storage

* **December 4th, 2018**
  * UI now allows for showing categories or event metrics per pixel

* **November 30th, 2018**
  * Some old changes that needed pushing
  * Playing with UI
  * Added subset of event data

* **October 19th, 2018**
  * Attempted to host all of 2017 data but it fell over
  * Created a second instance at "MHWtracker"

* **October 18th, 2018**
  * Global proof of concept
  * Basic clickability added

* **October 11th, 2018**
  * Playing about with popping the leaflet into shiny
  * Basic product up and running

* **October 5th, 2018**
  * Rather moving towards leaflet
  * Made some baby steps

* **September 25th, 2018**
  * First push of initial MHW calculations for South Africa region
  * An attempt was made at a basic plotly diagram
