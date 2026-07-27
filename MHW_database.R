# MHW_database.R
# This script builds the full OISST backend from scratch on a machine that has
# none of it yet, so the project can be stood up on a fresh clone rather than
# relying on data that only ever existed on the original server.
# It is a one-time/deliberate-rerun script (not part of the daily cron job) and
# reuses the functions already defined in "MHW_daily_functions.R" rather than
# redefining them here.
# NB: run via :
# source("MHW_database.R") 
# in a terminal R session, not an IDE (e.g. RStudio Server).
# It performs the following tasks:
## 1: Sets up the environment
## 2: Downloads the full NOAA OISST daily archive (1982-01 onward)
## 3: Builds the 1440 per-longitude OISST NetCDF files
## 4: Builds the per-longitude climatology (thresh) files for both baselines
## 5: Symlinks the data folders into shiny/ for the Shiny app


# 1: Setup ------------------------------------------------------------------

source("MHW_daily_functions.R")

# Directories a fresh machine won't have yet. Some of the functions below
# (OISST_url_daily_save() for the year sub-folders, create_thresh() for the
# MHW/MCS thresh outputs) assume these already exist rather than creating them
dir.create("../data/OISST/daily", recursive = TRUE, showWarnings = FALSE)
dir.create("../data/thresh/MCS", recursive = TRUE, showWarnings = FALSE)

# Parallel worker plan: separate processes (multisession), never fork-based,
# same rationale as MHW_daily.R (see the fork/HDF5-hang notes in
# MHW_daily_functions.R). Workers independently source MHW_daily_functions.R
# at startup so they end up with identical state to the main session.
n_workers <- max(1, floor(parallel::detectCores()/2))
oisst_cwd <- getwd()
oisst_cl <- parallelly::makeClusterPSOCK(
  n_workers, rscript_libs = .libPaths(),
  rscript_startup = bquote({setwd(.(oisst_cwd)); source("MHW_daily_functions.R")})
)
future::plan(future::cluster, workers = oisst_cl)


# 2: Download the full OISST daily archive -----------------------------------

# Get the source index of monthly file folders, from 1982-01 onward (matches
# the climatology baseline start date used throughout the rest of the
# pipeline. The archive technically goes back to 1981-09, but it's cleaner
# to start at 1982-01 and the first few months of 1981 are not used in any
# downstream calculations anyway.
print(paste0("Fetching OISST folder names at ",Sys.time()))
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"
OISST_url_month_get <- getURL(OISST_url_month)
OISST_table <- readHTMLTable(OISST_url_month_get, as.data.frame = TRUE)[[1]] |>
  rename(months = V1) |> drop_na()
OISST_months <- slice(OISST_table, which(OISST_table$months == "198201/"):nrow(OISST_table)) |>
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) |>
  mutate(months = gsub("-", "", substr(months, 1, 7)))

# Find and download every daily file for those months
# NB: on a fresh machine this is the entire archive - tens of thousands of
# files - so this step dominates the runtime of this script
print(paste0("Fetching OISST file names at ",Sys.time()))
OISST_filenames <- furrr::future_map_dfr(OISST_months$months, OISST_url_daily) |>
  dplyr::select(files, t, full_name)

if(length(OISST_filenames$files) > 0){
  cat(paste0("Saving ",nrow(OISST_filenames)," files locally at ", Sys.time()), "\n")
  furrr::future_walk(OISST_filenames$full_name, OISST_url_daily_save)
}


# 3: Build the 1440 per-longitude OISST NetCDF files -------------------------

# Refresh the daily-file index in the main session - the copy metadata.R
# created in Section 1 (via MHW_daily_functions.R) predates the downloads above
OISST_daily_nc_files <- dir("../data/OISST/daily", pattern = "oisst-avhrr", full.names = TRUE, recursive = TRUE)
oisst_dates <- OISST_dates_index(OISST_daily_nc_files)
date_max <- max(oisst_dates$final_dates)

# Builds all 1440 files in one pass over the daily archive. Safe to re-run if
# interrupted - already-complete years are skipped and OISST_database_verify()
# runs at the end to repair any longitude left short by a stalled worker
print(paste0("Began building 1440 OISST lon files at ", Sys.time()))
OISST_database_build(date_max = date_max)
print(paste0("Finished building 1440 OISST lon files at ", Sys.time()))


# 4: Build climatology (thresh) files for both baselines ---------------------

# create_thresh() reads OISST_files[lon_row]. OISST_files was set once in
# Section 1 (via metadata.R, before Section 3 wrote the 1440 ts files) and is
# now stale/empty in the main session. Restarting the worker cluster does NOT
# fix this on its own: future/furrr auto-export any global a dispatched
# function references using the *calling* (main) session's current value,
# which silently overrides whatever a freshly-started worker's own
# rscript_startup sourcing computed - confirmed by testing, not something
# visible from reading the function definitions alone. So the fix is to
# refresh OISST_files here, in the main session, before dispatching -
# whatever the workers' own state is doesn't matter once this is correct.
OISST_files <- dir("../data/OISST", pattern = "oisst-avhrr", full.names = TRUE)

print(paste0("Began 1982-2011 baseline thresh calcs at ", Sys.time()))
furrr::future_walk(1:1440, create_thresh, base_years = c(1982, 2011),
                   .options = furrr::furrr_options(seed = TRUE))

print(paste0("Began 1991-2020 baseline thresh calcs at ", Sys.time()))
furrr::future_walk(1:1440, create_thresh, base_years = c(1991, 2020),
                   .options = furrr::furrr_options(seed = TRUE))

print(paste0("Finished thresh files at ", Sys.time()))

# Shut down the parallel worker pool
future::plan(future::sequential)
parallel::stopCluster(oisst_cl)


# From here, event/category files (which also need this same 1440-longitude
# structure) are created by MHW_daily.R's own "2: Update MHW event and
# category data" section, not by this script - see the comments there.


# 5: Symlink the data folders into shiny/ ------------------------------------

# shiny/global.R expects OISST/event/thresh/cat_clim to exist as symlinks
# inside shiny/, pointing at the real data this pipeline builds under
# ../data. NB: using absolute targets here rather than the literal "../data/*"
# form global.R's own comment describes: shiny/OISST sits one directory
# deeper than ../data/OISST is from the repo root, so a literal "../data/OISST"
# target would resolve (relative to the symlink's own location inside shiny/)
# to shiny/../data/OISST, not the sibling ../data/OISST this pipeline actually
# populates. Absolute paths sidestep that nesting mismatch entirely.
data_root <- normalizePath("../data")
system(paste0("ln -sf ", data_root, "/OISST shiny/OISST"))
system(paste0("ln -sf ", data_root, "/thresh shiny/thresh"))
# NB: event/ and cat_clim/ are not created by this script (see header) - those
# two symlinks won't resolve to a real directory until MHW_daily.R has been
# run at least once and populated ../data/event and ../data/cat_clim.
# NB: -f makes this safe to re-run - it replaces a stale/incorrect symlink in
# place but (per GNU ln) refuses to clobber an existing real directory.
system(paste0("ln -sf ", data_root, "/event shiny/event"))
system(paste0("ln -sf ", data_root, "/cat_clim shiny/cat_clim"))

