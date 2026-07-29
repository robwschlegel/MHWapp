# MHW_database.R
# This script builds the full OISST backend from scratch on a machine that has
# none of it yet, so the project can be stood up on a fresh clone rather than
# relying on data that only ever existed on the original server.
# It is a one-time/deliberate-rerun script (not part of the daily cron job) and
# uses the functions already defined in "MHW_daily_functions.R".
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

# Directories a fresh machine may not have yet. Some of the functions below
# (OISST_url_daily_save() for the year sub-folders, create_thresh() for the
# MHW/MCS thresh outputs) assume these already exist rather than creating them.
dir.create("../data/OISST/daily", recursive = TRUE, showWarnings = FALSE)
dir.create("../data/thresh/MCS", recursive = TRUE, showWarnings = FALSE)

# Parallel worker plan: separate processes (i.e. multisession), never fork-based.
# Workers independently source "MHW_daily_functions.R"
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

# Re-set the OISST files as this was updated above
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


# From here, event/category files are created by "MHW_daily.R"


# 5: Symlink the data folders into shiny/ ------------------------------------

# shiny/global.R expects OISST/event/thresh/cat_clim to exist as symlinks inside "shiny/"
data_root <- normalizePath("../data")
system(paste0("ln -sf ", data_root, "/OISST shiny/OISST"))
system(paste0("ln -sf ", data_root, "/thresh shiny/thresh"))
# NB: event/ and cat_clim/ are not created by this script (see header) - those
# two symlinks won't resolve to a real directory until "MHW_daily.R" has been
# run at least once and populated ../data/event and ../data/cat_clim.
# NB: -f makes this safe to re-run - it replaces a stale/incorrect symlink.
system(paste0("ln -sf ", data_root, "/event shiny/event"))
system(paste0("ln -sf ", data_root, "/cat_clim shiny/cat_clim"))

