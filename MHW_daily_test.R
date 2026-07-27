# MHW_daily_test.R
# This script houses code used to test the main steps in MHW_daily.R 
## 1: Sets up the environment
## 2: Testing the downloaded NOAA data
## 3: Testing the MHW event and cat production
## 4: Testing global files

source("MHW_daily_functions.R")
# NB: sst_seas_thresh_ts(), used in Section 3, lives in shiny/functions.R -
# sourced here too so it's available up front, same as MHW_daily_functions.R
source("shiny/functions.R")


# 1: Setup ----------------------------------------------------------------

# Parallel worker plan: separate processes (multisession), never fork-based
# (doParallel/plyr .parallel). Forking around ncdf4/HDF5 hangs unpredictably,
# see the fork/HDF5-hang notes in MHW_daily_functions.R
# NB: tear down any workers/plan left over from a prior source("MHW_daily.R")
# or source("MHW_database.R") run in this same R session before starting a
# fresh cluster below, otherwise the old cluster's worker processes are
# orphaned rather than reused

future::plan(future::sequential)
if(exists("oisst_cl")) parallel::stopCluster(oisst_cl)

n_workers <- max(1, floor(parallel::detectCores()/2))
oisst_cwd <- getwd()
oisst_test <- parallelly::makeClusterPSOCK(
  n_workers, rscript_libs = .libPaths(),
  rscript_startup = bquote({setwd(.(oisst_cwd)); source("MHW_daily_functions.R"); source("shiny/functions.R")})
)
future::plan(future::cluster, workers = oisst_test)


# 2: Testing the downloaded NOAA data -------------------------------------

# Front nub
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# Download a day of data and test it directly
OISST_test <- furrr::future_map_dfr(paste0(OISST_url_month, "202006/oisst-avhrr-v02r01.20200601.nc"),
                                    OISST_url_daily_dl)
OISST_test$lon <- ifelse(OISST_test$lon > 180, OISST_test$lon-360, OISST_test$lon)

# test visual
ggplot(data = OISST_test, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = temp))

# Function for extracting one day of data
extract_OISST_one <- function(index_val, date_val){
  file_name <- OISST_files[index_val]
  date_int <- as.integer(date_val)
  nc <- nc_open(file_name)
  time_idx <- which(as.integer(nc$dim$time$vals) == date_int)
  lon_val <- as.numeric(nc$dim$lon$vals)
  lat_vals <- as.numeric(nc$dim$lat$vals)
  sst_vals <- as.numeric(ncvar_get(nc, "sst", start = c(1, 1, time_idx), count = c(-1, -1, 1)))
  nc_close(nc)
  data.frame(lon = lon_val, lat = lat_vals, time = date_val, sst = sst_vals)
}

# Load every pixel for a chosen day
OISST_test <- furrr::future_map_dfr(1:1440, extract_OISST_one,
                                    date_val = as.Date("2020-06-21"),
                                    .options = furrr::furrr_options(seed = TRUE))

# test visual
ggplot(data = OISST_test, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = sst))

# test visuals for data downloaded via MHW_daily.r script
ggplot(data = filter(OISST_dat, t == "2020-06-20"), aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = temp))

# test the lon lat grid
ggplot(data = lon_lat_OISST, aes(x = lon, y = lat)) +
  geom_tile(fill = "red") +
  borders(fill = "grey70", colour = "black")


# 3: Testing the MHW event and cat production -----------------------------

# Set the current_dates as desired
# current_dates <- seq(as.Date("1982-01-01"), as.Date("2017-12-31"), by = "day")

chosen_sub <- 1
chosen_lat <- -5.125

## test a single run
# MHW_event_cat_update(lon_OISST[chosen_sub], current_dates = current_dates)
# MHW_load_proc_save(lon_OISST[chosen_sub])

## Load sst/seas/thresh
# NB: sst_seas_thresh_ts() (sourced from shiny/functions.R in Section 1) does
# this same sst/seas/thresh merge via raw ncdf4 (no tidync dependency), so
# it's reused here rather than duplicating the logic in the analysis scripts
sst_seas_thresh_sub <- sst_seas_thresh_ts(lon_step = lon_OISST[chosen_sub],
                                          lat_step = chosen_lat,
                                          base_years = "1982-2011")

## Load events
MHW_event_data <- readRDS(MHW_event_files[chosen_sub]) |>
  filter(lat == chosen_lat)#,
         # date_start >= "2018-01-01")

## Load a daily slice that should have a MHW
MHW_cat_data <- readRDS(cat_clim_files[which(as.Date("2018-10-15") == seq(as.Date("1982-01-01"),
                                            as.Date("2018-12-31"), by = "day"))]) |>
  filter(lat == chosen_lat,
         lon == lon_OISST[chosen_sub])

## Load a cat lon slice
MHW_cat_lon <- readRDS(cat_lon_files[chosen_sub])

## Visualise
p <- ggplot(data = sst_seas_thresh_sub, aes(x = t, y = temp)) +
  geom_flame3(aes(y2 = thresh)) +
  geom_line(colour = "grey20",
            aes(group = 1, text = paste0("Date: ",t,
                                         "<br>Temperature: ",temp,"°C"))) +
  geom_line(linetype = "dashed", colour = "steelblue3",
            aes(x = t, y = seas, group = 1,
                text = paste0("Date: ",t,
                              "<br>Climatology: ",seas,"°C"))) +
  geom_line(linetype = "dotted", colour = "tomato3",
            aes(x = t, y = thresh, group = 1,
                text = paste0("Date: ",t,
                              "<br>Threshold: ",thresh,"°C"))) +
  labs(x = "", y = "Temperature (°C)") +
  scale_x_date(expand = c(0, 0))
if(length(MHW_event_data$date_start) > 0){
  p <- p + geom_rug(data = MHW_event_data, sides = "b", colour = "red3", size = 2,
                    aes(x = date_peak, y = min(sst_seas_thresh_sub$temp),
                        text = paste0("Event: ",event_no,
                                      "<br>Duration: ",duration," days",
                                      "<br>Start Date: ", date_start,
                                      "<br>Peak Date: ", date_peak,
                                      "<br>End Date: ", date_end,
                                      "<br>Mean Intensity: ",intensity_mean,"°C",
                                      "<br>Max. Intensity: ",intensity_max,"°C",
                                      "<br>Cum. Intensity: ",intensity_cumulative,"°C")))

}
p


# 4: Testing global files -------------------------------------------------

# Load a single file
MHW_cat_clim <- readRDS("shiny/cat_clim/2016/cat.clim.2016-01-01.Rda")

# Crude global plot
ggplot(data = MHW_cat_clim, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = F)

# Shut down the parallel worker pool started in Section 1
future::plan(future::sequential)
parallel::stopCluster(oisst_test)

