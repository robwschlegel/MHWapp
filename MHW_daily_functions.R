# This script houses all of the functions used in "MHW_daily.R"
# This is done to keep everything tidier and easier to read

# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
# devtools::install_github("robwschlegel/heatwaveR")
# library(jsonlite, lib.loc = "../R-packages/")
library(tidyverse)
library(rerddap)
library(tidync)
library(ncdf4)
library(abind)
library(padr)
# library(qs, lib.loc = "../R-packages/")
library(heatwaveR, lib.loc = "../R-packages/")
print(paste0("heatwaveR version = ",packageDescription("heatwaveR")$Version))
# doMC::registerDoMC(cores = 25)
doParallel::registerDoParallel(cores = 25)


# Prep functions ----------------------------------------------------------

# Tester...
# load("../data/MHW/MHW.calc.0001.RData")

# Pull out climatologies
MHW_clim <- function(df){
  clim <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event)# %>% 
  # select(-(threshCriterion:event))
}
# test <- MHW_clim(MHW_res)

# Pull out events
MHW_event <- function(df){
  event <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event)
}
# test <- MHW_event(MHW_res)

# Pull out category climatologies
MHW_cat_clim <- function(df, long = FALSE){
  cat_clim <- df %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cat)
  if(long){
    cat_clim_long <- cat_clim %>% 
      group_by(lon, lat) %>%
      nest() %>%
      mutate(long = map(data, pad, interval = "day", 
                        start_val = as.Date("1982-01-01"))) %>% 
      dplyr::select(-data) %>%
      unnest()
  } else {
    return(cat_clim)
  }
}
# test <- MHW_cat_clim(MHW_res)
# test <- MHW_cat_clim(MHW_res, long = T)

# Pull out event category summaries
MHW_cat_event <- function(df){
  suppressWarnings(
    cat_event <- df %>% 
      unnest(cat) %>% 
      filter(row_number() %% 2 == 0) %>% 
      unnest(cat)
  )
}
# test <- MHW_cat_event(MHW_res)


# Meta-data ---------------------------------------------------------------

lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)
lon_lat_OISST <- base::expand.grid(lon_OISST, lat_OISST) %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>% 
  arrange(lon, lat) %>% 
  data.frame()

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MHW_event_files <- dir("../data/event", pattern = "MHW.event.", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)
cat_lon_files <- dir("../data/cat_lon", full.names = T)
cat_clim_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim", 
                                   full.names = TRUE, recursive = TRUE))

# Date range of already processed data
## NB: This is currently static but must be self-updating to work correctly
### A self updating file that grabs dates from somewhere...
# load("../MHWapp/shiny/current_dates.RData")
# head(current_dates)
# tail(current_dates)
# current_dates <- seq(as.Date("1982-01-01"), as.Date("2018-12-31"), by = "day")

# The current date
  # NB: This script is running from a server in Atlantic Canada (UTC-3)
current_date <- Sys.Date()

# Wrapper function to coerce ERDDAP date format to R
NOAA_date <- function(date_string, piece){
  res <- as.Date(as.POSIXct(as.numeric(sapply(strsplit(as.character(date_string), ", "), "[[", piece)),
                            origin = "1970-01-01 00:00:00"))
}


# 1: Update OISST data functions ------------------------------------------

# This downloads the data
OISST_dl <- function(times, product){
  oisst_res <- griddap(x = product, 
                       url = "https://www.ncei.noaa.gov/erddap/", 
                       time = times, 
                       depth = c(0, 0),
                       latitude = c(-89.875, 89.875),
                       longitude = c(0.125, 359.875),
                       fields = "sst")$data %>% 
    dplyr::rename(temp = sst, t = time) %>% 
    mutate(temp = round(temp, 2),
           lon = ifelse(lon > 180, lon-360, lon),
           t = as.Date(str_remove(t, "T00:00:00Z"))) %>%
    select(lon, lat, t, temp) %>% 
    na.omit()
}

# Function for creating arrays from data.frames
# df <- OISST_step_2
OISST_acast <- function(df){
  # Ensurecorrect grid size
  lon_lat_OISST_sub <- lon_lat_OISST %>% 
    filter(lon == df$lon[1])
  # Round data for massive file size reduction
  df$temp <- round(df$temp, 2)
  # Force grid
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat"))
  # Create array
  res_array <- base::array(res$temp, dim = c(720,1,1))
  dimnames(res_array) <- list(lat = lon_lat_OISST_sub$lat,
                              lon = unique(lon_lat_OISST_sub$lon),
                              t = unique(na.omit(res$t)))
  return(res_array)
}

# Wrapper function that serves as the last step before
# data are entered into the NetCDF files
# df <- OISST_prelim_sub
OISST_temp <- function(df){
  # Filter NA and convert dates to integer
  OISST_step <- df %>% 
    mutate(temp = ifelse(is.na(temp), NA, temp),
           t = as.integer(t)) %>% 
    na.omit()
  
  # Acast
  dfa <- OISST_step %>%
    mutate(t2 = t) %>% 
    group_by(t2) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, OISST_acast)) %>%
    select(-data)
  
  # Final form
  dfa_temp <- abind(dfa$data2, along = 3, hier.names = T)
  # dimnames(dfa_temp)
  return(dfa_temp)
}

# Function for merging OISST data into existing NetCDF files
# tester...
# lon_step <- lon_OISST[1]
# df_final <- OISST_final_1
# df_prelim <- OISST_prelim_1
OISST_merge <- function(lon_step, df_prelim, df_final){
  
  ### Determine the correct lon slice/file
  # Determine lon slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # print(paste0("Began run on avhrr-only-v2.ts.",lon_row_pad,".nc at ",Sys.time()))
  
  # Determine file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  # tester...
  # ncdf_file_name <- paste0("proc/test/test-only-v2.ts.",lon_row_pad,".nc")
  #
  
  ### Open NetCDF and determine dates present
  nc <- nc_open(ncdf_file_name, write = T)
  
  time_vals <- as.Date(nc$dim$time$vals, origin = "1970-01-01")
  # tail(time_vals)
  
  ### Grab the lon slice intended for the chosen NetCDF file
  OISST_prelim_sub <- df_prelim %>% 
    filter(lon  == lon_step,
           as.integer(t) > max(time_vals))
  OISST_final_sub <- df_final %>% 
    filter(lon  == lon_step)
  
  ### Check again for errors in the data
  if(nrow(OISST_final_sub) > 1){
    if(max(OISST_final_sub$temp, na.rm = T) > 100){
      stop(paste0("There are errors in the final OISST data for ",ncdf_file_name))
    }
  }
  if(nrow(OISST_prelim_sub) > 1){
    if(max(OISST_prelim_sub$temp, na.rm = T) > 100){
      stop(paste0("There are errors in the prelim OISST data for ",ncdf_file_name))
    }
  }
  
  ### Create data arrays and insert into NetCDF file
  if(nrow(OISST_prelim_sub) > 0){
    
    prelim_temp <- OISST_temp(OISST_prelim_sub)

    for(i in 1:length(prelim_temp[1,1,])){
      ncvar_put(nc = nc, varid = "sst", vals = prelim_temp[,,i], verbose = FALSE,
                start = c(1,1,(length(nc$dim$time$vals)+i)), count = c(720,1,1))
      ncvar_put(nc = nc, varid = "time", vals = as.integer(dimnames(prelim_temp)[[3]])[i],
                start = (length(nc$dim$time$vals)+i), verbose = FALSE)
      nc_sync(nc)
    }
  }
  
  if(nrow(OISST_final_sub) > 0){
    
    final_temp <- OISST_temp(OISST_final_sub)
    
    for(i in 1:length(final_temp[1,1,])){
      date_put <- which(nc$dim$time$vals == as.integer(dimnames(final_temp)[[3]])[i])
      ncvar_put(nc = nc, varid = "sst", vals = final_temp[,,i], verbose = FALSE,
                start = c(1,1,date_put), count = c(720,1,1))
      nc_sync(nc)
    }
  }
  # sst <- ncvar_get(nc, "sst")
  # tail(as.Date(nc$dim$time$vals, origin = "1970-01-01"))
  
  ### Close file and exit
  nc_close(nc)
  # print(paste0("Finished run on avhrr-only-v2.ts.",lon_row_pad,".nc at ",Sys.time()))
}


# 2: Update MHW event and category data functions -------------------------

# Function that loads and merges sst/seas/thresh for a given lon_step
# lon_step <- lon_OISST[2]
# start_date <- as.Date("2018-01-01")
# end_date <- as.Date("2018-12-31")
sst_seas_thresh_merge <- function(lon_step, start_date){
  
  # Establish lon row number
  lon_row <- which(lon_OISST == lon_step)
  
  # OISST data
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_filter(time = between(time, as.integer(start_date), as.integer(Sys.Date()))) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(ts_x = time, ts_y = sst) %>%
    group_by(lon, lat) %>% 
    group_modify(~heatwaveR:::make_whole_fast(.x)) %>% # This is necessary for the doy column
    ungroup() %>% 
    dplyr::rename(t = ts_x, temp = ts_y) %>%
    select(lon, lat, t, doy, temp)
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST %>% 
    left_join(hyper_tibble(tidync(seas_thresh_files[lon_row])), 
              by = c("lon", "lat", "doy" = "time"))
  return(sst_seas_thresh)
}

# Function for updating the MHW event metric lon slice files
# tester...
# lon_step <- lon_OISST[1]
MHW_event_cat_update <- function(lon_step){
  
  # load the final download date
  load("metadata/final_dates.Rdata")
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Load current lon slice for event/category
  MHW_event_data <- na.omit(readRDS(MHW_event_files[lon_row]))
  if(MHW_event_data$lon[1] != lon_step) stop(paste0("The lon_row indexing has broken down somewhere for ",lon_row_pad))
  MHW_cat_lon <- na.omit(readRDS(cat_lon_files[lon_row]))
  if(MHW_cat_lon$lon[1] != lon_step) stop(paste0("The lon_row indexing has broken down somewhere for ",lon_row_pad))
  
  # Begin the calculations
  # print(paste0("Began run on ",MHW_event_files[lon_row]," at ",Sys.time()))
  
  # Determine how far back in time to get old data based on the occurrence of previous MHWs
  # The problem here is that sometimes the prelim data will dip below the threshold long enough
  # that the MHW will be considered finished and the code would incorrectly begin calculating another MHW
  # To correct for this we must use the final_dates R object, which is tied to the OISST downloading
  # We then go back one event before the final data end date to ensure we are not missing anything
  previous_event_index <- MHW_event_data %>% 
    group_by(lon, lat) %>% 
    filter(date_start < max(final_dates)) %>% 
    filter(event_no == max(event_no)-2)
  # test_index <- MHW_event_data %>% 
  #   filter(lat == -51.875)
  
  # Extract each pixel time series based on how far back the oldest event occurred for the entire longitude slice
  sst_seas_thresh <- sst_seas_thresh_merge(lon_step, 
                                           start_date = min(previous_event_index$date_start))
                                           # Use to recalculate everything
                                           # start_date = as.Date("1982-01-01"))
  
  # Calculate new event metrics with new data as necessary
  # system.time(
  MHW_event_cat <- previous_event_index %>% 
    mutate(lat2 = lat) %>% 
    group_by(lat2) %>% 
    nest() %>% 
    mutate(event_cat_res = map(data, event_calc,
                               sst_seas_thresh = sst_seas_thresh,
                               MHW_event_data = MHW_event_data,
                               MHW_cat_lon = MHW_cat_lon)) %>% 
    ungroup() %>% 
    select(-data, -lat2) %>%
    unnest(cols = event_cat_res)
  # ) # ~28 seconds for 478 pixels
  
  # Save results and exit
  MHW_event_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cols = event_cat_res)
  saveRDS(MHW_event_new, file = MHW_event_files[lon_row])
  # tester...
  # MHW_event_new_test <- MHW_event_new %>% 
  #   filter(lat == df$lat)
  # saveRDS(MHW_event_new, dir("../data/test", pattern = "MHW.event", full.names = T)[lon_row])
  
  MHW_cat_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cols = event_cat_res)
  saveRDS(MHW_cat_new, file = cat_lon_files[lon_row])
  # tester...
  # MHW_cat_new_test <- MHW_cat_new %>% 
  #   filter(lat == df$lat)
  # saveRDS(MHW_cat_new, dir("../data/test", pattern = "MHW.cat", full.names = T)[lon_row])
  
  # print(paste0("Finished run on MHW.event.",lon_row_pad,".Rda at ",Sys.time()))
}

# Function for extracting correct sst data based on pre-determined subsets
# It also calculates and returns corrected MHW metric results
# df <- final_event_index[319,]
event_calc <- function(df, sst_seas_thresh, MHW_event_data, MHW_cat_lon){
  
  # Extract necessary SST
  sst_step_1 <- sst_seas_thresh %>% 
    filter(lat == df$lat,
           t >= df$date_end)
  
  # Calculate events
  event_base <- detect_event(sst_step_1)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon, lat = df$lat) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  event_step_2 <- MHW_event_data %>%
    filter(lat == df$lat,
           date_end <= df$date_end) %>%
    rbind(event_step_1) %>%
    mutate(event_no = seq_len(n()))
  
  # Calculate categories
  cat_step_1 <- category(event_base, climatology = T)$climatology %>% 
    mutate(event_no = event_no + df$event_no,
           lon = df$lon,
           lat = df$lat) %>% 
    select(t, lon, lat, event_no, intensity, category)
  cat_step_2 <- MHW_cat_lon %>%
    filter(lat == df$lat,
           t <= df$date_end) %>%
    rbind(cat_step_1)
  
  # Exit
  event_cat <- list(event = event_step_2,
                    cat = cat_step_2)
  return(event_cat)
}


# 3: Create daily global file functions -----------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- cat_lon_files[1118]
# date_choice <- max(current_dates)+1
# date_choice <- min(update_dates)
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-01-31"))
load_sub_cat_clim <- function(cat_lon_file, date_range){
  # cat_clim <- qs::qread(cat_lon_file)
  cat_clim <- readRDS(cat_lon_file)
  cat_clim_sub <- cat_clim %>%
    filter(t >= date_range[1], t <= date_range[2])
  rm(cat_clim)
  return(cat_clim_sub)
}

save_sub_cat_clim <- function(date_choice, df){
  # Establish flie name and save location
  cat_clim_year <- lubridate::year(date_choice)
  cat_clim_dir <- paste0("../data/cat_clim/",cat_clim_year)
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  cat_clim_name <- paste0("cat.clim.",date_choice,".Rda")
  # Extract data and save
  df_sub <- df %>% 
    filter(t == date_choice)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
  print(paste0("Finished creating ", date_choice," slice at ",Sys.time()))
}

# Function for loading, prepping, and saving the daily global category slices
# tester...
# date_choice <- max(current_dates)+1
# date_choice <- as.Date("2019-02-10")
# date_range <- c(as.Date("1984-01-01"), as.Date("1986-01-31"))
cat_clim_global_daily <- function(date_range){
  # print(paste0("Began creating ", date_choice," slice at ",Sys.time()))
  # tester...
  # cat_clim_daily <- plyr::ldply(dir("../data/test/", pattern = "MHW.cat", full.names = T), 
  cat_clim_daily <- plyr::ldply(cat_lon_files,
                                load_sub_cat_clim,
                                .parallel = T, date_range = date_range) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")))
  
  # NB: Running this in parallel causes serious RAM issues
  doParallel::registerDoParallel(cores = 10)
  plyr::l_ply(seq(min(cat_clim_daily$t), max(cat_clim_daily$t), by = "day"), 
              save_sub_cat_clim, .parallel = T, df = cat_clim_daily)
}

