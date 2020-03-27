# MHW_tikoraluk.R
# The purpose of this script is to house code that will only work when run  
# while ssh'd into the tikoraluk server


# Libraries ---------------------------------------------------------------

library(tidyverse)
# library(padr)
doMC::registerDoMC(cores = 50)


# Meta-data ---------------------------------------------------------------

MHW_files <- dir(path = "../data", pattern = "MHW.calc", full.names = T)


# Prep functions ----------------------------------------------------------

# Pull out climatologies
MHW_clim <- function(df){
  clim <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event)
}

# Pull out events
MHW_event <- function(df){
  event <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event)
}

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

# Pull out event category summaries
MHW_cat_event <- function(df){
  suppressWarnings(
    cat_event <- df %>% 
      unnest(cat) %>% 
      filter(row_number() %% 2 == 0) %>% 
      unnest(cat)
  )
}


# Load functions ----------------------------------------------------------

# Load and subset category climatologies
load_sub_MHW_cat_clim <- function(file_name, sub_year){
  load(file = file_name)
  data_sub <- MHW_cat_clim(MHW_res) %>% 
    filter(t >= as.Date(paste0(sub_year,"-01-01")),
           t <= as.Date(paste0(sub_year,"-12-31"))) %>% 
    mutate(intensity = round(intensity, 2))
  rm(MHW_res)
  return(data_sub)
} # 0.8 seconds for one

# Load and subset event metrics
load_sub_MHW_event <- function(file_name, sub_year){
  load(file = file_name)
  data_sub <- MHW_event(MHW_res) %>% 
    filter(date_start >= as.Date(paste0(sub_year,"-01-01")),
           date_start <= as.Date(paste0(sub_year,"-12-31")))
  rm(MHW_res)
  return(data_sub)
} # 1.8 seconds for one

# Load and subset climatologies # Opting for NetCDF files...
# load_sub_MHW_clim <- function(file_name, sub_year){
#   load(file = file_name)
#   data_sub <- MHW_clim(MHW_res) %>%
#     filter(t >= as.Date(paste0(sub_year,"-01-01")),
#            t <= as.Date(paste0(sub_year,"-12-01")))
#   rm(MHW_res)
#   return(data_sub)
# } # 1.8 seconds for one


# Processing functions ----------------------------------------------------

# Process annual category climatologies
proc_sub_MHW_cat_clim <- function(sub_year){
  print(paste0("Began run on ",sub_year," at ",Sys.time()))
  MHW_cat_clim_sub <- map_dfr(MHW_files, load_sub_MHW_cat_clim, sub_year)
  # MHW_cat_clim_sub <- plyr::ldply(MHW_files, .fun = load_sub_MHW_cat_clim, 
                                  # .parallel = T, sub_year)
  MHW_cat_clim_sub <- MHW_cat_clim_sub %>%
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           # category = as.integer(category),
           lon = ifelse(lon > 180, lon-360, lon),
           intensity = round(intensity, 2))
  # MHW_cat_clim_sub <- as.tibble(MHW_cat_clim_sub)
  saveRDS(MHW_cat_clim_sub, file = paste0("data/MHW_cat_clim_",sub_year,".Rda"))
  rm(MHW_cat_clim_sub)
  print(paste0("Finished run on ",sub_year," at ",Sys.time()))
}

# Process annual event metrics
proc_sub_MHW_event <- function(sub_year){
  print(paste0("Began run on ",sub_year," at ",Sys.time()))
  MHW_event_sub <- map_dfr(MHW_files, load_sub_MHW_event, sub_year)
  # MHW_event_sub <- plyr::ldply(MHW_files, .fun = load_sub_MHW_event, 
  #                              .parallel = T, sub_year)
  MHW_event_sub <- MHW_event_sub %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
    dplyr::select(lon:event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  # MHW_event_sub <- as.tibble(MHW_event_sub)
  saveRDS(MHW_event_sub, file = paste0("data/MHW_event_",sub_year,".Rda"))
  rm(MHW_event_sub)
  print(paste0("Finished run on ",sub_year," at ",Sys.time()))
}

# Process threshold values # Opting for NetCDf files...
# proc_sub_MHW_thresh <- function(sub_year){
#   MHW_clim_sub <- plyr::ldply(MHW_files, .fun = load_sub_MHW_clim, 
#                               .parallel = T, sub_year)
#   MHW_clim_sub <- MHW_clim_sub %>%
#     mutate(lon = ifelse(lon > 180, lon-360, lon),
#            seas = round(seas, 2),
#            thresh = round(thresh, 2)) %>%
#     dplyr::select(lon, lat, doy, seas, thresh) %>% 
#     distinct() %>% 
#     arrange(lon, lat, doy)
#   MHW_clim_sub <- as.tibble(MHW_clim_sub)
#   saveRDS(MHW_clim_sub, file = paste0("data/MHW_clim_",sub_year,".Rda"))
#   rm(MHW_clim_sub)
# }

# Process annual climatologies # Opting for NetCDf files...
# proc_sub_MHW_clim <- function(sub_year){
#   MHW_clim_sub <- plyr::ldply(MHW_files, .fun = load_sub_MHW_clim, 
#                               .parallel = T, sub_year)
#   MHW_clim_sub <- MHW_clim_sub %>%
#     mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#     # group_by(lon, lat) %>%
#     #mutate(anom = temp - mean(temp, na.rm = T)) %>%
#     dplyr::select(lon:thresh, event_no) %>%
#     # ungroup() %>%
#     mutate_all(round, 3)
#   MHW_clim_sub <- as.tibble(MHW_clim_sub)
#   saveRDS(MHW_clim_sub, file = paste0("data/MHW_clim_",sub_year,".Rda"))
#   rm(MHW_clim_sub)
# }


# Create files ------------------------------------------------------------

# NB: Only needed to run once
# NB: Can't run all years at once... uses too much memory

# Category climatologies
# plyr::ldply(1982:1987, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)
# plyr::ldply(1988:1993, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)
# plyr::ldply(1994:1999, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)
# plyr::ldply(2000:2005, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)
# plyr::ldply(2006:2011, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)
# plyr::ldply(2012:2017, .fun = proc_sub_MHW_cat_clim, .parallel = TRUE)

# Event metrics
# plyr::ldply(1982:1987, .fun = proc_sub_MHW_event, .parallel = TRUE) # Already run
# plyr::ldply(1988:1993, .fun = proc_sub_MHW_event, .parallel = TRUE)
# plyr::ldply(1994:1999, .fun = proc_sub_MHW_event, .parallel = TRUE)
# plyr::ldply(2000:2005, .fun = proc_sub_MHW_event, .parallel = TRUE)
# plyr::ldply(2006:2011, .fun = proc_sub_MHW_event, .parallel = TRUE)
# plyr::ldply(2012:2017, .fun = proc_sub_MHW_event, .parallel = TRUE)


# Sub-samples -------------------------------------------------------------

# load("../tikoraluk/metadata/lon_OISST.RData")

# The rates of onset and decline are not calculated correctly when the start/end date 
# is the same as the peak date.
# This was found at the following coordinates:

## Rate onset issue: 
## lon = 80.625, lat = 73.875, event_no = 93, duration = 9,
## date_start = 2017-11-02, date_peak = 2017-11-02, date_end = 2017-11-10,
## intensity_mean = 0.398, intensity_max = 0.481, rate_onset = -0.021, rate_decline = 0.023
# load(MHW_files[which(lon_OISST == 80.625)])
# rate_onset_issue <- MHW_clim(MHW_res) %>% 
#   filter(lat == 73.875) %>% 
#   select(lon, lat, t, temp)
# write_csv(rate_onset_issue, path = "../tikoraluk/issues/rate_onset_issue.csv")

## Rate decline issue:
## lon = -170.125, lat = 67.625, event_no = 81, duration = 6,
## date_start = 2017-04-21, date_peak = 2017-04-26, date_end = 2017-04-26,
## intensity_mean = 0.420, intensity_max = 0.465, rate_onset = 0.042, rate_decline = -0.041
# load(MHW_files[which(lon_OISST == -170.125+360)])
# rate_decline_issue <- MHW_clim(MHW_res) %>% 
#   filter(lat == 67.625) %>% 
#   select(lon, lat, t, temp)
# write_csv(rate_decline_issue, path = "../tikoraluk/issues/rate_decline_issue.csv")

