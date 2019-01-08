# The purpose of this script is to provide the space in which the 
# database used by the app is created


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
library(odbc)
# library(fasttime)
source("MHW_tikoraluk.R")
doMC::registerDoMC(cores = 50)


# Load function -----------------------------------------------------------

load_sub_MHW_ALL <- function(file_name){
  load(file_name)
  
  # The category clims
  MHW_cat_clim_sub <- MHW_cat_clim(MHW_res) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           category = as.integer(category),
           lon = ifelse(lon > 180, lon-360, lon),
           intensity = round(intensity, 2))
  # MHW_cat_clim_sub <- as.tibble(MHW_cat_clim_sub)
  
  # The event metrics
  MHW_event_sub <- MHW_event(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
    dplyr::select(lon:event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  # MHW_event_sub <- as.tibble(MHW_event_sub)
  
  # The daily values # Opting for NetCDF...
  # MHW_daily_sub <- MHW_clim(MHW_res) %>%
  #   mutate(lon = ifelse(lon > 180, lon-360, lon),
  #          temp = round(temp, 1)) %>%
  #   dplyr::select(lon, lat, t, temp)
  # MHW_daily_sub <- as.tibble(MHW_daily_sub)
  
  # The thresholds
  MHW_thresh_sub <- MHW_clim(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon),
           seas = round(seas, 2),
           thresh = round(thresh, 2)) %>%
    dplyr::select(lon, lat, doy, seas, thresh) %>% 
    distinct() %>% 
    arrange(lon, lat, doy)
  # MHW_thresh_sub <- as.tibble(MHW_thresh_sub)
  
  rm(MHW_res)
  MHW_ALL_sub <- list(MHW_cat_clim_sub = MHW_cat_clim_sub, 
                      MHW_event_sub = MHW_event_sub,
                      # MHW_daily_sub = MHW_daily_sub,
                      MHW_thresh_sub = MHW_thresh_sub)
  return(MHW_ALL_sub)
  # rm(MHW_ALL_sub)
}


# Append function ---------------------------------------------------------

MHW_append <- function(file_name){
  print(paste0("Began run on ",file_name," at ",Sys.time()))
  MHW_ALL_sub <- load_sub_MHW_ALL(file_name)
  odbc::dbWriteTable(conn = MHW_DB, name = "MHW_cat_clim_sub", MHW_ALL_sub$MHW_cat_clim_sub, append = TRUE)
  odbc::dbWriteTable(conn = MHW_DB, name = "MHW_event_sub", MHW_ALL_sub$MHW_event_sub, append = TRUE)
  # odbc::dbWritetable(conn = MHW_DB, name = "MHW_daily_sub", MHW_ALL_sub$MHW_daily_sub, append = TRUE) # Opting for NetCDF...
  odbc::dbWriteTable(conn = MHW_DB, name = "MHW_thresh_sub", MHW_ALL_sub$MHW_thresh_sub, append = TRUE)
  rm(MHW_ALL_sub)
  print(paste0("Finished run on ",file_name," at ",Sys.time()))
}


# Database creation -------------------------------------------------------

# NB: This only needs to be run once to set up the database

# Start by loading only one lon slice to serve as the foundation for the databse
# system.time(
# MHW_ALL_sub <- load_sub_MHW_ALL(MHW_files[1])
# ) # 3 seconds, 220 MB, 28 MB without daily data

# Initiales database
# MHW_DB <- src_sqlite("data/MHW_DB.sqlite", create = TRUE)
# MHW_DB

# Fill it full of data
# copy_to(dest = MHW_DB, name = "MHW_cat_clim_sub", MHW_ALL_sub$MHW_cat_clim_sub, temporary = FALSE)
# copy_to(dest = MHW_DB, name = "MHW_event_sub", MHW_ALL_sub$MHW_event_sub, temporary = FALSE)
# # copy_to(dest = MHW_DB, name = "MHW_daily_sub", MHW_ALL_sub$MHW_daily_sub, temporary = FALSE) # Opting for NetCDF...
# copy_to(dest = MHW_DB, name = "MHW_thresh_sub", MHW_ALL_sub$MHW_thresh_sub, temporary = FALSE)
# 
# MHW_DB


# Append data -------------------------------------------------------------

# NB: This only needs to be run once to complete the database

# Open connection
# MHW_DB <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_DB.sqlite")
# src_dbi(MHW_DB)

# Append all of the other files
# File 1 was used to create the database so does not need to be added again
# system.time(
# MHW_append(MHW_files[2])
# ) # 3 seconds
# File 2 was added just above so is not added here either
# plyr::ldply(MHW_files[-c(1:2)], .fun = MHW_append, .parallel = TRUE)
# dbDisconnect()


# Test connection ---------------------------------------------------------

MHW_DB <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_DB.sqlite")
src_dbi(MHW_DB)

# test <- tbl(MHW_DB, "MHW_clim_sub") %>% 
#   select(lon) %>% 
#   distinct() %>% 
#   collect()

date_filter <- as.integer(as.Date("2017-06-17"))

cat_clim <- tbl(MHW_DB, "MHW_cat_clim_sub")
system.time(
cat_clim_1 <- cat_clim %>%
  filter(t == date_filter) %>%
  collect()
) # 25 seconds
head(cat_clim_1)

ggplot(cat_clim_1, aes(x = lon, y = lat, fill = category)) +
  geom_raster()

dbDisconnect()

