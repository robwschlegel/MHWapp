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

load_sub_MHW_cat_clim <- function(file_name){
  load(file_name)
  print(paste0("Began run on ",file_name," at ",Sys.time()))
  MHW_cat_clim_sub <- MHW_cat_clim(MHW_res) %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           category = as.integer(category),
           lon = ifelse(lon > 180, lon-360, lon),
           intensity = round(intensity, 2))
  rm(MHW_res)
  print(paste0("Finished run on ",file_name," at ",Sys.time()))
  return(MHW_cat_clim_sub)
}
# system.time(
# test <- load_sub_MHW_cat_clim(MHW_files[1])
# ) # 1 second

load_sub_MHW_event <- function(file_name){
  load(file_name)
  print(paste0("Began run on ",file_name," at ",Sys.time()))
  MHW_event_sub <- MHW_event(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
    dplyr::select(lon:event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  rm(MHW_res)
  print(paste0("Finished run on ",file_name," at ",Sys.time()))
  return(MHW_event_sub)
}
# system.time(
# test <- load_sub_MHW_event(MHW_files[1])
# ) # 1 second

load_sub_MHW_thresh <- function(file_name){
  load(file_name)
  print(paste0("Began run on ",file_name," at ",Sys.time()))
  MHW_thresh_sub <- MHW_clim(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon),
           seas = round(seas, 2),
           thresh = round(thresh, 2)) %>%
    dplyr::select(lon, lat, doy, seas, thresh) %>% 
    distinct() %>% 
    arrange(lon, lat, doy)
  rm(MHW_res)
  print(paste0("Finished run on ",file_name," at ",Sys.time()))
  return(MHW_thresh_sub)
}
# system.time(
# test <- load_sub_MHW_thresh(MHW_files[1])
# ) # 3 seconds



# Native R lon slices -----------------------------------------------------

event_lon <- function(file_name){
  file_num <- sapply(strsplit(as.character(file_name), "[.]"), "[[", 5)
  res <- load_sub_MHW_event(file_name)
  saveRDS(res, file = paste0("../data/event/MHW.event.",file_num,".Rda"))
}
# system.time(
# event_lon(MHW_files[1])
# ) # 1 second
# event_1 <- readRDS("data/MHW.event.0001.Rda")
# plyr::ldply(MHW_files, .fun = event_lon, .parallel = TRUE)


# Native R daily slices ---------------------------------------------------

# The category climatology yearly files
cat_clim_files <- c(dir(path = "data", pattern = "*MHW_cat_clim_19*", full.names = T),
                    dir(path = "data", pattern = "*MHW_cat_clim_20*", full.names = T))

# Function for saving a daily global slice
cat_clim_save <- function(df){
  cat_clim_year <- lubridate::year(unique(df$t))
  cat_clim_dir <- paste0("../data/cat_clim/",cat_clim_year)
  cat_clim_name <- paste0("cat.clim.",unique(df$t),".Rda")
  df_sub <- df %>% 
    select(-t)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
}

# Function for loading and preping the daily global slices
cat_clim_daily <- function(file_name){
  cat_clim_data <- readRDS(file_name) %>% 
    mutate(category = factor(category, labels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           t2 = as.integer(t)) %>% 
    group_by(t2) %>% 
    nest() %>% 
    mutate(proc = map(data, cat_clim_save))
}

# Process the lot of them
# plyr::ldply(cat_clim_files, .fun = cat_clim_daily, .parallel = TRUE)


# Append function ---------------------------------------------------------

# MHW_append <- function(file_name){
#   print(paste0("Began run on ",file_name," at ",Sys.time()))
#   MHW_ALL_sub <- load_sub_MHW_ALL(file_name)
#   odbc::dbWriteTable(conn = MHW_DB, name = "MHW_cat_clim_sub", MHW_ALL_sub$MHW_cat_clim_sub, append = TRUE)
#   odbc::dbWriteTable(conn = MHW_DB, name = "MHW_event_sub", MHW_ALL_sub$MHW_event_sub, append = TRUE)
#   # odbc::dbWritetable(conn = MHW_DB, name = "MHW_daily_sub", MHW_ALL_sub$MHW_daily_sub, append = TRUE) # Opting for NetCDF...
#   odbc::dbWriteTable(conn = MHW_DB, name = "MHW_thresh_sub", MHW_ALL_sub$MHW_thresh_sub, append = TRUE)
#   rm(MHW_ALL_sub)
#   print(paste0("Finished run on ",file_name," at ",Sys.time()))
# }


# Database creation -------------------------------------------------------

# NB: This only needs to be run once to set up the database

# Start by loading only one lon slice to serve as the foundation for the databse
# system.time(
# MHW_ALL_sub <- load_sub_MHW_ALL(MHW_files[1])
# ) # 3 seconds, 220 MB, 28 MB without daily data

# Run on Jan 8th, 2019
  # When created, the most recent data were 2017-12-31
# MHW_cat_clim_ALL <- plyr::ldply(MHW_files, .fun = load_sub_MHW_cat_clim, .parallel = TRUE)
# saveRDS(MHW_cat_clim_ALL, file = "data/MHW_cat_clim_ALL.Rda")
# rm(MHW_cat_clim_ALL)
# MHW_event_ALL <- plyr::ldply(MHW_files, .fun = load_sub_MHW_event, .parallel = TRUE)
# saveRDS(MHW_event_ALL, file = "data/MHW_event_ALL.Rda")
# rm(MHW_event_ALL)
# MHW_thresh_ALL <- plyr::ldply(MHW_files, .fun = load_sub_MHW_thresh, .parallel = TRUE)
# saveRDS(MHW_thresh_ALL, file = "data/MHW_thresh_ALL.Rda")
# rm(MHW_thresh_ALL)

# Initiales database
# MHW_DB <- src_sqlite("data/MHW_DB.sqlite", create = TRUE)
# MHW_DB

# Fill it full of data
# MHW_cat_clim_ALL <- readRDS("data/MHW_cat_clim_ALL.Rda")
# copy_to(dest = MHW_DB, name = "MHW_cat_clim_sub", MHW_cat_clim_ALL, temporary = FALSE)
# rm(MHW_cat_clim_ALL)
# MHW_event_ALL <- readRDS("data/MHW_event_ALL.Rda")
# copy_to(dest = MHW_DB, name = "MHW_event_sub", MHW_event_ALL, temporary = FALSE)
# rm(MHW_event_ALL)
# # copy_to(dest = MHW_DB, name = "MHW_daily_sub", MHW_ALL_sub$MHW_daily_sub, temporary = FALSE) # Opting for NetCDF...
# MHW_thresh_ALL <- readRDS("data/MHW_thresh_ALL.Rda")
# copy_to(dest = MHW_DB, name = "MHW_thresh_sub", MHW_thresh_ALL, temporary = FALSE)
# rm(MHW_event_ALL)
# MHW_cat_clim_2016 <- readRDS("data/MHW_cat_clim_2016.Rda")
# copy_to(dest = MHW_DB, name = "MHW_cat_clim_2016", MHW_cat_clim_2016, temporary = FALSE)
# rm(MHW_cat_clim_2016)

# MHW_DB
# dbDisconnect(conn = MHW_DB)

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

# MHW_DB <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_DB.sqlite")
# src_dbi(MHW_DB)

# test <- tbl(MHW_DB, "MHW_clim_sub") %>% 
#   select(lon) %>% 
#   distinct() %>% 
#   collect()

# date_filter <- as.integer(as.Date("2016-06-17"))

# cat_clim <- tbl(MHW_DB, "MHW_cat_clim_2016")
# system.time(
# cat_clim_1 <- cat_clim %>%
#   filter(t == date_filter) %>%
#   collect()
# ) # 4 seconds
# head(cat_clim_1)

# ggplot(cat_clim_1, aes(x = lon, y = lat, fill = category)) +
#   geom_raster()

# dbDisconnect()

