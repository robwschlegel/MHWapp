# The purpose of this script is to provide the space in which the 
# database used by the app is created


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
# library(fasttime)
source("MHW_tikoraluk.R")


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
  MHW_cat_clim_sub <- as.tibble(MHW_cat_clim_sub)
  
  # The event metrics
  MHW_event_sub <- MHW_event(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
    dplyr::select(lon:event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  MHW_event_sub <- as.tibble(MHW_event_sub)
  
  # The clims
  MHW_clim_sub <- MHW_clim(MHW_res) %>%
    mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
    dplyr::select(lon:thresh, event_no) %>%
    mutate_all(round, 3)
  MHW_clim_sub <- as.tibble(MHW_clim_sub)
  
  rm(MHW_res)
  MHW_ALL_sub <- list(MHW_cat_clim_sub = MHW_cat_clim_sub, 
                      MHW_event_sub = MHW_event_sub,
                      MHW_clim_sub = MHW_clim_sub)
  return(MHW_ALL_sub)
  rm(MHW_ALL_sub)
}


# Append function ---------------------------------------------------------

MHW_append <- function(file_name){
  MHW_ALL_sub <- load_sub_MHW_ALL(file_name)
  dbAppendTable(conn = MHW_DB, name = "MHW_cat_clim_sub", MHW_ALL_sub$MHW_cat_clim_sub)
  dbAppendTable(conn = MHW_DB, name = "MHW_event_sub", MHW_ALL_sub$MHW_event_sub)
  dbAppendTable(conn = MHW_DB, name = "MHW_clim_sub", MHW_ALL_sub$MHW_clim_sub)
  rm(MHW_ALL_sub)
}


# Data --------------------------------------------------------------------

# NB: This only needs to be run once to set up the database

# Start by loading only one lon slice to serve as the foundation for the databse
# MHW_ALL_sub <- load_sub_MHW_ALL(MHW_files[1])


# Database creation -------------------------------------------------------

# NB: This only needs to be run once to set up the database

# Initiales database
# MHW_db <- src_sqlite("data/MHW_db.sqlite", create = TRUE)
# MHW_db

# Fill it full of data
# copy_to(dest = MHW_db, df = MHW_ALL_sub$MHW_cat_clim_sub, 
#         name = "MHW_cat_clim_sub", temporary = FALSE)
# copy_to(dest = MHW_db,  df = MHW_ALL_sub$MHW_event_sub,
#         name = "MHW_event_sub", temporary = FALSE)
# copy_to(dest = MHW_db,  df = MHW_ALL_sub$MHW_clim_sub,
#         name = "MHW_clim_sub", temporary = FALSE)
# MHW_db


# Append data -------------------------------------------------------------

# Open connection
# MHW_DB <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_db.sqlite")
# src_dbi(MHW_DB)

# Append all of the other files
# File 1 was used to create the database so does not need to be added again
# for(i in 2:length(MHW_files)){
#   MHW_append(MHW_files[i])
# }
# 
# dbDisconnect()

# Test connection ---------------------------------------------------------

MHW_DB <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_db.sqlite")
src_dbi(MHW_DB)

date_filter <- as.integer(as.Date("2017-06-17"))

cat_clim <- tbl(MHW_DB, "MHW_cat_clim_sub")
cat_clim_1 <- cat_clim %>%
  filter(t == date_filter) %>%
  collect()
head(cat_clim_1)

ggplot(cat_clim_1, aes(x = lon, y = lat, fill = category)) +
  geom_raster()

dbDisconnect()

