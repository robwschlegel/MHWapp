# The purpose of this script is to house code that will only work when run on tikoraluk


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(doMC); doMC::registerDoMC(cores = 50)
library(padr)


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

load_sub_MHW <- function(file_name){
  load(file = file_name)
  data_sub <- MHW_cat_clim(MHW_res) %>% 
    filter(t >= as.Date("2017-12-01")) %>% 
    mutate(intensity = round(intensity, 2))
  return(data_sub)
}

# Data --------------------------------------------------------------------

MHW_files <- dir(path = "../data", pattern = "MHW.calc", full.names = T)

# system.time(
# MHW_cat_clim_sub <- plyr::ldply(MHW_files, .fun = load_sub_MHW, .parallel = T)
# ) # 737 seconds at 50 cores
# save(MHW_cat_clim_sub, file = "data/MHW_cat_clim_sub.RData")

