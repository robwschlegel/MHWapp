# The purpose of this script is to house the functions written to create NetCDF files
# from the MHW data, as well as to then run the functions and create the NetCDF files


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(abind)
library(ncdf4)
library(R.matlab)
doMC::registerDoMC(cores = 50)


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")
load("../tikoraluk/metadata/lon_OISST.RData")
lon_lat_OISST <- arrange(lon_lat_OISST, lon, lat)
OISST_files <- dir("../../oliver/data/sst/noaa_oi_v2/avhrr/timeseries",
                   pattern = "avhrr-only", full.names = T)

# Data --------------------------------------------------------------------

# Load December sub-samples
# load("../MHWapp/data/MHW_cat_clim_sub.RData")
# load("../MHWapp/data/MHW_event_sub.RData")
# load("../MHWapp/data/MHW_clim_sub.RData")


# Functions ---------------------------------------------------------------

# tester...
# file_name <- OISST_files[1]
# Function for loading OISST data from matlab format
OISST_mat <- function(file_name){
  file_num <- as.integer(sapply(strsplit(as.character(file_name), "[.]"), "[[", 7))
  mat_file <- readMat(file_name) # ~7 seconds
  mat_file_ts <- as.data.frame(t(mat_file$sst.ts)) %>% 
    setNames(., as.numeric(mat_file$lat)) %>% 
    mutate(t = as.Date(as.POSIXct((mat_file$time - 719529) * 86400,
                                  origin = "1970-01-01", tz = "UTC"))) %>% 
    gather(-t, key = lat, value = temp) %>% 
    mutate(lon = mat_file$lon[as.numeric(file_num)],
           lat = as.numeric(lat),
           temp = ifelse(is.nan(temp), NA, temp)) %>%
    select(lon, lat, t, temp) %>% 
    na.omit() # ~2 seconds
}

# Function for creating arrays from data.frames
OISST_acast <- function(df, var, lon_sub = T){
  if(lon_sub){
    lon_lat_OISST_sub <- lon_lat_OISST %>% 
      filter(lon == df$lon[1])
  } else {
    lon_lat_OISST_sub <- lon_lat_OISST
  }
  if("temp" %in% colnames(df)){
    df$temp <- round(df$temp, 2)
  }
  res <- df %>%
    right_join(lon_lat_OISST_sub, by = c("lon", "lat")) %>%
    reshape2::acast(lat~lon, value.var = var)
}

#tester...
# system.time(
# df <- OISST_mat(OISST_files[100])
# ) # 13 seconds
# Function for creating NetCDF files from OISST data
OISST_ncdf <- function(df){
 
  # Determine lon slice
  lon_row <- which(lon_OISST == df$lon[1])
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Set file name
  ncdf_file_name <- paste0("../data/OISST/avhrr-only-v2.ts.",lon_row_pad,".nc")
  
  # Define dimensions -------------------------------------------------------
  
  # Set the dataframe in question
  dataset <- df %>% 
    mutate(t = as.integer(t))
  
  # lon
  xvals <- unique(df$lon)
  if(length(xvals) > 1) stop("Too many lon values. Should only be one.")
  # xvals_df <- data.frame(lon = lon_lat_OISST$lon)
  nx <- length(xvals)
  lon_def <- ncdim_def("lon", "degrees_east", xvals)
  
  # lat
  yvals <- unique(lon_lat_OISST$lat)
  # yvals_df <- data.frame(lat = lon_lat_OISST$lat)
  ny <- length(yvals)
  lat_def <- ncdim_def("lat", "degrees_north", yvals)
  
  # time
  tunits <- "days since 1970-01-01 00:00:00"
  tvals <- seq(min(dataset$t), max(dataset$t))
  nt <- length(tvals)
  time_def <- ncdim_def("time", tunits, tvals, unlim = TRUE)
  
  # Create data arrays ------------------------------------------------------
  
  dfa <- dataset %>%
    group_by(t) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, OISST_acast, "temp")) %>%
    select(-data)
  
  dfa_temp <- abind(dfa$data2, along = 3)
  
  # Define variables --------------------------------------------------------
  
  temp_def <- ncvar_def(name = "sst", units = "deg_C", 
                        dim = list(lat_def, lon_def, time_def), 
                        longname = "Sea Surface Temperature",
                        missval = -999, prec = "float")
  
  # Create NetCDF files -----------------------------------------------------
  
  ncout <- nc_create(filename = ncdf_file_name, vars = list(temp_def), force_v4 = T)
  
  # Put variables -----------------------------------------------------------
  
  ncvar_put(nc = ncout, varid = temp_def, vals = dfa_temp)
  
  # Additional attributes ---------------------------------------------------
  
  ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  # Global attributes -------------------------------------------------------
  
  # ncatt_put(ncout,0,"title",title$value)
  # ncatt_put(ncout,0,"institution",institution$value)
  # ncatt_put(ncout,0,"source",datasource$value)
  # ncatt_put(ncout,0,"references",references$value)
  # history <- paste("P.J. Bartlein", date(), sep=", ")
  # ncatt_put(ncout,0,"history",history)
  # ncatt_put(ncout,0,"Conventions",Conventions$value)
  
  # close the file, writing data to disk
  nc_close(ncout)
}

# Wrapper function to load mat data and save as NetCDF
OISST_proc <- function(file_name){
  print(paste0("Began run on ",file_name," at ",Sys.time()))
  res <- OISST_mat(file_name)
  OISST_ncdf(res)
  print(paste0("Finished run on ",file_name," at ",Sys.time()))
}


# Create NetCDF files -----------------------------------------------------

# system.time(
# OISST_proc(OISST_files[1])
# ) # 124 seconds

# plyr::ldply(OISST_files, .fun = OISST_proc, .parallel = TRUE)
# NB file 721 crashed and somehow prevented all other files from being written...
plyr::ldply(OISST_files[721:1440], .fun = OISST_proc, .parallel = TRUE)


# Create thresh NetCDF file -----------------------------------------------

# NB: This only needs to be run once

MHW_thresh <- readRDS("data/MHW_thresh_ALL.Rda")

# lon
xvals <- unique(lon_lat_OISST$lon)
# xvals_df <- data.frame(lon = lon_lat_OISST$lon)
nx <- length(xvals)
lon_def <- ncdim_def("lon", "degrees_east", xvals)

# lat
yvals <- unique(lon_lat_OISST$lat)
# yvals_df <- data.frame(lat = lon_lat_OISST$lat)
ny <- length(yvals)
lat_def <- ncdim_def("lat", "degrees_north", yvals)

# time
tunits <- "day of year (day)"
tvals <- seq(1:366)
nt <- length(tvals)
time_def <- ncdim_def("time", tunits, tvals, unlim = TRUE)

dfa <- MHW_thresh %>%
  group_by(doy) %>%
  nest() %>%
  mutate(data2 = purrr::map(data, OISST_acast, "seas", lon_sub = F),
         data3 = purrr::map(data, OISST_acast, "thresh", lon_sub = F)) %>%
  select(-data)

dfa_seas <- abind(dfa$data2, along = 3)
dfa_thresh <- abind(dfa$data3, along = 3)

seas_def <- ncvar_def(name = "seas", units = "deg_C", 
                      dim = list(lat_def, lon_def, time_def), 
                      longname = "Seasonal threshold",
                      missval = -999, prec = "float")

thresh_def <- ncvar_def(name = "thresh", units = "deg_C", 
                        dim = list(lat_def, lon_def, time_def), 
                        longname = "90th percentile threshold",
                        missval = -999, prec = "float")

ncout <- nc_create(filename = "../data/thresh/MHW.seas.thresh.nc", vars = list(seas_def, thresh_def), force_v4 = T)

ncvar_put(nc = ncout, varid = seas_def, vals = dfa_seas)
ncvar_put(nc = ncout, varid = thresh_def, vals = dfa_thresh)

ncatt_put(ncout,"lon","axis","X") #,verbose=FALSE) #,definemode=FALSE)
ncatt_put(ncout,"lat","axis","Y")
ncatt_put(ncout,"time","axis","T")

# close the file, writing data to disk
nc_close(ncout)


# Load and visualise ------------------------------------------------------

# nc <- nc_open("../data/OISST/avhrr-only-v2.ts.0100.nc")

# res <- ncvar_get(nc, varid = "sst")
# dimnames(res) <- list(lat = nc$dim$lat$vals,
#                       # lon = nc$dim$lon$vals,
#                       t = nc$dim$time$vals)
# nc_close(nc)

# res2 <- as.data.frame(reshape2::melt(res, value.name = "temp"), row.names = NULL) %>%
#   mutate(t = as.Date(t, origin = "1970-01-01")) %>%
#   # select(lon, everything()) %>%
#   filter(lat == -70.375) %>%
#   na.omit()

# ggplot(res2, aes(x = t, y = temp)) +
#   geom_line()

