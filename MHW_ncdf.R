# The purpose of this script is to house the functions written to create NetCDF files
# from the MHW data, as well as to then run the functions and create the NetCDF files


# Libraries ---------------------------------------------------------------

library(tidyverse)
doMC::registerDoMC(cores = 50)
library(abind)
library(ncdf4)


# Meta-data ---------------------------------------------------------------

load("../tikoraluk/metadata/lon_lat_OISST.RData")


# Data --------------------------------------------------------------------

# Load December sub-samples
load("../MHWapp/data/MHW_cat_clim_sub.RData")
load("../MHWapp/data/MHW_event_sub.RData")
load("../MHWapp/data/MHW_clim_sub.RData")


# Functions ---------------------------------------------------------------

# # Function for creating NetCDF files from MHW data
# MHW_ncdf <- function(){}


# Define dimensions -------------------------------------------------------

# Set the dataframe in question
dataset <- MHW_cat_clim_sub %>% 
  mutate(t = as.double(t))
filename <- "test.nc"

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
tunits <- "days since 1970-01-01 00:00:00"
tvals <- seq(min(dataset$t), max(dataset$t))
nt <- length(tvals)
time_def <- ncdim_def("time", tunits, tvals, unlim = TRUE)


# Create data arrays ------------------------------------------------------

acast_OISST <- function(df, var){
  res <- df %>%
    right_join(lon_lat_OISST, by = c("lon", "lat")) %>%
    reshape2::acast(lat~lon, value.var = var)
}

dfa <- dataset %>%
  mutate(category = as.integer(category)) %>%
  group_by(t) %>%
  nest() %>%
  mutate(data2 = map(data, acast_OISST, "intensity"),
         data3 = map(data, acast_OISST, "category")) %>%
  select(-data)
# dfa2 <- array(data = dfa$data2, dimnames = list(dfa$t))

dfa_int <- abind(dfa$data2, along = 3)
dfa_cat <- abind(dfa$data3, along = 3)
# dfa3 <- acast_OISST(dataset, "intensity")


# Define variables --------------------------------------------------------

int_def <- ncvar_def(name = "intensity", units = "deg_C", 
                     dim = list(lat_def, lon_def, time_def), 
                     longname = "Degrees C above threshold",
                     missval = -999, prec = "float")
cat_def <- ncvar_def(name = "category", units = "categories", 
                     dim = list(lat_def, lon_def, time_def), 
                     longname = "Different category levels",
                     missval = -999, prec = "float")


# Create NetCDF files -----------------------------------------------------

ncout <- nc_create(filename = filename, vars = list(int_def, cat_def), force_v4 = T)


# Put variables -----------------------------------------------------------

ncvar_put(nc = ncout, varid = int_def, vals = dfa_int)
ncvar_put(nc = ncout, varid = cat_def, vals = dfa_cat)

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


# Load and visualise ------------------------------------------------------

nc <- nc_open("test.nc")

res <- ncvar_get(nc, varid = "category")[,,1]
dimnames(res) <- list(lat = nc$dim$lat$vals,
                      lon = nc$dim$lon$vals)#,
                      # t = nc$dim$time$vals)
nc_close(nc)

res2 <- as.data.frame(reshape2::melt(res, value.name = "category"), row.names = NULL) %>% 
  # mutate(t = as.Date(t, origin = "1970-01-01")) %>%
  # select(lon, everything()) %>% 
  # filter(t == as.Date("2017-12-13")) %>% 
  na.omit()

ggplot(res2, aes(x = lon, y = lat, fill = intensity)) +
  geom_raster(na.rm = T) +
  borders(fill = "black") +
  scale_fill_viridis_c()


