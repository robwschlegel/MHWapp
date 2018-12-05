# The purpose of this script is to house the functions written to create NetCDF files
# from the MHW data, as well as to then run the functions and create the NetCDF files
# Largely taken from:
# http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html

# Libraries ---------------------------------------------------------------

library(tidyverse)
doMC::registerDoMC(cores = 50)
# library(padr)
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

fillvalue <- -999
tmp_array <- array(fillvalue, dim=c(nx,ny,nt))

j2 <- sapply(dataset$lon, function(x) which.min(abs(xvals-x)))
k2 <- sapply(dataset$lat, function(x) which.min(abs(yvals-x)))
nobs <- dim(dataset)[1]
l <- rep(5,each=nobs)
tmp_array[cbind(j2,k2,l)] <- as.matrix(dataset[1:nobs,5])

# dataarray <- xtabs(category ~ lon + lat + t, dataset)
# library(vcd)
# dataarray <- structable(category ~ lon + lat + t, dataset)
# 
# dfa <- array(data = dataset$category,
#              dim = c(xvals, yvals, tvals),
#              dimnames = list(xvals, yvals, tvals))


# df1 <- dataset %>% 
#   filter(t == 17501) %>% 
#   right_join(lon_lat_OISST, by = c("lon", "lat")) %>% 
#   reshape2::acast(lat~lon, value.var = "category")

# acast_OISST <- function(df, var){
#   res <- df %>% 
#     right_join(lon_lat_OISST, by = c("lon", "lat")) %>% 
#     reshape2::acast(lat~lon, value.var = var)
# }
# 
# dfa <- dataset %>% 
#   group_by(t) %>% 
#   nest() %>% 
#   mutate(data2 = map(data, acast_OISST, "intensity")) %>% 
#   select(-data)
# dfa2 <- array(data = dfa$data2, dimnames = list(dfa$t))
# 
# dfa3 <- acast_OISST(dataset, "intensity")


# Define variables --------------------------------------------------------

int_def <- ncvar_def(name = "intensity", units = "deg_C", 
                     dim = list(lat_def, lon_def, time_def), 
                     longname = "Degrees C above threshold",
                     missval = -999, prec = "float")


# Create NetCDF files -----------------------------------------------------

ncout <- nc_create(filename = filename, vars = list(int_def), force_v4 = T)


# Put variables -----------------------------------------------------------

ncvar_put(nc = ncout, varid = int_def, vals = tmp_array)


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
# nc$dim$lat
# nc$dim$lon
# nc$dim$time
# nc$var$intensity

res <- ncvar_get(nc, varid = "intensity")
dimnames(res) <- list(lat = nc$dim$lat$vals,
                      lon = nc$dim$lon$vals,
                      t = nc$dim$time$vals)
nc_close(nc)

res2 <- as.data.frame(reshape2::melt(res, value.name = "intensity"), row.names = NULL) %>% 
  mutate(t = as.Date(t, origin = "1970-01-01"),
         intensity = replace(intensity, which(intensity < 0), NA)) %>%
  select(lon, everything()) %>% 
  filter(t == as.Date("2017-12-13")) %>% 
  na.omit()

ggplot(res2, aes(x = lon, y = lat, fill = intensity)) +
  geom_raster() +
  scale_fill_viridis_c()


