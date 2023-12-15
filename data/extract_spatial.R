# data/extract_spatial.R
# The purpose of this script is to extract data for a bespoke time and place
# These data may then be converted to raster format for those that request it


# Setup -------------------------------------------------------------------

source("MHW_daily_functions.R")
library(raster)
library(gganimate)#, lib.loc = "~/R-packages/")
doParallel::registerDoParallel(cores = 50)

# CCI file location
CCI_files <- dir("../data/CCI", full.names = T)
# Remove 1981 data
CCI_files_sub <- CCI_files[123:length(CCI_files)]; rm(CCI_files)
# head(CCI_files, 1); tail(CCI_files, 1)

# Global MHW daily clim file locations
cat_clim_annual_folders <- dir("shiny/cat_clim", full.names = T)
cat_clim_files <- dir(cat_clim_annual_folders, recursive = T, full.names = T)

# The base map for plotting
load("metadata/map_base.Rdata")


# Load global data --------------------------------------------------------

# Function for loading file and including date from file name
readRDS_date <- function(file_name){
  file_date <- sapply(strsplit(file_name, "/"), "[[", 4)
  file_date <- sapply(strsplit(file_date, "[.]"), "[[", 3)
  base <- readRDS(file_name) %>% 
    mutate(t = as.Date(file_date)) %>% 
    dplyr::select(t, lon, lat, category)
  return(base)
}

# Load and filter out desired 2017 data around Uruguay
MHW_2017 <- map_dfr(dir("shiny/cat_clim/2017", full.names = T), readRDS_date) %>% 
  filter(t <= "2017-04-30")

# Grab a single day
MHW_2017_single <- readRDS_date("shiny/cat_clim/2017/cat.clim.2017-02-25.Rda")

# 2016 data for NZ MHW
MHW_2016_single <- readRDS_date("shiny/cat_clim/2016/cat.clim.2016-02-01.Rda")


# Load regional data ------------------------------------------------------

# Function for loading file, including date from file name, and subsetting by lon/lat
# file_name <- "shiny/cat_clim/2017/cat.clim.2017-02-25.Rda"
# lon_min <- -15; lon_max <- 10; lat_min <- 49; lat_max <- 65
readRDS_date_sub <- function(file_name, lon_min, lon_max, lat_min, lat_max){
  file_date <- sapply(strsplit(file_name, "/"), "[[", 4)
  file_date <- sapply(strsplit(file_date, "[.]"), "[[", 3)
  base <- readRDS(file_name) %>% 
    mutate(t = as.Date(file_date)) %>% 
    dplyr::select(t, lon, lat, category) %>% 
    filter(lon >= lon_min, lon <= lon_max,
           lat >= lat_min, lat <= lat_max)
  return(base)
}

# Get subset of years to upload
cat_clim_files_sub <- dir(cat_clim_annual_folders[30:41], recursive = T, full.names = T)

# Get single dataframe for UK region
MHW_UK <- plyr::ldply(cat_clim_files_sub, readRDS_date_sub, .parallel = T,
                      lon_min = -15, lon_max = 10, lat_min = 49, lat_max = 65)
write_csv(MHW_UK, file = "data/MHW_UK.csv")

# Test visual
filter(MHW_UK, t == as.Date("2013-02-19")) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = category)) +
  borders() +
  coord_quickmap(xlim = c(-15, 10), ylim = c(49, 65))


# Create rasters ----------------------------------------------------------

# The two different map projections used
inputProj <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
leafletProj <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs"

# The colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Function for performing the full raster process
raster_MHW <- function(df){
  df <- dplyr::select(df, lon, lat, category)
  # Rename the columns to make the raster function happy
  colnames(df) <- c("X", "Y", "Z")
  # Convert the category column to numeric values
  df$Z <- as.numeric(df$Z)
  # Convert to a raster
  df_raster <- raster::rasterFromXYZ(df, res = c(0.25, 0.25), digits = 3, crs = inputProj)
}

# Create single raster
MHW_2017_single_raster <- raster_MHW(MHW_2017_single)
MHW_2016_single_raster <- raster_MHW(MHW_2016_single)

# Save single raster
writeRaster(MHW_2017_single_raster, "data/washington_post.asc")
writeRaster(MHW_2016_single_raster, "data/washington_post_NZ.asc")


# Visualise ---------------------------------------------------------------

# Plot a single raster file
plot(MHW_2017_single_raster)
plot(MHW_2016_single_raster)

# Uruguay bounding box
bbox_uruguay <- c(-67, -6, -55, -23)

# Test figure to look at extent of focus MHW
ggplot(data = filter(MHW_2017, t == "2017-02-25"), aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_raster(aes(fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  coord_cartesian(xlim = c(bbox_uruguay[1], bbox_uruguay[2]),
                  ylim = c(bbox_uruguay[3], bbox_uruguay[4])) +
  labs(x = NULL, y = NULL)


# Animate -----------------------------------------------------------------

# The base map
world <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_cartesian(xlim = c(bbox_uruguay[1], bbox_uruguay[2]),
                  ylim = c(bbox_uruguay[3], bbox_uruguay[4])) +
  labs(x = NULL, y = NULL)+
  theme(panel.background = element_rect(fill = "lightblue"))

# The number of day steps for animation
anim_days <- length(unique(MHW_2017$t))

cat_plot <- world +
  geom_raster(data = MHW_2017, aes(x = lon, y = lat, fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  labs(title = 'Date: {frame_time}') +
  transition_time(time = t)# +
  # ease_aes("linear", interval = 2)

# It seems as though it is impossible to get the video to render as anything other than 4 seconds...
cat_anim <- animate(plot = cat_plot, renderer = ffmpeg_renderer())
# cat_anim <-  animate(cat_anim, fps = 10, duration = anim_days)

# Save the animation
anim_save(animation = cat_anim, filename = "washington_post.mp4", path = "anim")

# Create a slower video using console commands
system("ffmpeg -i anim/washington_post.mp4 -vf 'setpts=7*PTS' anim/washington_post_slow.mp4")


# Extract SST for a lon step ----------------------------------------------

# See lon_lat_OISST for exact coordinate options
unique(lon_lat_OISST$lon)
unique(lon_lat_OISST$lat)

# Set extraction coordinates
lon_point <- 8.625
lat_point <- 58.375

# Set download name
dl_name <- "norway_2018"

## NB: Don't change this code chunk
OISST_dl <- sst_seas_thresh_merge(lon_step = lon_point,
                                  start_date = "2018-01-01") %>% 
  filter(lat == lat_point,
         t <= "2018-12-31") %>% 
  mutate(lon = lon_point) %>% 
  select(lon, everything())
saveRDS(OISST_dl, paste0("downloads/",dl_name,".Rda"))

# Extractions:
# USA  site coordinates: 41.478ºN, - 71.362º, MHW map for Aug 30 2018, time series for whole of 2018. 
# Norway site coordinate = 58,317861  8,596863, May 30, 2018, time series for whole of 2018.    


# Find the index of a range of lon steps to manually download -------------

# The longitude associated with the files
lon_seq <-  c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))

# Add this to the file directories
MHW_event_files <- data.frame(file_name = dir("../data/event", pattern = "MHW.event.", full.names = T),
                              lon_step = lon_seq, stringsAsFactors = F)
cat_lon_files <- data.frame(file_name = dir("../data/cat_lon", full.names = T),
                            lon_step = lon_seq, stringsAsFactors = F)
seas_thresh_files <- data.frame(file_name = dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T),
                                lon_step = lon_seq, stringsAsFactors = F)

# Check the contents of a file
MHW_event_file <- readRDS(MHW_event_files$file_name[1])
cat_lon_file <- readRDS(cat_lon_files$file_name[1])


# Function for combining multiple lon steps into a single file ------------

# 'lon_steps' is expected to be two numbers, the range of lon values
munge_lon_steps <- function(lon_steps, lon_files){
  file_names <- lon_files %>% 
    filter(lon_step >= min(lon_steps), lon_step <= max(lon_steps))
  df <- plyr::ldply(file_names$file_name, readRDS, .parallel = T)
  return(df)
}


# Extract US coastal EEZ for EPA ------------------------------------------

# Saving the files with .Rds compression is much more efficient

# US west coast
USWC <- c(-130, -117)
USWC_MHW_event_file <- munge_lon_steps(USWC, MHW_event_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USWC_MHW_event_file, file = "data/USWC_MHW_event_file.Rds")
USWC_cat_lon_file <- munge_lon_steps(USWC, cat_lon_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USWC_cat_lon_file, file = "data/USWC_cat_lon_file.Rds")

# US east coast
USEC <- c(-81, -65)
USEC_MHW_event_file <- munge_lon_steps(USEC, MHW_event_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USEC_MHW_event_file, file = "data/USEC_MHW_event_file.Rds")
USEC_cat_lon_file <- munge_lon_steps(USEC, cat_lon_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USEC_cat_lon_file, file = "data/USEC_cat_lon_file.Rds")

# US Gulf coast
USGC <- c(-98, -81)
USGC_MHW_event_file <- munge_lon_steps(USGC, MHW_event_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USGC_MHW_event_file, file = "data/USGC_MHW_event_file.Rds")
USGC_cat_lon_file <- munge_lon_steps(USGC, cat_lon_files) %>% 
  filter(lat >= 10, lat <= 50)
saveRDS(USGC_cat_lon_file, file = "data/USGC_cat_lon_file.Rds")


# Extract global annual pixel data ----------------------------------------

# Locate files
cat_pixel_files <- dir("data/annual_summary", full.names = T,
                       pattern = paste0("OISST_cat_pixel_1982-2011"))

# Load them into one brick
cat_pixel <- map_dfr(cat_pixel_files, readRDS) 

# Filter down to only 2011-2021
MHW_global_2011_2021 <- cat_pixel %>% 
  filter(t >= "2011-01-01")

# Save as a .RData and .csv for convenience
save(MHW_global_2011_2021, file = "data/MHW_global_2011_2021.RData")
write_csv(MHW_global_2011_2021, "data/MHW_global_2011_2021.csv")


# Extract global cat_clim data --------------------------------------------

# Locate files
# NB: This is ALL of the data
# Filter this down first if desired
cat_clim_rds_files <- dir("../data/cat_clim", full.names = T, 
                          recursive = T, pattern = ".Rda")
cat_clim_tif_files <- dir("../data/cat_clim", full.names = T, 
                          recursive = T, pattern = ".tif")
cat_clim_tif_files <- cat_clim_tif_files[!grepl("MCS", cat_clim_tif_files)]

# Load them into one brick
# NB: Surprisingly this only takes a few minutes
cat_clim_ALL <- map_dfr(cat_clim_rds_files, readRDS)
cat_clim_ALL_tif <- lapply(cat_clim_tif_files, raster)
# cat_clim_ALL_tif_stack <- stack(cat_clim_tif_files) # Files don't have same extent...

# Filter as desired
# ...

# Save stacked .tif file
# raster::stackSave(cat_clim_ALL_tif, "data/cat_clim_ALL.tif") # Files don't have same extent...
save(cat_clim_ALL_tif, file = "data/cat_clim_ALL.RData")

# Save as zipped .csv per year
zip_by_year <- function(year, df, file_stub){
  # Subset data
  df_sub <- df |> 
    filter(t >= paste0(year,"-01-01"), t <= paste0(year,"-12-31"))
  # Save file
  write_csv(df_sub, paste0("data/",file_stub,"_",year,".csv.gz"))
}
doParallel::registerDoParallel(cores = 10)
plyr::l_ply(1982:2022, zip_by_year, df = cat_clim_ALL, file_stub = "cat_clim")


# Extract bounding boxes around Arctic fjords -----------------------------

# Function for extracting NOAA OISST only from a given bbox
## NB: This could easily be adapted to extract other MHW values etc. from a bbox
sst_bbox <- function(bbox, filter_full_year = TRUE){
  lon_bbox <- lon_OISST[which(lon_OISST >= bbox[1] & lon_OISST <= bbox[2])]
  lat_bbox <- range(lat_OISST[which(lat_OISST >= bbox[3] & lat_OISST <= bbox[4])])
  sst_dat <- plyr::ldply(lon_bbox, sst_seas_thresh_merge, .parallel = T, 
                         date_range = as.Date("1982-01-01"), lat_range = lat_bbox) %>% 
    dplyr::select(lon, lat, t, temp) %>% 
    dplyr::filter(lat >= bbox[3], lat <= bbox[4])
  if(filter_full_year){
    full_year <- as.Date(paste0(year(Sys.Date())-1,"-12-31"))
    sst_dat <- filter(sst_dat, t <= full_year)
  }
  return(sst_dat)
}

# Function for extracting CCI SST from a given bbox
sst_CCI_bbox <- function(file_name, bbox){
  res <- tidync(file_name) %>%
    hyper_filter(lon = dplyr::between(lon, bbox[1], bbox[2]),
                 lat = dplyr::between(lat, bbox[3], bbox[4])) %>%
    hyper_tibble() %>%
    dplyr::rename(t = time, temp = analysed_sst) %>%
    na.omit() %>% 
    mutate(t = as.Date(as.POSIXct(t, origin = '1981-01-01', tz = "GMT")),
           temp = round(temp-273.15, 2)) %>%
    dplyr::select(lon, lat, t, temp)
  return(res)
}

# Arctic circle
## NB: Too large to extract here
# doParallel::registerDoParallel(cores = 25)
# bbox_arctic <- c(-180, 180, 60, 90)
# sst_arctic <- sst_bbox(bbox_arctic)
# save(sst_arctic, file = "data/sst_arctic.RData")

# EU Arctic
## NB: Not run due to size
# doParallel::registerDoParallel(cores = 25)
# bbox_EU_arctic <- c(-60, 60, 60, 90)
# sst_EU_arctic <- sst_bbox(bbox_EU_arctic)
# save(sst_EU_arctic, file = "data/sst_EU_arctic.RData")

# Svalbard
bbox_sval <- c(9, 30, 76, 81)
sst_sval <- sst_bbox(bbox_sval)
save(sst_sval, file = "data/sst_sval.RData")
## NB: Not run due to size
# sst_CCI_sval <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_sval)
# save(sst_CCI_sval, file = "data/sst_CCI_sval.RData")
# rm(sst_sval, sst_CCI_sval); gc()

# Kongsfjorden
bbox_kong <- c(9.5, 14.0, 78.0, 79.5)
sst_kong <- sst_bbox(bbox_kong)
save(sst_kong, file = "data/sst_kong.RData")
sst_CCI_kong <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_kong)
save(sst_CCI_kong, file = "data/sst_CCI_kong.RData")
rm(sst_kong, sst_CCI_kong); gc()

# Isfjorden
bbox_is <- c(10.0, 18.0, 77.0, 79.0)
sst_is <- sst_bbox(bbox_is)
save(sst_is, file = "data/sst_is.RData")
sst_CCI_is <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_is)
save(sst_CCI_is, file = "data/sst_CCI_is.RData")
rm(sst_is, sst_CCI_is); gc()

# Storfjorden
bbox_stor <- c(17.0, 22.0, 77.0, 78.5)
sst_stor <- sst_bbox(bbox_stor)
save(sst_stor, file = "data/sst_stor.RData")
sst_CCI_stor <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_stor)
save(sst_CCI_stor, file = "data/sst_CCI_stor.RData")
rm(sst_stor, sst_CCI_stor); gc()

# Young sound
bbox_young <- c(-22.5, -17.5, 73.0, 75.5)
sst_young <- sst_bbox(bbox_young)
save(sst_young, file = "data/sst_young.RData")
sst_CCI_young <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_young)
save(sst_CCI_young, file = "data/sst_CCI_young.RData")
rm(sst_young, sst_CCI_young); gc()

# Disko Bay
bbox_disko <- c(-56.0, -49.0, 68.0, 71.0)
sst_disko <- sst_bbox(bbox_disko)
save(sst_disko, file = "data/sst_disko.RData")
sst_CCI_disko <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_disko)
save(sst_CCI_disko, file = "data/sst_CCI_disko.RData")
rm(sst_disko, sst_CCI_disko); gc()

# Nuup Kangerlua
bbox_nuup <- c(-53.5, -48.5, 63.5, 65.0)
sst_nuup <- sst_bbox(bbox_nuup)
save(sst_nuup, file = "data/sst_nuup.RData")
sst_CCI_nuup <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_nuup)
save(sst_CCI_nuup, file = "data/sst_CCI_nuup.RData")
rm(sst_nuup, sst_CCI_nuup); gc()

# Porsangerfjorden
bbox_por <- c(23.5, 28, 69, 72.0)
sst_por <- sst_bbox(bbox_por)
save(sst_por, file = "data/sst_por.RData")
sst_CCI_por <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_por)
save(sst_CCI_por, file = "data/sst_CCI_por.RData")
rm(sst_por, sst_CCI_por); gc()

# Tromso
bbox_trom <- c(17.5, 21.0, 69.0, 70.5)
sst_trom <- sst_bbox(bbox_trom)
save(sst_trom, file = "data/sst_trom.RData")
sst_CCI_trom <- plyr::ldply(CCI_files_sub, sst_CCI_bbox, .parallel = T, bbox = bbox_trom)
save(sst_CCI_trom, file = "data/sst_CCI_trom.RData")
rm(sst_trom, sst_CCI_trom); gc()

# Greenland
## NB: Not run due to size
# bbox_gland <- c(-77, -9, 58, 84)
# sst_gland <- sst_bbox(bbox_gland)
# save(sst_gland, file = "data/sst_gland.RData")


# Summary analyses --------------------------------------------------------

## Load all data to filter by event duration to get count of events etc. per pixel

# Load all and combine
OISST_ocean_coords$index <- NULL
cat_count_files <- dir("data/annual_summary", pattern = "MHW_cat_count_N", full.names = TRUE)
MHW_cat_count_all <- map_dfr(cat_count_files, readRDS)
MHW_cat_count_sum <- MHW_cat_count_all |>
  group_by(lon, lat) |> summarise_all(sum) |> ungroup()
MHW_cat_count_oce <- right_join(MHW_cat_count_sum, OISST_ocean_coords, by = c("lon", "lat"))
ggplot(data = MHW_cat_count_oce, aes(x = lon, y = lat, fill = `I Moderate`)) +
  geom_tile() + scale_fill_viridis_c(limit = c(1, 140))

# Get oceans pixels that have never had a MHW of any category
MHW_cat_count_miss <- MHW_cat_count_oce |>
  pivot_longer(`I Moderate`:`IV Extreme`, names_to = "Category", values_to = "Count") |>
  summarise(cat_sum = sum(Count), .by = c("lon", "lat")) |>
  filter(cat_sum == 0)
ggplot(data = OISST_ocean_coords, aes(x = lon, y = lat)) +
  geom_tile(fill = "grey") + geom_point(data = MHW_cat_count_miss, colour = "red", size = 5) +
  ggtitle("Red dots show the pixels that have never had a MHW", subtitle = "86 of 691,150 pixels (0.01%)")

# Load and filter
event_dur_filter <- function(file_name, dur_filt){
  file_filter <- readRDS(file_name) |> 
    filter(duration >= dur_filt) |> 
    summarise(count = n(), .by = c("lon", "lat")) |> 
    mutate(dur_limit = dur_filt)
}

# Test
test_1 <- readRDS(MHW_event_files[2]) |> filter(lat > -45, lat < -40)
test_1 <- event_dur_filter(MHW_event_files[2], dur_filt = 5)

# Run it
global_dur_05 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 5) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 5)
global_dur_10 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 10) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 10)
global_dur_15 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 15) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 15)
global_dur_20 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 20) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 20)
global_dur_25 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 25) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 25)
global_dur_30 <- plyr::ldply(MHW_event_files, event_dur_filter, .parallel = TRUE, dur_filt = 30) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat")) |> mutate(dur_limit = 30)

# Combine
global_dur <- rbind(global_dur_05, global_dur_10, global_dur_15, global_dur_20, global_dur_25, global_dur_30) |> 
  replace_na(list(count = 0))
global_dur_0 <- filter(global_dur, count == 0)

# Plot
ggplot(data = global_dur, aes(x = lon, y = lat, fill = count)) +
  geom_tile() + geom_point(data = global_dur_0, size = 0.01, colour = "red") +
  scale_fill_viridis_c() + facet_wrap(~dur_limit)

# Just 30 day events
ggplot(data = filter(global_dur, dur_limit == 30), aes(x = lon, y = lat, fill = count)) +
  geom_tile() + #geom_point(data = global_dur_0, size = 0.01, colour = "red") +
  scale_fill_viridis_c(limit = c(1, 45))

global_dur |>
  filter(dur_limit == 30,
         lon >= -7, lon <= 36, lat >= 30, lat <= 48) |> 
    ggplot(aes(x = lon, y = lat, fill = count)) +
  geom_tile() + #geom_point(data = global_dur_0, size = 0.01, colour = "red") +
  scale_fill_viridis_c()

med_dur_30 <- global_dur |>
  filter(dur_limit == 30,
         lon >= -7, lon <= 36, lat >= 30, lat <= 48)

# Load and filter by max duration
event_max_filter <- function(file_name){
  file_filter <- readRDS(file_name) |> 
    group_by(lon, lat) |> 
    filter(duration == max(duration)) |> 
    filter(date_peak == max(date_peak)) |> 
    ungroup()
  return(file_filter)
}
# Run it
global_dur_max <- plyr::ldply(MHW_event_files, event_max_filter, .parallel = TRUE) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat"))

# Set them for panels
theme_panel <- function(){
  theme(legend.position = "bottom",
        legend.key.width = unit(1.2, "cm"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 10, hjust = 1),
        panel.border = element_rect(fill = NA, colour = "black"),
        panel.background = element_rect(fill = "grey10"))
}

## A) Average annual MHW days from 1982-1986
# Calculate annual sum of MHW days per pixel
event_annual_sum_dur <- function(file_name){
  file_filter <- readRDS(file_name) |> 
    mutate(year = year(date_peak)) |> 
    summarise(annual_sum_duration = sum(duration, na.rm = TRUE), .by = c(lon, lat, year))
  return(file_filter)
}
# Run it
global_annual_sum_dur <- plyr::ldply(MHW_event_files, event_annual_sum_dur, .parallel = TRUE) |> 
  right_join(OISST_ocean_coords, by = c("lon", "lat"))
# Get global average for first pentad
global_mean_sum_dur_1986 <- global_annual_sum_dur |> 
  filter(year <= 1986) |> 
  summarise(mean_sum_duration = mean(annual_sum_duration, na.rm = TRUE), .by = c(lon, lat))
# Plot it
panel_A <- global_mean_sum_dur_1986 |> 
  # NB: Filter out several funny Arctic pixels
  mutate(mean_sum_duration = case_when(mean_sum_duration > 160 ~ 160, TRUE ~ mean_sum_duration)) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = mean_sum_duration), show.legend = TRUE) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), 
               fill = "grey70", colour = "black") +
  scale_fill_viridis_c(breaks = c(30, 60, 90, 120, 150)) +
  scale_x_continuous(breaks = c(-100, 0, 100),
                     labels = c("100°W", "0", "100°E")) +
  scale_y_continuous(breaks = c(-45, 0, 45),
                     labels = c("45°S", "0", "45°N")) +
  coord_cartesian(expand = F,
                  ylim = c(min(OISST_ocean_coords$lat), max(OISST_ocean_coords$lat))) +
  labs(x = NULL, y = NULL, fill = "MHW\ndays",
       title = "Mean annual sum of MHW days from 1982 - 1986") +
  theme_panel()
panel_A

## B) Longest duration MHW
# Get global average for first pentad
global_mean_sum_dur_2023 <- global_annual_sum_dur |> 
  filter(year >= 2019) |> 
  summarise(mean_sum_duration = mean(annual_sum_duration, na.rm = TRUE), .by = c(lon, lat))
# Plot it
panel_B <- global_mean_sum_dur_2023 |> 
  # NB: Filter out several funny Arctic pixels
  mutate(mean_sum_duration = case_when(mean_sum_duration > 160 ~ 160, TRUE ~ mean_sum_duration)) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = mean_sum_duration), show.legend = TRUE) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), 
               fill = "grey70", colour = "black") +
  scale_fill_viridis_c(breaks = c(30, 60, 90, 120, 150)) +
  scale_x_continuous(breaks = c(-100, 0, 100),
                     labels = c("100°W", "0", "100°E")) +
  scale_y_continuous(breaks = c(-45, 0, 45),
                     labels = c("45°S", "0", "45°N")) +
  coord_cartesian(expand = F,
                  ylim = c(min(OISST_ocean_coords$lat), max(OISST_ocean_coords$lat))) +
  labs(x = NULL, y = NULL, fill = "MHW\ndays",
       title = "Mean annual sum of MHW days from 2019 - 2023") +
  theme_panel()
panel_B

## C) First year of occurrence of 183+ annual MHW days
# Find years when this has already happened
global_183_hist <- global_annual_sum_dur |> 
  filter(annual_sum_duration >= 183, !is.na(year)) |> 
  summarise(year_183 = min(year), .by = c(lon, lat))
# Plot it
panel_C <- global_183_hist |> 
  # NB: Filter out several funny Arctic pixels
  mutate(year = year_183) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = year), show.legend = TRUE) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), 
               fill = "grey70", colour = "black") +
  scale_fill_viridis_c(option = "C") +
  scale_x_continuous(breaks = c(-100, 0, 100),
                     labels = c("100°W", "0", "100°E")) +
  scale_y_continuous(breaks = c(-45, 0, 45),
                     labels = c("45°S", "0", "45°N")) +
  coord_cartesian(expand = F,
                  ylim = c(min(OISST_ocean_coords$lat), max(OISST_ocean_coords$lat))) +
  # theme_void() +
  labs(x = NULL, y = NULL, fill = "Year", 
       title = "First year of occurrence of 183+ MHW days") +
  theme_panel()
panel_C

## D) Annual trend for sum of MHW days 
# Calculate linear trends
lm_annual_sum_dur <- function(df){
  # Decadal trend
  if(length(unique(df$annual_sum_duration)) >3){
    dec_trend <- broom::tidy(lm(annual_sum_duration ~ year, df)) |>  
      slice(2) |> 
      mutate(dec_trend = round(estimate*10, 3),
             p.value = round(p.value, 4)) %>%
      dplyr::select(dec_trend, p.value)
  }
}
# Run it
# NB: This takes a few minutes
global_annual_sum_dur_trend <- plyr::ddply(global_annual_sum_dur, c("lon", "lat"), lm_annual_sum_dur, .parallel = T)
# Filter for stipple plotting
global_annual_sum_dur_stipple <- filter(global_annual_sum_dur_trend, p.value <= 0.05) |> 
  mutate(lon = plyr::round_any(lon, 2.5), lat = plyr::round_any(lat, 2.5)) |> dplyr::select(lon, lat) |> distinct()
# Plot it
panel_D <- global_annual_sum_dur_trend |> 
  mutate(dec_trend = case_when(dec_trend > 80 ~ 80, dec_trend < -80 ~ -80, TRUE ~ dec_trend)) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = dec_trend/10), show.legend = TRUE) +
  geom_point(data = global_annual_sum_dur_stipple,
             colour = "black", size = 0.1, alpha = 0.1, show.legend = FALSE) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), 
               fill = "grey70", colour = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  scale_x_continuous(breaks = c(-100, 0, 100),
                     labels = c("100°W", "0", "100°E")) +
  scale_y_continuous(breaks = c(-45, 0, 45),
                     labels = c("45°S", "0", "45°N")) +
  coord_cartesian(expand = F,
                  ylim = c(min(OISST_ocean_coords$lat), max(OISST_ocean_coords$lat))) +
  # theme_void() +
  labs(x = NULL, y = NULL, fill = "MHW\ndays/year",
       title = "Trend for sum of MHW days per year") +
  theme_panel()
panel_D

# Stitch it together
global_MHW_summary_plot <- ggpubr::ggarrange(panel_A, panel_B, panel_C, panel_D, nrow = 2, ncol = 2, 
                                             labels = c("A)", "B)", "C)", "D)"))
ggsave("figures/global_MHW_summary_plot.png", global_MHW_summary_plot, width = 12, height = 8)
