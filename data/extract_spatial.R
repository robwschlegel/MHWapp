# data/extract_spatial.R
# The purpose of this script is to extract data for a bespoke time and place
# These data may then be converted to raster format for those that request it


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

library(tidyverse)
library(raster)
library(gganimate)#, lib.loc = "~/R-packages/")
doParallel::registerDoParallel(cores = 50)


# Load data ---------------------------------------------------------------

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

# Saving the files with .Rdds compression is much more efficient

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

