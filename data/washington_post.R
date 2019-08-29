# data/washington_post.R
# The purpose of this script is to extract data around Uruguay
# from January - April of 2017 in order ri visualise an IV Extreme MHW
# These data are then converted to raster format for the convenience
# of the Washington Post illustrators


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(raster)
library(gganimate, lib.loc = "~/R-packages/")
doMC::registerDoMC(cores = 50)


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

# Save single raster
writeRaster(MHW_2017_single_raster, "data/washington_post.asc")


# Visualise ---------------------------------------------------------------

# Plot a single raster file
plot(MHW_2017_single_raster)

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

