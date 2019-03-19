# MHW_anim.R
# The purpose of this script currently is to make animations of the 
# three reference time series that are pacakge in heatwaveR.
# it does this based on the daily category files used in the MHW Tracker



# Libraries ---------------------------------------------------------------
library(tidyverse)
library(gganimate, lib.loc = "../R-packages/")
library(heatwaveR, lib.loc = "../R-packages/")
doMC::registerDoMC(cores = 50)
# library(padr)


# Meta-data ---------------------------------------------------------------

category_files <- as.character(dir(path = "../data/cat_clim", pattern = "cat.clim",
                                   full.names = TRUE, recursive = TRUE))

site_coords <- data.frame(site = c("WA", "NW_Atl", "Med"),
                          lon = c(112.5, -67, 9),
                          lat = c(-29.5, 43, 43.5))

# Load data ---------------------------------------------------------------

sst_ALL <- rbind(sst_Med, sst_NW_Atl, sst_WA) %>% 
  mutate(site = rep(c("Med", "NW_Atl", "WA"), each = nrow(sst_WA)))


# Calculate MHWs ----------------------------------------------------------

# Calculate all results
MHW_res <- sst_ALL %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(clim = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31")),
         event = map(clim, detect_event),
         cat = map(event, category)) %>% 
  select(-data, -clim)

# Clim data.frame
MHW_clim <- MHW_res %>% 
  select(-cat) %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(event)

# Event data.frame
MHW_event <- MHW_res %>% 
  select(-cat) %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(event)

# Category data.frame
MHW_cat <- MHW_res %>% 
  select(-event) %>% 
  unnest(cat) %>% 
  mutate(category = factor(category, 
                           levels = c("I Moderate", "II Strong", 
                                      "III Severe", "IV Extreme")))
# levels(MHW_cat$category)


# Determine MHW of choice -------------------------------------------------

MHW_focus <- MHW_event %>% 
  group_by(site) %>% 
  filter(intensity_cumulative == max(intensity_cumulative))
# Interestingly this grabs a different MHW for the Med
# so we need to more manually grab that one

Med_focus <- MHW_event %>% 
  filter(site == "Med", date_end <= "2004-01-01") %>% 
  filter(intensity_cumulative == max(intensity_cumulative)) %>% 
  left_join(site_coords, by = "site")
WA_focus <- MHW_focus[3,] %>% 
  left_join(site_coords, by = "site")
NW_Atl_focus <- MHW_focus[2,] %>% 
  left_join(site_coords, by = "site")
Blob_focus <- data.frame()

  
# Decide on bounding box --------------------------------------------------

# Load the category days from the peak of the chosen MHWs
# category_test <- readRDS(category_files[1])
category_WA <- readRDS(category_files[grepl(pattern = as.character(WA_focus$date_peak), 
                                            x = category_files)])
category_NW_Atl <- readRDS(category_files[grepl(pattern = as.character(NW_Atl_focus$date_peak), 
                                                x = category_files)])
category_Med <- readRDS(category_files[grepl(pattern = as.character(Med_focus$date_peak), 
                                             x = category_files)])

# Test figure to look at extent of focus MHW
ggplot(data = category_Med, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  coord_cartesian(xlim = c(Med_focus$lon[1]-20, Med_focus$lon[1]+20),
                  ylim = c(Med_focus$lat[1]-20, Med_focus$lat[1]+20)) +
  labs(x = NULL, y = NULL)

# Bounding boxes from center point of MHW
# WA = lon+-20 lat+-20
# NW_Atl = lon+-20 lat+-20
# Med = lon+-20 lat+-20


# Subset category files ---------------------------------------------------

# Function that loads, subsets and returns a day of category data
# testers...
# vc <- category_files_focus[1]
# lon_point <- WA_focus$lon
# lat_point <- WA_focus$lat
# spread <- 20
category_subset <- function(vc, lon_point, lat_point, spread = 20){
  cat_dat <- readRDS(vc)
  res <- cat_dat %>%
    filter(lon >= lon_point-spread, lon <= lon_point+spread,
           lat >= lat_point-spread, lat <= lat_point+spread) %>%
    mutate(t = as.Date(sapply(strsplit(vc, "[.]"), "[[", 5)),
           category = factor(category, 
                             levels = c("I Moderate", "II Strong", 
                                        "III Severe", "IV Extreme")))
  return(res)
}

# This function is fed the one line dataframe with the focus event meta-data etc.
# testers...
# df <- WA_focus
category_load <- function(df){
  start_extract <- df$date_start-7
  end_extract <- df$date_end+7
  seq_extract <- seq(start_extract, end_extract, by = "day")
  category_files_focus <- grep(paste(seq_extract, collapse = "|"), 
                               category_files, value = TRUE)
  res <- plyr::ldply(category_files_focus, category_subset, 
                     lon = df$lon, lat = df$lat, .parallel = T)
  return(res)
}


WA_cat_dat <- category_load(WA_focus)
NW_Atl_cat_dat <- category_load(NW_Atl_focus)
Med_cat_dat <- category_load(Med_focus)


# Create animation --------------------------------------------------------


# df <- df %>% 
#   filter(t == "2003-05-29")

# Function that is fed a category dataframe and creates an animation
# testers...
# df <- WA_cat_dat
# spread <- 0
# site <- "WA"
category_animate <- function(df, site, spread = 0){
  anim_days <- length(unique(df$t))
  world <- ggplot() +
    borders("world", colour = "black", fill = "gray80") +
    coord_cartesian(xlim = c(min(df$lon)-spread, max(df$lon)+spread),
                    ylim = c(min(df$lat)-spread, max(df$lat)+spread),
                    expand = 0) +
    labs(x = NULL, y = NULL) +
    theme(panel.background = element_rect(fill = "lightblue"))
  cat_anim <- world +
    geom_raster(data = df, aes(x = lon, y = lat, fill = category)) +
    scale_fill_manual("Category",
                      values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                      labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
    # borders(fill = "grey80", colour = "black") +
    # coord_cartesian(xlim = c(min(df$lon)-spread, max(df$lon)+spread),
                    # ylim = c(min(df$lat)-spread, max(df$lat)+spread)) +
    labs(title = 'Date: {frame_time}') +
    transition_time(time = t)# +
    # ease_aes("linear", interval = 2)
  # It seems as though it is impossible to get the video to render as anything other than 4 seconds...
  res <- animate(cat_anim, renderer = ffmpeg_renderer())
  # res <- animate(cat_anim, fps = 10, duration = anim_days)
  anim_save(animation = res, filename = paste0(site,"_infamous.mp4"), path = "anim")
  # Create a slower video using console commands
  system(paste0("ffmpeg -i anim/",site,"_infamous.mp4 -vf 'setpts=7*PTS' anim/",site,"_infamous_slow.mp4"))
}

# Render and save the animations
# NB: Must manually delete the slow versions of the following outputs to re-create them
category_animate(WA_cat_dat, "WA")
category_animate(NW_Atl_cat_dat, "NW_Atl")
category_animate(Med_cat_dat, "Med")
