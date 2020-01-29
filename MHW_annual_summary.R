# MHW_annual_summary
# The purpose of this script is to perform an annual summary
# of the global state of MHWs


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

library(tidyverse)
# library(purrr)
library(heatwaveR)
library(ncdf4)
library(tidync)#, lib.loc = "../R-packages/")
library(dtplyr)#, lib.loc = "../R-packages/")
# library(ggrepel)
library(doParallel); registerDoParallel(cores = 50)

# Animation libraries
# library(animation)
# library(magick)


# Metadata ----------------------------------------------------------------

# The OISST data location
OISST_files <- dir("../data/OISST", full.names = T, pattern = "avhrr")

# Function for extracting one day of data
# testers...
# index_val <- 20
# file_name <- OISST_files[1]
# extract_OISST_one <- function(index_val){
#   file_name <- OISST_files[index_val]
#   res <- tidync(file_name) %>% 
#     hyper_filter(time = time == 18000) %>% 
#     hyper_tibble() %>% 
#     select(-time, -sst)
# }

# Get coords for each ocean pixel 
# system.time(OISST_ocean_coords <- plyr::ldply(1:1440, extract_OISST_one, .parallel = T)) # 9 seconds
# save(OISST_ocean_coords, file = "metadata/OISST_ocean_coords.Rdata")
load("metadata/OISST_ocean_coords.Rdata")

# Visualise ocean pixels
# ggplot(OISST_ocean_coords, aes(x = lon, y = lat)) + geom_tile()

# Exclude OISST pixels with any near-ice (-1.6C) cover
# During iterations of this methodology it was found that there are pixels near
# the ice edge that don't ever quite reach -1.8C but remain in a "slush" state
# This was determined to be an artefact of the OISST process and so the new
# "ice" limit was set to -1.6C
# load_OISST_no_ice_coords <- function(nc_file){
# 
#   # OISST data
#   nc_OISST <- nc_open(nc_file)
#   lon_vals <- as.vector(nc_OISST$dim$lon$vals)
#   lat_vals <- as.vector(nc_OISST$dim$lat$vals)
#   lat_index <- c(which(lat_vals == -69.875), which(lat_vals == 79.875))
#   time_vals <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
#   time_index <- which(time_vals == as.Date("2018-12-31"))
#   sst_raw <- ncvar_get(nc_OISST, "sst", start = c(lat_index[1], 1, 1),
#                        count = c(lat_index[2]-lat_index[1]+1, -1, time_index))
#   dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals[lat_index[1]:lat_index[2]],
#                             t = time_vals[seq_len(time_index)])
#   nc_close(nc_OISST)
# 
#   # Prep SST for further use
#   # system.time(
#   res <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
#     na.omit() %>%
#     mutate(lon = lon_vals[1],
#            t = as.Date(t, origin = "1970-01-01")) %>%
#     # Filter out pixels with any ice/slush cover during the time series
#     # Filter out pixels that don't cover the whole time series
#     group_by(lon, lat) %>%
#     filter(min(round(temp, 1)) > -1.6,
#            n() == 13514) %>%
#     ungroup() %>%
#     select(lon, lat) %>%
#     mutate(lon = ifelse(lon > 180, lon-360, lon)) %>%
#     unique() %>%
#     data.frame()
#   # ) ~1.7 seconds
#   
#   # Clear some RAM
#   rm(sst_raw); gc()
#   return(res)
# }

# Get coords for each no ice cover pixel 
# system.time(OISST_no_ice_coords <- plyr::ldply(OISST_files, load_OISST_no_ice_coords, .parallel = T)) # 448 seconds
# save(OISST_no_ice_coords, file = "metadata/OISST_no_ice_coords.Rdata")
load("metadata/OISST_no_ice_coords.Rdata")

# Create ice only layer
OISST_no_ice_coords$index <- paste(OISST_no_ice_coords$lon, OISST_no_ice_coords$lat)
OISST_ocean_coords$index <- paste(OISST_ocean_coords$lon, OISST_ocean_coords$lat)
OISST_ice_coords <- OISST_ocean_coords %>%
  filter(!(index %in% OISST_no_ice_coords$index))

# The base map
map_base <- ggplot2::fortify(maps::map(fill = TRUE, col = "grey80", plot = FALSE)) %>%
  dplyr::rename(lon = long) %>%
  # filter(lat >= 25.6) %>%
  mutate(group = ifelse(lon > 180, group+9999, group),
         lon = ifelse(lon > 180, lon-360, lon))

# The MHW category colour palette
MHW_colours <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)

# Chosen year for analysis
# chosen_year <- 2019
# chosen_year <- 1982


# Functions ---------------------------------------------------------------

# Function that loads a MHW cat file but includes the date from the file name
readRDS_date <- function(file_name){
  file_date <- sapply(str_split(file_name, "/"), "[[", 5)
  file_date <- as.Date(sapply(str_split(file_date, "[.]"), "[[", 3))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date)
}


# Full analysis -----------------------------------------------------------

MHW_annual_state <- function(chosen_year, force_calc = F){
  
  print(paste0("Started run on ",chosen_year," at ",Sys.time()))
  
  ## Find file location
  MHW_cat_files <- dir(paste0("../data/cat_clim/", chosen_year), full.names = T)
  print(paste0("There are currently ",length(MHW_cat_files)," days of data for ",chosen_year))
  
  ## Create figure title
  if(length(MHW_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  fig_title <- paste0("MHW categories of ",chosen_year, extra_bit,
                      "\nNOAA OISST; Climatogy period: 1982 - 2011")
  
  ## Load data
  if(force_calc){
    print(paste0("Loading ",chosen_year," MHW category data; ~12 seconds"))
    # system.time(
    MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date, .parallel = T) #%>% 
    #right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
    # na.omit()
    # ) # 12 seconds
  }
  
  ## Process data
  # Max category per pixel
  if(file.exists(paste0("data/annual_summary/MHW_cat_pixel_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_pixel <- readRDS(paste0("data/annual_summary/MHW_cat_pixel_",chosen_year,".Rds"))
  } else{
    print(paste0("Filtering out the max category at each pixel and counting sum of intensity; ~220 seconds"))
    
    MHW_intensity <- MHW_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity)) %>% 
      ungroup()
    
    # system.time(
    MHW_cat_pixel <- lazy_dt(MHW_cat) %>% 
      select(-event_no) %>% 
      group_by(lon, lat) %>% 
      filter(as.integer(category) == max(as.integer(category))) %>%
      filter(t == min(t)) %>% 
      ungroup() %>% 
      data.frame() %>% 
      left_join(MHW_intensity, by = c("lon", "lat"))
    # ) # 210 seconds
    saveRDS(MHW_cat_pixel, file = paste0("data/annual_summary/MHW_cat_pixel_",chosen_year,".Rds")) 
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/annual_summary/MHW_cat_daily_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_daily <- readRDS(paste0("data/annual_summary/MHW_cat_daily_",chosen_year,".Rds"))
  } else{
    print(paste0("Counting the daily + cumulative categories per day; ~3 seconds"))
    
    # Complete dates by categories data.frame
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(MHW_cat$t), by = "day"), 
                             category = as.factor(levels(MHW_cat$category))) %>% 
      mutate(category = factor(category, levels = levels(MHW_cat$category)))
    
    # system.time(
    MHW_cat_single <- MHW_cat_pixel %>%
      group_by(t) %>%
      count(category) %>%
      dplyr::rename(first_n = n) %>% 
      ungroup() %>% 
      arrange(t) %>% 
      group_by(category) %>%
      mutate(first_n_cum = cumsum(first_n)) %>% 
      ungroup() %>% 
      right_join(full_grid, by = c("t", "category")) %>% 
      group_by(category) %>% 
      fill(first_n_cum, .direction = "downup")
    # ) # 1 second
    
    # system.time(
    MHW_cat_daily <- MHW_cat %>% 
      group_by(t) %>% 
      count(category) %>% 
      group_by(category) %>% 
      dplyr::rename(cat_n = n) %>% 
      mutate(cat_n_cum = cumsum(cat_n),
             cat_n_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
      ungroup() %>% 
      right_join(MHW_cat_single, by = c("t", "category"))
    # ) # 2 seconds
    saveRDS(MHW_cat_daily, file = paste0("data/annual_summary/MHW_cat_daily_",chosen_year,".Rds"))
  }
  
  # Extract small data.frame for easier labelling
  MHW_cat_daily_labels <- MHW_cat_daily %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_n_cum = cumsum(first_n_cum))
  
  ## Create figures
  print("Creating figures")
  
  # Global map of MHW occurrence
  fig_map <- ggplot(MHW_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MHW_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                         max(OISST_ocean_coords$lat))) +
    theme_void() +
    guides(fill = guide_legend(override.aes = list(size = 10))) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # fig_map
   
  # Stacked barplot of global daily count of MHWs by category
  fig_count <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
                       breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
                       labels = paste0(seq(0, 100, by = 10), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MHW_cat_daily, aes(x = t, y = first_n_cum)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MHW_cat_daily_labels, show.legend = F,
               aes(yintercept = label_first_n_cum, colour = category)) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_colour_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
                       breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
                       labels = paste0(seq(0, 100, by = 10), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  # fig_prop
  
  print("Combining figures")
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
                               labels = c("A)"), common.legend = T, legend = "bottom",
                               font.label = list(size = 16))
  
  # Standard caption technique
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
  
  print("Saving final figure")
  ggsave(fig_ALL_cap, filename = paste0("figures/MHW_cat_summary_",chosen_year,".png"), height = 12, width = 18)
  
  print(paste0("Finished run on ",chosen_year," at ",Sys.time()))
}

# Test one year
# system.time(MHW_annual_state(1982, force_calc = T)) # 257 seconds
# MHW_annual_state(2019, force_calc = T)

# Run ALL years
  # NB: Running this in parallel will cause a proper stack overflow
# plyr::l_ply(1982:2019, MHW_annual_state, force_calc = T, .parallel = F) # ~1.5 hours
  # This is okay to run in parallel as it doesn't load/process any data
# plyr::l_ply(1982:2019, MHW_annual_state, force_calc = F, .parallel = T) # ~ 1 minute
MHW_annual_state(2020, force_calc = T)


# Animations --------------------------------------------------------------

# setwd("figures") # Need to change working directory for animation code to be able to access png files
# system.time(system("convert -delay 100 *.png ../anim/MHW_cat_summary.mp4")) # 139 seconds
# setwd("../")

