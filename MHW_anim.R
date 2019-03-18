# MHW_anim.R
# The purpose of this script currently is to make animations of the 
# three reference time series that are pacakge in heatwaveR.
# it does this based on the daily category files used in the MHW Tracker



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(gganimate, lib.loc = "../R-packages/")
library(heatwaveR, lib.loc = "../R-packages/")
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
  unnest(cat) 


# Determine MHW of choice -------------------------------------------------



# Decide on bounding box --------------------------------------------------

#

# Create animation --------------------------------------------------------

gb <- ggplot(place_holder, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  coord_cartesian(xlim = c(10, 40), ylim = c(-40, -25), expand = 0)
gb

gb + geom_raster(data = MHW_cat_clim_sub, aes(fill = category)) +
  scale_fill_manual(values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000")) +
  # coord_cartesian(xlim = c(10, 40), ylim = c(-40, -25), expand = 0) +
  labs(title = 'Date: {frame_time}', x = '', y = '') +
  transition_time(t)

  
  
ggplot(MHW_cat_clim_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = category)) +
  # borders(fill = "grey70", colour = "black") +
  # scale_fill_manual(values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000")) +
  # coord_cartesian(xlim = c(10, 40), ylim = c(-40, -25), expand = 0) +
  labs(title = 'Date: {frame_time}', x = '', y = '') +
  transition_time(t)
# )
anim_save(filename = "MHW_cat_anim.gif", path = "anim")

ggplot(place_holder, aes(x = lon, y = lat)) +
  # geom_raster(data = MHW_cat_clim_sub, aes(fill = category)) +
  borders(fill = "grey70", colour = "black") +
  # scale_fill_manual(values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000")) +
  coord_cartesian(xlim = c(10, 40), ylim = c(-40, -25), expand = 0) +
  labs(title = 'Date: {frame_time}', x = '', y = '') +
  transition_time(t)

