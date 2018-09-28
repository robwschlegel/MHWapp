
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(gganimate)
library(padr)


# Load data ---------------------------------------------------------------

# load("data/MHW_res.RData")
load("data/MHW_cat.RData")

# The climatologies
# MHW_clim <- MHW_res %>% 
#   unnest(event) %>% 
#   filter(row_number() %% 2 == 1) %>% 
#   unnest(event) %>% 
#   select(-(threshCriterion:event))
# rm(MHW_res)

MHW_cat_clim_short <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(cat)

MHW_cat_clim_long <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(cat) %>%
  group_by(lon, lat) %>%
  nest() %>%
  mutate(long = map(data, pad, interval = "day")) %>%
  select(-data) %>%
  unnest(long) %>%
  ungroup()
rm(MHW_cat)

MHW_cat_clim_sub <- MHW_cat_clim_long %>% 
  filter(t %in% seq(as.Date("2016-01-01"), as.Date("2016-01-15"), by = "day")) #%>% 
  # mutate(category = factor(category, levels = c("I Moderate", "II Strong", 
  #                                               "III Severe", "IV Extreme")))

date_holder <- data.frame(index = "x", 
                          t = unique(MHW_cat_clim_sub$t)) %>% 
  nest(t)

place_holder <- MHW_cat_clim_sub %>% 
  select(lon, lat) %>% 
  unique() %>% 
  mutate(t = date_holder$data) %>% 
  unnest(t)
  

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

