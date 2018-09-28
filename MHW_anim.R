
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(gganimate)
library(padr)


# Load data ---------------------------------------------------------------

load("data/MHW_res.RData")
load("data/MHW_cat.RData")

# The climatologies
MHW_clim <- MHW_res %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(event) %>% 
  select(-(threshCriterion:event))
rm(MHW_res)

MHW_cat_clim <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(cat) %>% 
  select(-(threshCriterion:event)) %>% 
  group_by(lon, lat) %>% 
  nest() %>% 
  mutate(long = map(data, pad, interval = "day"))
rm(MHW_cat)


# Create animation --------------------------------------------------------

ggplot(MHW_cat_clim, aes(lon, lat)) +
  geom_raster(aes(fill = category)) +
  labs(title = 'Year: {frame_time}', x = '', y = '') +
  transition_time(t)

