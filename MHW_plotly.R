
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(plotly)


# Load data ---------------------------------------------------------------

load("data/MHW_res.RData")
load("data/MHW_cat.RData")


# Prep data ---------------------------------------------------------------

# The categories
MHW_cat <- MHW_cat %>% 
  unnest(cat)

# The event metrics
MHW_event <- MHW_res %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(event)

# The climatologies
MHW_clim <- MHW_res %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(event)


# Visualise ---------------------------------------------------------------

test <- MHW_cat %>% 
  filter(event_no == 1)

tp <- ggplot(data = test, aes(x = lon, y = lat, fill = category)) +
  geom_raster()
tp

ggplotly(tp)
