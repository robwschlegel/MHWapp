# MHW_published.R
# The purpose of this script is to house the code used to load results from published work
# It also houses code that emulates results from published work when the results themselves
# do not mesh well into the format of the MHW Tracker
# All of the files produced in this script are then made available on the MHW Tracker in an
# interactive capacity


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(reticulate)
np <- import("numpy")

lon_OISST <- c(seq(0.125, 179.875, by = 0.25), seq(-179.875, -0.125, by = 0.25))
lat_OISST <- seq(-89.875, 89.875, by = 0.25)


# Oliver et al. 2018 (Nature) ---------------------------------------------

# Load python file
npz1 <- np$load("../../oliver/data/MHW/Trends/mhw_census.2019.npz")

npz1$files

# Function for extracting a layer
layer_prep <- function(data_layer){
  res <- as.data.frame(npz1$f[[data_layer]]) %>% 
    `colnames<-`(lon_OISST) %>% 
    mutate(lat = lat_OISST) %>% 
    reshape2::melt(id = "lat", variable.name = "lon", value.name = "val") %>% 
    mutate(lon = as.numeric(as.character(lon)),
           val = replace_na(val, NA),
           var = {{data_layer}}) %>% 
    dplyr::select(lon, lat, var, val)
  return(res)
}

# Function for plotting a layer
layer_plot <- function(df){
  ggplot(df, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = df$val)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    coord_cartesian(expand = F) +
    labs(x = NULL, y = NULL, fill = df$var[1])
}

# View data layers
npz1_layers <- npz1$files

# Extract all layers
Oliver_2018 <- plyr::ldply(.data = npz1_layers, .fun = layer_prep)

# Extract single layers
test <- layer_prep(npz1_layers[1])
max_trend <- layer_prep("MHW_max_tr")
dur_trend <- layer_prep("MHW_dur_tr")
mean_trend <- layer_prep("MHW_mean_tr")

# Visualise layers
layer_plot(max_trend)
layer_plot(dur_trend)
layer_plot(mean_trend)

