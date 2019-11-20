# MHW_annual_summary
# The purpose of this script is to perform an annual summary
# of the global state of MHWs


# Setup -------------------------------------------------------------------

# .libPaths(c("~/R-packages", .libPaths()))

library(tidyverse)
library(heatwaveR)
library(tidync, lib.loc = "../R-packages/")
library(dtplyr, lib.loc = "../R-packages/")
library(doParallel); registerDoParallel(cores = 50)


# Metadata ----------------------------------------------------------------

# The OISST data location
OISST_files <- dir("../data/OISST", full.names = T, pattern = "avhrr")

# Function for extracting one day of data
# testers...
# index_val <- 20
# file_name <- OISST_files[1]
extract_OISST_one <- function(index_val){
  file_name <- OISST_files[index_val]
  res <- tidync(file_name) %>% 
    hyper_filter(time = time == 18000) %>% 
    hyper_tibble() %>% 
    select(-time, -sst)
}

# Get coords for each ocean pixel 
# system.time(OISST_ocean_coords <- plyr::ldply(1:1440, extract_OISST_one, .parallel = T)) # 9 seconds
# save(OISST_ocean_coords, file = "metadata/OISST_ocean_coords.Rdata")
load("metadata/OISST_ocean_coords.Rdata")

# Visualise
# ggplot(OISST_ocean_coords, aes(x = lon, y = lat)) + geom_tile()

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


# Data --------------------------------------------------------------------

# Chosen year of MHWs
MHW_cat_files <- dir("../data/cat_clim/2019", full.names = T)
length(MHW_cat_files) # 322

# Function that loads a MHW cat file but includes the date from the file name
readRDS_date <- function(file_name){
  file_date <- sapply(str_split(file_name, "/"), "[[", 5)
  file_date <- as.Date(sapply(str_split(file_date, "[.]"), "[[", 3))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date)
}

# Load into one file
system.time(MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date, .parallel = T)) # 12 seconds


# Map ---------------------------------------------------------------------

# Filter out the max intensity at each pixel
system.time(
MHW_cat_max <- lazy_dt(MHW_cat) %>% 
  select(-event_no, -t, -intensity) %>% 
  group_by(lon, lat) %>% 
  filter(as.integer(category) == max(as.integer(category))) %>%
  unique() %>% 
  data.frame()
) # 68 seconds

# Visalise
ggplot(OISST_ocean_coords, aes(x = lon, y = lat)) +
  geom_tile(fill = "white", colour = NA) +
  geom_tile(data = MHW_cat_max, aes(fill = category)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  scale_fill_manual("Category", values = MHW_colours) +
  coord_cartesian(expand = F) +
  theme_void() +
  theme(legend.position = "bottom") +
  ggtitle("MHW categories of 2019 (so far)", subtitle = "NOAA OISST; Climatogy period: 1982 - 2011")
# ggsave("figures/MHW_cat_map_2019.png", height = 12, width = 24)


# Time series -------------------------------------------------------------

## Daily occurrence
# Daily MHW values
system.time(
MHW_cat_daily <- MHW_cat %>% 
  group_by(t) %>% 
  mutate(itensity_daily = sum(intensity)) %>% 
  group_by(t, itensity_daily) %>% 
  count(category)
)

# Stacked barplots
ggplot(MHW_cat_daily, aes(x = t, y = n)) +
  geom_bar(aes(fill = category), stat = "identity",
           position = position_stack(reverse = TRUE), width = 1) +
  scale_fill_manual("Category", values = MHW_colours) +
  scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords))) +
  labs(x = NULL, y = "Daily count of MHWs (global pixel)")

# Cumulative occurrence
MHW_cat_daily_cum <- MHW_cat_daily %>% 
  group_by(category) %>% 
  mutate(n_cum = cumsum(n))

# Stacked barplots
ggplot(MHW_cat_daily_cum, aes(x = t, y = n_cum)) +
  geom_bar(aes(fill = category), stat = "identity",
           position = position_stack(reverse = TRUE), width = 1) +
  scale_fill_manual("Category", values = MHW_colours) +
  # scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords))) +
  labs(x = NULL, y = "Cumulative count of MHWs (global pixel)")


# Animations --------------------------------------------------------------


