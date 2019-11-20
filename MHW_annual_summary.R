# MHW_annual_summary
# The purpose of this script is to perform an annual summary
# of the global state of MHWs


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))

library(tidyverse)
library(heatwaveR)
library(tidync, lib.loc = "../R-packages/")
library(dtplyr, lib.loc = "../R-packages/")
# library(ggrepel)
library(doParallel); registerDoParallel(cores = 50)


# Metadata ----------------------------------------------------------------

# The OISST data location
# OISST_files <- dir("../data/OISST", full.names = T, pattern = "avhrr")

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

# Chosen year for analysis
# chosen_year <- 2019


# Functions ---------------------------------------------------------------

# Function that loads a MHW cat file but includes the date from the file name
readRDS_date <- function(file_name){
  file_date <- sapply(str_split(file_name, "/"), "[[", 5)
  file_date <- as.Date(sapply(str_split(file_date, "[.]"), "[[", 3))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date)
}


# Full analysis -----------------------------------------------------------

MHW_annual_state <- function(chosen_year){
  
  print(paste0("Started run on ",chosen_year," at ",Sys.time()))
  
  ## Find file location
  MHW_cat_files <- dir(paste0("../data/cat_clim/", chosen_year), full.names = T)
  print(paste0("There are currently ",length(MHW_cat_files)," days of data for ",chosen_year)) # 322
  
  ## Create figure title
  if(length(MHW_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  fig_title <- paste0("MHW categories of ",chosen_year, extra_bit,
                      "\nNOAA OISST; Climatogy period: 1982 - 2011")
  
  ## Load data
  print(paste0("Loading ",chosen_year," MHW category data; ~12 seconds"))
  system.time(MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date, .parallel = T)) # 12 seconds
  
  ## Process data
  print(paste0("Filtering out the max category at each pixel; ~70 seconds"))
  # system.time(
  MHW_cat_max <- lazy_dt(MHW_cat) %>% 
    select(-event_no, -t, -intensity) %>% 
    group_by(lon, lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>%
    unique() %>% 
    data.frame()
  # ) # 70 seconds
  
  print(paste0("Counting the daily + cumulative categories globally; ~2 seconds"))
  # system.time(
  MHW_cat_daily_cum <- MHW_cat %>% 
    group_by(t) %>% 
    # mutate(itensity_daily = sum(intensity)) %>% 
    # group_by(t, itensity_daily) %>% 
    count(category) %>% 
    group_by(category) %>% 
    mutate(n_cum = cumsum(n),
           n_prop = n_cum/nrow(OISST_ocean_coords))
  # ) # 2 seconds
  
  print(paste0("Finding the earliest occurrence of the largest MHW per pixel; ~180 seconds"))
  # system.time(
  MHW_cat_single <- lazy_dt(MHW_cat) %>% 
    # slice(1:2000, 20000:20100) %>%
    # filter(lon == 0.125) %>%
    group_by(lon, lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>% 
    filter(t == min(t)) %>% 
    group_by(t) %>%
    count(category) %>%
    ungroup() %>% 
    arrange(t) %>% 
    group_by(category) %>%
    mutate(n_cum = cumsum(n)) %>% 
    data.frame()
  # ) # 180 seconds
  
  # Fix cumulative counts so there are no gaps
  fix_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(MHW_cat$t), by = "day"), 
                          category = as.factor(levels(MHW_cat_single$category))) %>% 
    mutate(category = factor(category, levels = levels(MHW_cat_single$category)))
  
  # Merge
  MHW_cat_single_fix <- MHW_cat_single %>% 
    right_join(fix_grid, by = c("t", "category")) %>% 
    group_by(category) %>% 
    fill(n_cum, .direction = "down")
  
  # Extract small data.frame for easier labelling
  MHW_cat_single_fix_labels <- MHW_cat_single_fix %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_cum = cumsum(n_cum))
  
  ## Create figures
  print("Creating figures")
  
  # Global map of MHW occurrence
  fig_map <- ggplot(MHW_cat_max, aes(x = lon, y = lat)) +
    # geom_tile(fill = "grey80", colour = NA) +
    geom_tile(aes(fill = category)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MHW_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                         max(OISST_ocean_coords$lat))) +
    theme_void() +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "grey80")) #+
    # ggtitle("MHW categories of 2019 (so far)", subtitle = "NOAA OISST; Climatogy period: 1982 - 2011")
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count <- ggplot(MHW_cat_daily_cum, aes(x = t, y = n)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
                       breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
                       labels = paste0(seq(0, 100, by = 10), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(x = NULL, y = "Daily count of MHWs") +
    coord_cartesian(expand = F) #+
    # theme(legend.position = "bottom") 
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MHW_cat_single_fix, aes(x = t, y = n_cum)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MHW_cat_single_fix_labels, show.legend = F,
               aes(yintercept = label_cum, colour = category)) +
    # geom_label_repel(data = MHW_cat_daily_cum_single_fix_labels,
    #                  aes(y = label_cum, x = as.Date("2019-03-01"), 
    #                      label = category, fill = category)) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_colour_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, nrow(OISST_ocean_coords)),
                       breaks = seq(0, nrow(OISST_ocean_coords), length.out = 11),
                       labels = paste0(seq(0, 100, by = 10), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(x = NULL, y = "Cumulative occurrence of largest MHW",
         caption = "") +
    coord_cartesian(expand = F) #+
    # theme(legend.position = "bottom")
  
  # Stacked barplot of cumulative proportion of ocean affected by MHWs
  fig_prop <- ggplot(MHW_cat_daily_cum, aes(x = t, y = n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    # geom_hline(yintercept = nrow(OISST_ocean_coords)) +
    # geom_label(aes(x = as.Date("2019-07-01"), y = nrow(OISST_ocean_coords), label = "Gobal pixel count")) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(labels = ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(x = NULL, y = "Average MHW days per pixel") +
    coord_cartesian(expand = F) #+
    # theme(legend.position = "bottom") 
  
  print("Combining figures")
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), common.legend = T, legend = "bottom")
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.5),
                               labels = c("A)"), common.legend = T, legend = "bottom")
  
  # Fancy caption technique
  # fig_ALL_cap <-  grid::textGrob(paste0(strwrap(fig_cap, 140), sep = "", collapse = "\n"),
  #                               x = 0.01, just = "left", gp = grid::gpar(fontsize = 10))
  
  # Standard caption technique
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 16))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.05, 1), nrow = 2)
  
  print("Saving final figure")
  ggsave(fig_ALL_cap, filename = paste0("figures/MHW_cat_summary_",chosen_year,".png"), height = 12, width = 18)
  
  print(paste0("Finished run on ",chosen_year," at ",Sys.time()))
}

# Test one year
# system.time(MHW_annual_state(1982)) # 257 seconds
# MHW_annual_state(2019)

# Run ALL years
plyr::l_ply(1982:2019, MHW_annual_state) # ~2.5 hours


# Animations --------------------------------------------------------------


