# MHW_annual_summary.R
# The purpose of this script is to perform an annual summary of the global state of MHWs
# 1: Setup the environment
# 2: Functions used in the following steps
# 3: The full annual summary analysis
# 4: An additional count summary of each category
# 5: Total summary of all years
# 6: Comparisons of the annual summaries of multiple products
# 7: Animations (not much here yet)


# 1: Setup ----------------------------------------------------------------

# source("MHW_daily_functions.R")
library(dtplyr)

# Set number of cores
registerDoParallel(cores = 50)

# Animation libraries
# library(animation)
# library(magick)


# 2: Functions ------------------------------------------------------------

# Function that loads a MHW cat file but includes the date from the file name
readRDS_date <- function(file_name){
  file_date <- sapply(str_split(file_name, "/"), "[[", 5)
  file_date <- as.Date(sapply(str_split(file_date, "[.]"), "[[", 3))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date)
}

# Function for finding the first date of the highest category MHW per pixel
max_event_date <- function(df){
  df %>% 
    group_by(lat) %>% 
    filter(as.integer(category) == max(as.integer(category))) %>% 
    filter(t == min(t)) %>% 
    ungroup()
}


# 3: Full analysis --------------------------------------------------------

# testers...
# product <- "OISST"
# chosen_year <- 2020
# chosen_clim <- "1982-2011"
# chosen_clim <- "1992-2018"
# MHW <- F; force_calc <- T; database <- F
event_annual_state <- function(chosen_year, product, chosen_clim, MHW = T, force_calc = F, database = F){
  
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
  }
  
  print(paste0("Started run on ",product," ",event_type," (", 
               chosen_clim,"): ", chosen_year," at ",Sys.time()))
  
  ## Find file location
    # NB: The database files are created in 'event_database.R'
  if(!MHW){
    event_cat_files <- dir(paste0("../data/cat_clim/MCS/",chosen_year), full.names = T)
  } else if(database){
    event_cat_files <- dir(paste0("../data/",product,"_cat/", chosen_year), 
                           full.names = T, pattern = chosen_clim)
  } else {
    event_cat_files <- dir(paste0("../data/cat_clim/",chosen_year), full.names = T)
  }

  # print(paste0("There are currently ",length(event_cat_files)," days of data for ",chosen_year))
  
  ## Create figure title
  if(length(event_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  product_name <- product
  if(product == "OISST") product_name <- "NOAA OISST"
  fig_title <- paste0(event_type," categories of ",chosen_year, extra_bit,
                      "\n",product_name,"; Climatogy period: ",chosen_clim)
  
  ## Load data
  if(force_calc){
    # system.time(
    event_cat <- map_df(event_cat_files, readRDS) #%>% 
    # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
    # na.omit()
    # ) # 5 seconds
  }
  
  ## Process data
  # Max category per pixel
  if(file.exists(paste0("data/annual_summary/",product,event_file,"_cat_pixel_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    event_cat_pixel <- readRDS(paste0("data/annual_summary/",product,event_file,"_cat_pixel_",
                                chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Filtering out the max category at each pixel and counting sum of intensity; ~14 seconds"))
    
    event_intensity <- event_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity), .groups = "drop")

    # system.time(
    event_cat_pixel <- event_cat %>% 
      dplyr::select(-event_no) %>%
      na.omit() %>% 
      group_by(lon) %>% 
      nest() %>% 
      mutate(data = map(data, max_event_date)) %>% 
      unnest(data) %>% 
      distinct() %>%
      left_join(event_intensity, by = c("lon", "lat"))
    # ) # 29 seconds
    saveRDS(event_cat_pixel, file = paste0("data/annual_summary/",product,event_file,"_cat_pixel_",
                                           chosen_clim,"_",chosen_year,".Rds"))
    if(product == "OISST" & chosen_clim == "1982-2011"){
      saveRDS(event_cat_pixel, file = paste0("data/annual_summary/",event_type,"_cat_pixel_",chosen_year,".Rds"))
    }
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/annual_summary/",product,event_file,"_cat_daily_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    event_cat_daily <- readRDS(paste0("data/annual_summary/",product,event_file,
                                      "_cat_daily_",chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Counting the daily + cumulative categories per day; ~3 seconds"))
    
    # Complete dates by categories data.frame
    ## This will need to be changed if the ice category is introduced
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(event_cat$t), by = "day"), 
                             category = as.factor(levels(event_cat$category))) %>% 
      mutate(category = factor(category, levels = levels(event_cat$category)))
    
    # system.time(
    event_cat_first <- event_cat_pixel %>%
      right_join(lon_lat_OISST_area, by = c("lon", "lat")) %>% 
      group_by(t, category) %>%
      # count(category) %>%
      # dplyr::rename(first_n = n) %>% 
      # ungroup() %>% 
      summarise(first_n = n(),
                first_area = sum(sq_area), .groups = "drop") %>% 
      right_join(full_grid, by = c("t", "category")) %>%
      mutate(first_n = ifelse(is.na(first_n), 0, first_n),
             first_n_prop = round(first_n/nrow(OISST_ocean_coords), 4),
             first_area = ifelse(is.na(first_area), 0, first_area),
             first_area_prop = round(first_area/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
      arrange(t, category) %>% 
      group_by(category) %>%
      mutate(first_n_cum = cumsum(first_n),
             first_area_cum = cumsum(first_area),
             first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
             first_area_cum_prop = round(first_area_cum/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      ungroup()
    # ) # 1 second
    
    # system.time(
    event_cat_daily <- event_cat %>% 
      right_join(lon_lat_OISST_area, by = c("lon", "lat")) %>% 
      group_by(t, category) %>% 
      # count(category) %>% 
      # ungroup() %>% 
      summarise(cat_n = n(),
                cat_area = sum(sq_area), .groups = "drop") %>% 
      right_join(full_grid, by = c("t", "category")) %>% 
      # dplyr::rename(cat_n = n) %>% 
      mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n),
             cat_n_prop = round(cat_n/nrow(OISST_ocean_coords), 4),
             cat_area = ifelse(is.na(cat_area), 0, cat_area),
             cat_area_prop = round(cat_area/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      arrange(t, category) %>% 
      group_by(category) %>% 
      mutate(cat_n_cum = cumsum(cat_n),
             cat_area_cum = cumsum(cat_area),
             cat_n_cum_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4),
             cat_area_cum_prop = round(cat_area_cum/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      ungroup() %>% 
      right_join(event_cat_first, by = c("t", "category"))
    # ) # 7 second
    saveRDS(event_cat_daily, file = paste0("data/annual_summary/",product,event_file,"_cat_daily_",
                                           chosen_clim,"_",chosen_year,".Rds"))
    if(product == "OISST" & chosen_clim == "1982-2011"){
      saveRDS(event_cat_daily, file = paste0("data/annual_summary/",event_type,"_cat_daily_",chosen_year,".Rds"))
    }
  }
  
  # Extract small data.frame for easier labeling
  event_cat_daily_labels <- event_cat_daily %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_area_cum = cumsum(first_area_cum_prop)) %>% 
    filter(first_area_cum != 0)
  
  ## Create figures
  # print("Creating figures")
  
  # Global map of event occurrence
  fig_map <- ggplot(event_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = event_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                         max(OISST_ocean_coords$lat))) +
    theme_void() +
    guides(fill = guide_legend(override.aes = list(size = 10))) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # fig_map
   
  # Stacked barplot of global daily count of MHWs by category
  fig_count <- ggplot(event_cat_daily, aes(x = t, y = cat_area_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = paste0("Daily ",event_type," coverage for ocean\n(non-cumulative)"), 
         x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(event_cat_daily, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = event_cat_daily_labels, show.legend = F,
               aes(yintercept = label_first_area_cum, colour = category)) +
    scale_fill_manual("Category", values = event_colours) +
    scale_colour_manual("Category", values = event_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = paste0("Top ",event_type," category for ocean\n(cumulative)"), 
         x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(event_cat_daily, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(breaks = round(seq(sum(event_cat_daily_labels$cat_area_cum_prop)*0.25,
                                      sum(event_cat_daily_labels$cat_area_cum_prop)*0.75, length.out = 3), 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = paste0("Average ",event_type," days for ocean\n(cumulative)"), 
         x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_prop
  
  # print("Combining figures")
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
                               labels = c("A)"), common.legend = T, legend = "bottom",
                               font.label = list(size = 16))
  
  # Standard caption technique
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
  
  # print("Saving final figure")
  ggsave(fig_ALL_cap, height = 12, width = 18, 
         filename = paste0("figures/",product,event_file,"_cat_summary_", chosen_clim,"_",chosen_year,".png"))
  # ggsave(fig_ALL_cap, height = 12, width = 18, 
         # filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".pdf")) # looks bad...
  
  # print(paste0("Finished run on ",product, "(", 
  #              chosen_clim,"): ", chosen_year," at ",Sys.time()))
}

# Run the current year
## MHW
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())), 
                   product = "OISST", chosen_clim = "1982-2011", force_calc = T) # 161 seconds
## MCS
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())), MHW = F,
                   product = "OISST", chosen_clim = "1982-2011", force_calc = T) # 161 seconds
# event_annual_state(2020, product = "OISST", chosen_clim = "1982-2011", force_calc = T)

# Run ALL years
### OISST
## MHW
# plyr::l_ply(1982:2019, event_annual_state, force_calc = T, .parallel = T,
#             product = "OISST", chosen_clim = "1982-2011") # ~90 seconds for one
# plyr::l_ply(1982:2019, event_annual_state, force_calc = T, database = T, .parallel = T,
#             product = "OISST", chosen_clim = "1992-2018")
## MCS
# plyr::l_ply(1982:2020, event_annual_state, MHW = F, force_calc = T, .parallel = T,
#             product = "OISST", chosen_clim = "1982-2011")

### CCI
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1982-2011") # ~50 seconds for one
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1992-2018")

### CMC
# plyr::l_ply(1992:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CMC", chosen_clim = "1992-2018")


# 4: Annual sum of MHW categories per pixel -------------------------------

# Create a load option for southern hemisphere, July - June
# and one for Northern Hemisphere, January - December
# NB: This is old code from the previous pipeline...

# chosen_year <- 2013
# hemisphere <- "S"
MHW_annual_count <- function(chosen_year, hemisphere){
  
  print(paste0("Started run on ",chosen_year," at ",Sys.time()))
  
  ## Decide on files based on hemisphere
  if(hemisphere == "N"){
    MHW_cat_files <- dir(paste0("../data/cat_clim/", chosen_year), full.names = T)
  } else if(hemisphere == "S"){
    MHW_cat_files <- c(dir(paste0("../data/cat_clim/", chosen_year), full.names = T),
                       dir(paste0("../data/cat_clim/", chosen_year+1), full.names = T))
    if(chosen_year == 2019){ # This will need to be updated once July 1st, 2020 data are available
      MHW_cat_files <- MHW_cat_files[grep("07-01", MHW_cat_files)[1]:length(MHW_cat_files)]
    } else {
      MHW_cat_files <- MHW_cat_files[grep("07-01", MHW_cat_files)[1]:grep("06-30", MHW_cat_files)[2]]
    }
  }
  
  ## Load data
  MHW_cat <- plyr::ldply(MHW_cat_files, readRDS_date, .parallel = T) 
  
  ## Summarise
  # system.time(
  MHW_cat_count <- lazy_dt(MHW_cat) %>% 
    group_by(lon, lat, event_no) %>% 
    summarise(max_cat = max(as.integer(category))) %>% 
    data.frame() %>% 
    dplyr::select(-event_no) %>% 
    mutate(max_cat = factor(max_cat, levels = c(1:4),  labels = levels(MHW_cat$category))) %>% 
    group_by(lon, lat) %>% 
    table() %>% 
    as.data.frame() %>% 
    pivot_wider(values_from = Freq, names_from = max_cat) %>% 
    mutate(lon = as.numeric(as.character(lon)),
           lat = as.numeric(as.character(lat)))
  # ) # 16 seconds
  saveRDS(MHW_cat_count, paste0("data/annual_summary/MHW_cat_count_",hemisphere,"_", chosen_year,".Rds"))
  # write_csv(MHW_cat_count, paste0("data/annual_summary/MHW_cat_sum_",hemisphere,"_", chosen_year,".csv"))
}

# Run them all
# plyr::l_ply(2015:2019, MHW_annual_count, .parallel = F, hemisphere = "S")
# plyr::l_ply(2016:2020, MHW_annual_count, .parallel = F, hemisphere = "N")

# test visuals
# MHW_cat_count <- readRDS("data/annual_summary/MHW_cat_count_S_2014.Rds")
# ggplot(data = MHW_cat_count, aes(x = lon, y = lat, fill = `I Moderate`)) +
#   geom_tile()


# 5: Total summary of all years -------------------------------------------

# testers...
# product <- "OISST"
# chosen_clim <- "1982-2011"
# chosen_clim <- "1992-2018"
# MHW <- F
event_total_state <- function(product, chosen_clim, MHW = T){
  
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
  }
  
  # Create mean values of daily count
  cat_daily_files <- dir("data/annual_summary", full.names = T,
                         pattern = paste0(product,event_file,"_cat_daily_",chosen_clim))
  cat_daily_files <- cat_daily_files[!grepl("total", cat_daily_files)]
  cat_daily_mean <- map_dfr(cat_daily_files, readRDS) %>%
    mutate(t = lubridate::year(t)) %>%
    group_by(t, category) %>% 
    summarise(cat_area_prop_mean = mean(cat_area_prop, na.rm = T), .groups = "drop") %>% 
    filter(t <= lubridate::year(Sys.Date())) # Use this to not include the current partial year
  
  # Extract only values from December 31st
  cat_daily <- map_dfr(cat_daily_files, readRDS) %>%
  # cat_daily <- map_dfr(dir("data/annual_summary/v2.0", pattern = "cat_daily",
  # full.names = T), readRDS) %>% # The old v2.0 OISST data
    # group_by(lubridate::year(t)) %>% 
    # filter(t == max(t)) %>% 
    # filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    mutate(t = lubridate::year(t)) %>% #,
           # first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
    left_join(cat_daily_mean, by = c("t", "category"))# %>% 
    # filter(t <= lubridate::year(Sys.Date())) # Use this to not include the current partial year
  
  # Save and exit
  saveRDS(cat_daily, paste0("data/annual_summary/",product,event_file,
                            "_cat_daily_", chosen_clim,"_total.Rds"))
}

## Run them all
# OISST
# event_total_state("OISST", "1982-2011")
# event_total_state("OISST", "1992-2018")
# event_total_state("OISST", "1982-2011", MHW = F)
# CCI
# event_total_state("CCI", "1982-2011")
# event_total_state("CCI", "1992-2018")
# CMC
# event_total_state("CMC", "1992-2018")

event_total_state_fig <- function(df, product = "OISST", chosen_clim = "1982-2011", MHW = T){
  
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
    event_limits <- c(0, 65)
    event_breaks <- seq(5, 60, by = 5)
    second_breaks <- c(7.3, 14.6, 21.9, 29.2, 36.5, 43.8, 51.1, 58.4)
    second_break_labels <- c("2%", "4%", "6%", "8%", "10%", "12%", "14%", "16%")
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
    event_limits <- c(0, 27)
    event_breaks <- seq(5, 25, by = 5)
    second_breaks <- c(7.3, 14.6, 21.9)
    second_break_labels <- c("2%", "4%", "6%")
  }
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count_historic <- ggplot(df, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    # If the ice category is added
    # geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
    #                  aes(pattern_colour = hemi, colour = hemi), 
    #                  pattern = "stripe", pattern_fill = NA, fill = NA, 
    #                  pattern_density = 1, pattern_size = 0.6) +
    # scale_colour_manual(values = c("lightpink", "plum")) +
    # scale_pattern_colour_manual(values = c("lightpink", "plum")) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(limits = event_limits,
                       breaks = event_breaks,
                       sec.axis = sec_axis(name = paste0("Average daily ",event_type," coverage"), 
                                           trans = ~ . + 0,
                                           breaks = second_breaks,
                                           labels = second_break_labels)) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(y = paste0("Average ",event_type," days"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  fig_count_historic

  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum_historic <- ggplot(df, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    labs(y = paste0("Total ",event_type," coverage"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  fig_cum_historic

  # Create the figure title
  product_title <- product
  if(product == "OISST") product_title <- "NOAA OISST"
  min_year <- min(df$t)
  max_year <- max(df$t)
  clim_title <- gsub("-", " - ", chosen_clim)
  fig_title <- paste0(event_type," category summaries: ",min_year," - ",max_year,
                      "\n",product_title,"; Climatogy period: ",clim_title)
  
  # Stick them together and save
  fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic,
                                        ncol = 2, align = "hv", labels = c("A)", "B)"), #hjust = -0.1,
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 16))
  fig_ALL_full <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_historic, heights = c(0.2, 1), nrow = 2)
  ggsave(fig_ALL_full, filename = paste0("figures/",product,event_file,"_cat_historic_",chosen_clim,".png"), height = 4.25, width = 8)
  # ggsave(fig_ALL_full, filename = paste0("figures/",product,"_cat_historic_",chosen_clim,".eps"), height = 4.25, width = 12)
}

## Run them all
# OISST
event_total_state_fig(readRDS("data/annual_summary/OISST_cat_daily_1982-2011_total.Rds"))
event_total_state_fig(readRDS("data/annual_summary/OISST_cat_daily_1992-2018_total.Rds"), chosen_clim = "1992-2018")
event_total_state_fig(readRDS("data/annual_summary/OISST_MCS_cat_daily_1982-2011_total.Rds"), MHW = F)
# CCI
# CCI_1982_2011 <- readRDS("data/annual_summary/CCI_cat_daily_1982-2011_total.Rds")
# event_total_state_fig(CCI_1982_2011, "CCI", "1982-2011")
# CCI_1992_2018 <- readRDS("data/annual_summary/CCI_cat_daily_1992-2018_total.Rds")
# event_total_state_fig(CCI_1992_2018, "CCI", "1992-2018")
# CMC
# CMC_1992_2018 <- readRDS("data/annual_summary/CMC_cat_daily_1992-2018_total.Rds")
# event_total_state_fig(CMC_1992_2018, "CMC", "1992-2018")

# Look at a total sum
# OISST_1982_2011_sum <- OISST_1982_2011 %>%
#   group_by(t) %>%
#   summarise_if(is.numeric, sum)


# 6: Comparisons ----------------------------------------------------------

# Load the annual summaries
# ann_sum_OISST <- readRDS("data/annual_summary/OISST_cat_daily_1992-2018_total.Rds") %>% 
#   mutate(product = "OISST")
# ann_sum_CCI <- readRDS("data/annual_summary/CCI_cat_daily_1992-2018_total.Rds") %>% 
#   mutate(product = "CCI")
# ann_sum_CMC <- readRDS("data/annual_summary/CMC_cat_daily_1992-2018_total.Rds") %>% 
#   mutate(product = "CMC")

# Combine for further analyses
# ann_sum_ALL <- rbind(ann_sum_OISST, ann_sum_CCI, ann_sum_CMC) %>% 
#   filter(t >= 1992, t <= 2018)

# Highest total daily occurrences; cat_prop_daily_mean
# ann_sum_daily <- ann_sum_ALL %>% 
#   group_by(product, category) %>% 
#   summarise(cat_daily_mean = round(mean(cat_prop_daily_mean), 5)*100)

# Highest total ocean coverage; first_n_cum_prop
# ann_sum_cover <- ann_sum_ALL %>% 
#   group_by(product, category) %>% 
#   summarise(first_n_cum = round(mean(first_n_cum_prop), 5)*100)

# Highest MHW days per pixel; cat_n_prop
# ann_sum_days <- ann_sum_ALL %>% 
#   group_by(product, category) %>% 
#   summarise(cat_n = round(mean(cat_n_prop), 5)*100)


# 7: Animations -----------------------------------------------------------

# setwd("figures") # Need to change working directory for animation code to be able to access png files
# system.time(system("convert -delay 100 *.png ../anim/MHW_cat_summary.mp4")) # 139 seconds
# setwd("../")

