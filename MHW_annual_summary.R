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
## Only needed when running animations at bottom of script
# library(animation)
# library(magick)
# library(gganimate)


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
# chosen_year <- 2023
# chosen_clim <- "1982-2011"
# chosen_clim <- "1991-2020"
# chosen_clim <- "1992-2018"
# MHW <- T; force_calc <- T; database <- F
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
    event_cat_files <- dir(paste0("../data/cat_clim/MCS/",chosen_year), pattern = ".Rds", full.names = T)
  } else if(database){
    event_cat_files <- dir(paste0("../data/",product,"_cat/", chosen_year), 
                           full.names = T, pattern = chosen_clim)
  } else {
    event_cat_files <- dir(paste0("../data/cat_clim/",chosen_year), pattern = ".Rda", full.names = T)
  }
  event_cat_files <- event_cat_files[grepl(chosen_clim, event_cat_files)]
  # print(paste0("There are currently ",length(event_cat_files)," days of data for ",chosen_year))
  
  ## Load data
  if(force_calc){
    # system.time(
    event_cat <- map_df(event_cat_files, readRDS) #%>% 
    # NB: Filter out ice if desired
    # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  
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
    if(product == "OISST" ){
      saveRDS(event_cat_pixel, file = paste0("data/annual_summary/",event_type,"_cat_pixel_",chosen_clim,"_",chosen_year,".Rds"))
      saveRDS(event_cat_pixel, file = paste0("../data/OISST/annual_summary/",event_type,"_cat_pixel_",chosen_clim,"_",chosen_year,".Rds"))
    }
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/annual_summary/",product,event_file,"_cat_daily_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    event_cat_daily <- readRDS(paste0("data/annual_summary/",product,event_file,
                                      "_cat_daily_",chosen_clim,"_",chosen_year,".Rds"))
  } else {
    # print(paste0("Counting the daily + cumulative categories per day; ~3 seconds"))
    
    # TODO: Add days of MHW/MCS per year per pixel
    
    # Complete dates by categories data.frame
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(event_cat$t), by = "day"), 
                             category = as.factor(levels(event_cat$category))) %>% 
      mutate(category = factor(category, levels = levels(event_cat$category)))
    
    # system.time(
    event_cat_first <- event_cat_pixel %>%
      right_join(OISST_ocean_coords, by = c("lon", "lat")) %>% 
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
             first_area_prop = round(first_area/sum(OISST_ocean_coords$sq_area), 4)) %>% 
      mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
      arrange(t, category) %>% 
      group_by(category) %>%
      mutate(first_n_cum = cumsum(first_n),
             first_area_cum = cumsum(first_area),
             first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
             first_area_cum_prop = round(first_area_cum/sum(OISST_ocean_coords$sq_area), 4)) %>% 
      ungroup()
    # ) # 1 second
    
    # system.time(
    event_cat_daily <- event_cat %>% 
      right_join(OISST_ocean_coords, by = c("lon", "lat")) %>% 
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
             cat_area_prop = round(cat_area/sum(OISST_ocean_coords$sq_area), 4)) %>% 
      arrange(t, category) %>% 
      group_by(category) %>% 
      mutate(cat_n_cum = cumsum(cat_n),
             cat_area_cum = cumsum(cat_area),
             cat_n_cum_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4),
             cat_area_cum_prop = round(cat_area_cum/sum(OISST_ocean_coords$sq_area), 4)) %>% 
      ungroup() %>% 
      right_join(event_cat_first, by = c("t", "category"))
    # ) # 7 second
    saveRDS(event_cat_daily, file = paste0("data/annual_summary/",product,event_file,"_cat_daily_",
                                           chosen_clim,"_",chosen_year,".Rds"))
    if(product == "OISST"){
      saveRDS(event_cat_daily, file = paste0("data/annual_summary/",event_type,"_cat_daily_",chosen_clim,"_",chosen_year,".Rds"))
      saveRDS(event_cat_daily, file = paste0("../data/OISST/annual_summary/",event_type,"_cat_daily_",chosen_clim,"_",chosen_year,".Rds"))
    }
  }
}

# Run the current year
## MHW
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())),
                   product = "OISST", chosen_clim = "1982-2011", force_calc = T) # ~30 seconds
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())),
                   product = "OISST", chosen_clim = "1991-2020", force_calc = T) # ~30 seconds
# event_annual_state(2023, product = "OISST", chosen_clim = "1991-2020", force_calc = T)
## MCS
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())), MHW = F,
                   product = "OISST", chosen_clim = "1982-2011", force_calc = T) # ~30 seconds
event_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())), MHW = F,
                   product = "OISST", chosen_clim = "1991-2020", force_calc = T) # ~30 seconds
# event_annual_state(2023, MHW = F, product = "OISST", chosen_clim = "1991-2020", force_calc = T)

# Run ALL years
## NB: Do not run on more than 25 cores
# registerDoParallel(cores = 25)
### OISST
## MHW
# plyr::l_ply(1982:2023, event_annual_state, force_calc = TRUE, .parallel = TRUE,
#             product = "OISST", chosen_clim = "1991-2020") # ~90 seconds for one
# plyr::l_ply(1982:2023, event_annual_state, force_calc = TRUE, .parallel = TRUE,
#             product = "OISST", chosen_clim = "1982-2011") # ~90 seconds for one
# plyr::l_ply(1982:2019, event_annual_state, force_calc = TRUE, database = TRUE, .parallel = TRUE,
#             product = "OISST", chosen_clim = "1992-2018")
## MCS
# plyr::l_ply(1982:2023, event_annual_state, MHW = FALSE, force_calc = TRUE, .parallel = TRUE,
#             product = "OISST", chosen_clim = "1991-2020") # ~90 seconds for one
# plyr::l_ply(1982:2023, event_annual_state, MHW = FALSE, force_calc = TRUE, .parallel = TRUE,
#             product = "OISST", chosen_clim = "1982-2011")

### CCI
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1982-2011") # ~50 seconds for one
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1992-2018")

### CMC
# plyr::l_ply(1992:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CMC", chosen_clim = "1992-2018")


# 4: Total summary of all years -------------------------------------------

# testers...
# product <- "OISST"
# chosen_clim <- "1982-2011"
# chosen_clim <- "1992-2018"
# chosen_clim <- "1991-2020"
# MHW <- FALSE
event_total_state <- function(product, chosen_clim, MHW = TRUE){
  
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
    filter(t < lubridate::year(Sys.Date())) # Use this to not include the current partial year
  
  # Extract only values from December 31st
  cat_daily <- map_dfr(cat_daily_files, readRDS) %>%
    filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    mutate(t = lubridate::year(t)) %>% #,
    # first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
    left_join(cat_daily_mean, by = c("t", "category"))# %>% 
    # filter(t <= lubridate::year(Sys.Date())) # Use this to not include the current partial year
  
  # Create a slimmed down dataframe to save as CSV for easier external use
  cat_daily_slim <- cat_daily %>% 
    dplyr::select(t, category, cat_n_cum:cat_area_cum_prop, first_n_cum:first_area_cum_prop)
  
  # Save and exit
  saveRDS(cat_daily, paste0("data/annual_summary/",product,event_file,
                            "_cat_daily_",chosen_clim,"_total.Rds"))
  write_csv(cat_daily_slim, paste0("data/annual_summary/",product,event_file,
                                   "_cat_daily_",chosen_clim,"_total.csv"))
}

## Run them all
# OISST
event_total_state("OISST", "1982-2011")
event_total_state("OISST", "1991-2020")
# event_total_state("OISST", "1992-2018")
event_total_state("OISST", "1982-2011", MHW = F)
event_total_state("OISST", "1991-2020", MHW = F)
# CCI
# event_total_state("CCI", "1982-2011")
# event_total_state("CCI", "1992-2018")
# CMC
# event_total_state("CMC", "1992-2018")

# Look at a total sum
# OISST_1991_2020_sum <- OISST_1991_2020 %>%
#   group_by(t) %>%
#   summarise_if(is.numeric, sum)


# 5: Annual summary of categories per pixel -------------------------------

# TODO: This needs to be checked to see if it matches the new 1991-2020 pipeline
# chosen_year <- 2013; hemisphere <- "N"
# chosen_year <- 2015; hemisphere <- "S"
MHW_annual_count <- function(chosen_year, hemisphere){
  
  print(paste0("Started run on ",chosen_year," at ",Sys.time()))
  
  ## Decide on files based on hemisphere
  if(hemisphere == "N"){
    MHW_cat_files <- dir(paste0("../data/cat_clim/", chosen_year), pattern = "Rda", full.names = T)
  } else if(hemisphere == "S"){
    # Need to account for possible leap years when subsetting by index
    MHW_cat_files_pre <- c(dir(paste0("../data/cat_clim/", chosen_year), full.names = T, pattern = "Rda"),
                           dir(paste0("../data/cat_clim/", chosen_year+1), full.names = T, pattern = "Rda"))
    MHW_cat_files <- MHW_cat_files_pre[yday(paste0(chosen_year,"-07-01")):
                                         (yday(paste0(chosen_year,"-12-31"))+yday(paste0(chosen_year+1,"-06-30")))]
    rm(MHW_cat_files_pre)
  }
  
  ## Load data
  MHW_cat <- plyr::ldply(MHW_cat_files, readRDS, .parallel = T) 
  
  ## Summarise
  # system.time(
  MHW_cat_count <- lazy_dt(MHW_cat) %>% 
    group_by(lon, lat, event_no) %>% 
    summarise(max_cat = max(as.integer(category)), .groups = "drop") %>% 
    data.frame() %>% 
    dplyr::select(-event_no) %>% 
    mutate(max_cat = factor(max_cat, levels = c(1:4), labels = levels(MHW_cat$category))) %>% 
    group_by(lon, lat) %>% 
    table() %>% 
    as.data.frame() %>% 
    pivot_wider(values_from = Freq, names_from = max_cat) %>% 
    mutate(lon = as.numeric(as.character(lon)),
           lat = as.numeric(as.character(lat)))
  # ) # 24 seconds
  saveRDS(MHW_cat_count, paste0("data/annual_summary/MHW_cat_count_",hemisphere,"_", chosen_year,".Rds"))
  saveRDS(MHW_cat_count, paste0("../data/OISST/annual_summary/MHW_cat_count_",hemisphere,"_", chosen_year,".Rds"))
  # write_csv(MHW_cat_count, paste0("data/annual_summary/MHW_cat_sum_",hemisphere,"_", chosen_year,".csv"))
}

# Run them all
# NB: Needs up to June 30th of the year for the Southern Hemisphere
# plyr::l_ply(1982:2022, MHW_annual_count, .parallel = F, hemisphere = "S")
# plyr::l_ply(1982:2023, MHW_annual_count, .parallel = F, hemisphere = "N")

# test visuals
# MHW_cat_count <- readRDS("data/annual_summary/MHW_cat_count_S_2014.Rds")
# ggplot(data = MHW_cat_count, aes(x = lon, y = lat, fill = `I Moderate`)) +
#   geom_tile()


# 6: Summary figures ------------------------------------------------------

# Figures per year
event_annual_state_fig <- function(chosen_year, product, chosen_clim, MHW = T){
  
  # Set metadata
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
  }
  
  ## Get daily file count for title
  if(!MHW){
    event_cat_files <- dir(paste0("../data/cat_clim/MCS/",chosen_year), pattern = ".Rds", full.names = T)
  } else {
    event_cat_files <- dir(paste0("../data/cat_clim/",chosen_year), pattern = ".Rda", full.names = T)
  }
  
  ## Create figure title
  if(length(event_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  product_name <- product
  if(product == "OISST") product_name <- "NOAA OISST"
  fig_title <- paste0(event_type," categories of ",chosen_year, extra_bit,
                      "\n",product_name,"; Climatology period: ",chosen_clim)
  
  # Load data
  event_cat_pixel <- readRDS(paste0("data/annual_summary/",product,event_file,"_cat_pixel_",
                                    chosen_clim,"_",chosen_year,".Rds"))
  event_cat_daily <- readRDS(paste0("data/annual_summary/",product,event_file,
                                    "_cat_daily_",chosen_clim,"_",chosen_year,".Rds"))
  event_total <- readRDS(paste0("data/annual_summary/",product,event_file,
                                "_cat_daily_",chosen_clim,"_total.Rds"))
  
  # Get highest category per pixel
  event_cat_pixel_max <- event_cat_pixel |> ungroup() |> 
    summarise(category = max(as.integer(category), na.rm = TRUE), .by = c("lon", "lat")) |> 
    mutate(category = factor(category, labels = levels(event_cat_pixel$category)))
  
  # Extract small data.frame for easier labeling
  event_cat_daily_labels <- event_cat_daily %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_area_cum = cumsum(first_area_cum_prop)) %>% 
    filter(first_area_cum != 0)
  
  # Global map of event occurrence
  fig_map <- ggplot(event_cat_pixel_max, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey60") +
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
  
  # Stacked barplot of global daily  percent of ocean affected by events
  fig_count <- ggplot(event_cat_daily, aes(x = t, y = cat_area_prop)) +
    # This allows total coverage to be plotted in the background
    # geom_bar(aes(y = first_area_cum_prop, fill = category), stat = "identity", show.legend = F,
    #          position = position_stack(reverse = TRUE), width = 1, alpha = 0.6) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1, alpha = 1) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%"),
                       # sec.axis = sec_axis(name = paste0("Total ",event_type," coverage for ocean\n(cumulative)"), 
                       #                     transform = ~ . + 0,
                       #                     breaks = seq(0.2, 0.8, length.out = 4),
                       #                     labels = paste0(seq(20, 80, by = 20), "%"))
                       ) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = paste0("Daily ",event_type," coverage for ocean\n(non-cumulative)"),
         x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  # fig_count
  
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
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  # fig_prop
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum_historic <- ggplot(event_total, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = event_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    labs(y = paste0("Total ",event_type," coverage\n(annual)"), x = "Year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16))
  # fig_cum_historic
  
  # Combine and exit
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_prop, fig_cum_historic, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
                               labels = c("A)"), common.legend = T, legend = "bottom",
                               font.label = list(size = 16))
  
  # Standard caption technique
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2) + 
    ggpubr::bgcolor("white") + ggpubr::border("white")
  
  # print("Saving final figure")
  # NB: PDF looks terrible...
  ggsave(fig_ALL_cap, height = 12, width = 18, 
         filename = paste0("figures/",product,event_file,"_cat_summary_",chosen_clim,"_",chosen_year,".png"))
  # ggsave(fig_ALL_cap, height = 12, width = 18,
  #        filename = paste0("figures/",product,event_file,"_cat_summary_", chosen_clim,"_",chosen_year,".eps"))
}

# Run the current year
## MHW
event_annual_state_fig(chosen_year = as.numeric(lubridate::year(Sys.Date())),
                       product = "OISST", chosen_clim = "1982-2011") # 5 seconds
event_annual_state_fig(chosen_year = as.numeric(lubridate::year(Sys.Date())),
                       product = "OISST", chosen_clim = "1991-2020") # 5 seconds
# event_annual_state_fig(2023, product = "OISST", chosen_clim = "1991-2020")
## MCS
event_annual_state_fig(chosen_year = as.numeric(lubridate::year(Sys.Date())), MHW = F,
                       product = "OISST", chosen_clim = "1982-2011") # 5 seconds
event_annual_state_fig(chosen_year = as.numeric(lubridate::year(Sys.Date())), MHW = F,
                       product = "OISST", chosen_clim = "1991-2020") # 5 seconds
# event_annual_state_fig(2023, product = "OISST", chosen_clim = "1991-2020", MHW = F)

# Run ALL years
### OISST
## MHW
# plyr::l_ply(1982:2023, event_annual_state_fig, .parallel = T,
#             product = "OISST", chosen_clim = "1991-2020")
# plyr::l_ply(1982:2023, event_annual_state_fig, .parallel = T,
#             product = "OISST", chosen_clim = "1982-2011")
# plyr::l_ply(1982:2019, event_annual_state_fig, .parallel = T,
#             product = "OISST", chosen_clim = "1992-2018")
## MCS
# plyr::l_ply(1982:2023, event_annual_state_fig, .parallel = T,
#             product = "OISST", chosen_clim = "1991-2020", MHW = F)
# plyr::l_ply(1982:2023, event_annual_state_fig, .parallel = T,
#             product = "OISST", chosen_clim = "1982-2011", MHW = F)

### CCI
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1982-2011") # ~50 seconds for one
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1992-2018")

### CMC
# plyr::l_ply(1992:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CMC", chosen_clim = "1992-2018")

# Figures of total time series by year
event_total_state_fig <- function(df, product = "OISST", chosen_clim = "1991-2020", MHW = T){
  
  # Get the range needed for the y-axis
  df_sum <- df |> summarise(y_height = sum(cat_area_cum_prop), .by = "t")
  df_y_height <- plyr::round_any(df_sum$y_height[df_sum$y_height == max(df_sum$y_height)], 5)
  df_y_height_365 <- plyr::round_any(round(df_y_height/365, 2), 0.02)
  
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
    event_limits <- c(0, round(df_y_height+1, -1))
    event_breaks <- seq(10, event_limits[2]-10, by = 10)
    second_breaks <- seq(0.04, plyr::round_any(event_limits[2]/365, 0.02), 0.04)*365
    second_break_labels <- paste0(seq(from = 4, by = 4, length.out = length(second_breaks)), "%")
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
    event_limits <- c(0, round(df_y_height+1, -1))
    event_breaks <- seq(5, event_limits[2]-5, by = 5)
    second_breaks <- seq(0.02, plyr::round_any(event_limits[2]/365, 0.02), 0.02)*365
    second_break_labels <- paste0(seq(from = 2, by = 2, length.out = length(second_breaks)), "%")
  }
  
  # Stacked barplot of global daily count of events by category
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
                                           transform = ~ . + 0,
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
  # fig_count_historic
  
  # Stacked barplot of cumulative percent of ocean affected by events
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
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10))
  # fig_cum_historic
  
  # Create the figure title
  product_title <- product
  if(product == "OISST") product_title <- "NOAA OISST"
  min_year <- min(df$t); max_year <- max(df$t)
  clim_title <- gsub("-", " - ", chosen_clim)
  fig_title <- paste0(event_type," category summaries: ",min_year," - ",max_year,
                      "\n",product_title,"; Climatology period: ",clim_title)
  
  # Stick them together and save
  fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic,
                                        ncol = 2, align = "hv", labels = c("A)", "B)"), #hjust = -0.1,
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 16))
  fig_ALL_full <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_historic, heights = c(0.2, 1), nrow = 2) + 
    ggpubr::bgcolor("white") + ggpubr::border("white")
  ggsave(fig_ALL_full, filename = paste0("figures/",product,event_file,"_cat_historic_",chosen_clim,".png"), height = 4.25, width = 8)
  ggsave(fig_ALL_full, filename = paste0("figures/",product,event_file,"_cat_historic_",chosen_clim,".eps"), height = 4.25, width = 8)
  # require(svglite); ggsave(fig_ALL_full, filename = paste0("figures/",product,event_file,"_cat_historic_",chosen_clim,".svg"), height = 4.25, width = 8)
}

## Run them all
# OISST
event_total_state_fig(readRDS("data/annual_summary/OISST_cat_daily_1991-2020_total.Rds"))
event_total_state_fig(readRDS("data/annual_summary/OISST_cat_daily_1982-2011_total.Rds"), chosen_clim = "1982-2011")
# event_total_state_fig(readRDS("data/annual_summary/OISST_cat_daily_1992-2018_total.Rds"), chosen_clim = "1992-2018")
event_total_state_fig(readRDS("data/annual_summary/OISST_MCS_cat_daily_1991-2020_total.Rds"), MHW = F)
event_total_state_fig(readRDS("data/annual_summary/OISST_MCS_cat_daily_1982-2011_total.Rds"), MHW = F, chosen_clim = "1982-2011")
# CCI
# CCI_1982_2011 <- readRDS("data/annual_summary/CCI_cat_daily_1982-2011_total.Rds")
# event_total_state_fig(CCI_1982_2011, "CCI", "1982-2011")
# CCI_1992_2018 <- readRDS("data/annual_summary/CCI_cat_daily_1992-2018_total.Rds")
# event_total_state_fig(CCI_1992_2018, "CCI", "1992-2018")
# CMC
# CMC_1992_2018 <- readRDS("data/annual_summary/CMC_cat_daily_1992-2018_total.Rds")
# event_total_state_fig(CMC_1992_2018, "CMC", "1992-2018")

# Figures of total time series by year a la format for the annual BAMS report
# TODO: Fix for new larger Y axis in panel A+C
BAMS_fig <- function(){
  
  # Load data
  df_MHW <- readRDS("data/annual_summary/OISST_cat_daily_1991-2020_total.Rds")
  df_MCS <- readRDS("data/annual_summary/OISST_MCS_cat_daily_1991-2020_total.Rds")

  # Get the range needed for the y-axis
  ## MHW
  df_MHW_sum <- df_MHW |> summarise(y_height = sum(cat_area_cum_prop), .by = "t")
  df_MHW_y_height <- plyr::round_any(df_MHW_sum$y_height[df_MHW_sum$y_height == max(df_MHW_sum$y_height)], 5)
  df_MHW_y_height_365 <- plyr::round_any(round(df_MHW_y_height/365, 2), 0.02)
  ## MCS
  df_MCS_sum <- df_MCS |> summarise(y_height = sum(cat_area_cum_prop), .by = "t")
  df_MCS_y_height <- plyr::round_any(df_MCS_sum$y_height[df_MCS_sum$y_height == max(df_MCS_sum$y_height)], 5)
  df_MCS_y_height_365 <- plyr::round_any(round(df_MCS_y_height/365, 2), 0.02)
  
  # Set plotting parameters
  ## MHW
  event_limits_MHW <- c(0, round(df_MHW_y_height+1, -1))
  event_breaks_MHW <- seq(20, event_limits_MHW[2]-10, by = 20)
  second_breaks_MHW <- seq(0.04, plyr::round_any(event_limits_MHW[2]/365, 0.02), 0.04)*365
  second_break_labels_MHW <- paste0(seq(from = 4, by = 4, length.out = length(second_breaks_MHW)), "%")
  ## MCS
  event_limits_MCS <- c(0, round(df_MCS_y_height+1, -1))
  event_breaks_MCS <- seq(5, event_limits_MCS[2]-5, by = 5)
  second_breaks_MCS <- seq(0.02, plyr::round_any(event_limits_MCS[2]/365, 0.02), 0.02)*365
  second_break_labels_MCS <- paste0(seq(from = 2, by = 2, length.out = length(second_breaks_MCS)), "%")
  
  # Set same x-axis breaks for all panels
  x_breaks <- seq(1983, 2023, 10)
  
  # Stacked barplot of global daily count of MHW by category
  fig_count_historic_MHW <- ggplot(df_MHW, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = event_limits_MHW, breaks = event_breaks_MHW,
                       sec.axis = sec_axis(name = paste0("Average daily MHW coverage"), 
                                           transform = ~ . + 0,
                                           breaks = second_breaks_MHW,
                                           labels = second_break_labels_MHW)) +
    scale_x_continuous(breaks = x_breaks) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(y = paste0("Average MHW days"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # fig_count_historic_MHW
  
  # Stacked barplot of global daily count of MCS by category
  fig_count_historic_MCS <- ggplot(df_MCS, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = event_limits_MCS, breaks = event_breaks_MCS,
                       sec.axis = sec_axis(name = paste0("Average daily MCS coverage"), 
                                           transform = ~ . + 0,
                                           breaks = second_breaks_MCS,
                                           labels = second_break_labels_MCS)) +
    scale_x_continuous(breaks = x_breaks) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    labs(y = paste0("Average MCS days"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # fig_count_historic_MCS
  
  # Stacked barplot of cumulative percent of ocean affected by MHW
  fig_cum_historic_MHW <- ggplot(df_MHW, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = x_breaks) +
    labs(y = paste0("Total MHW coverage"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # fig_cum_historic_MHW
  
  # Stacked barplot of cumulative percent of ocean affected by MCS
  fig_cum_historic_MCS <- ggplot(df_MCS, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = x_breaks) +
    labs(y = paste0("Total MCS coverage"), x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # fig_cum_historic_MCS
  
  # Create figure titles
  min_year <- min(df_MHW$t); max_year <- max(df_MHW$t)
  fig_title_MHW <- paste0("MHW category summaries: ",min_year," - ",max_year)
  fig_title_MCS <- paste0("MCS category summaries: ",min_year," - ",max_year)
  
  # Stick them together and save
  ## MHW
  fig_historic_MHW <- ggpubr::ggarrange(fig_count_historic_MHW, fig_cum_historic_MHW, ncol = 2, align = "hv", labels = c("(a)", "(b)"),
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  fig_cap_MHW <- grid::textGrob(fig_title_MHW, x = 0.01, just = "left", gp = grid::gpar(fontsize = 16))
  fig_full_MHW <- ggpubr::ggarrange(fig_cap_MHW, fig_historic_MHW, heights = c(0.1, 1), nrow = 2) #+ ggpubr::bgcolor("white")
  ## MCS
  fig_historic_MCS <- ggpubr::ggarrange(fig_count_historic_MCS, fig_cum_historic_MCS, ncol = 2, align = "hv", labels = c("(c)", "(d)"),
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  fig_cap_MCS <- grid::textGrob(fig_title_MCS, x = 0.01, just = "left", gp = grid::gpar(fontsize = 16))
  fig_full_MCS <- ggpubr::ggarrange(fig_cap_MCS, fig_historic_MCS, heights = c(0.1, 1), nrow = 2) #+ ggpubr::bgcolor("white")
  ## Final
  fig_full <- ggpubr::ggarrange(fig_full_MHW, fig_full_MCS, ncol = 1, nrow = 2) + 
    ggpubr::bgcolor("white") + ggpubr::border("white")
  ggsave(fig_full, filename = "../tikoraluk/graph/BAMS_summary.png", height = 8, width = 8)
  ggsave(fig_full, filename = "../tikoraluk/graph/BAMS_summary.eps", height = 8, width = 8)
}

# Run the figure
# BAMS_fig()


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

# Legacy code
# setwd("figures") # Need to change working directory for animation code to be able to access png files
# system.time(system("convert -delay 100 *.png ../anim/MHW_cat_summary.mp4")) # 139 seconds
# setwd("../")

# Create animations of the annual state for MHW or MCS
# TODO: Allow user to subset a region
# testers...
# product <- "OISST"; chosen_clim <- "1982-2011"; MHW <- TRUE
# chosen_clim <- "1992-2018"
# MHW <- FALSE
event_annual_state_anim <- function(product = "OISST", chosen_clim = "1982-2011", MHW = TRUE){
  
  # Set metadata
  if(MHW){
    event_type <- "MHW"
    event_file <- ""
    event_colours <- MHW_colours
  } else {
    event_type <- "MCS"
    event_file <- "_MCS"
    event_colours <- MCS_colours
  }
  
  # Load data
  event_cat_pixel <- map_dfr(dir("data/annual_summary/", full.names = TRUE,
                                 pattern = paste0(product,event_file,"_cat_pixel_", chosen_clim)), readRDS) |> 
    ungroup() |> mutate(year = year(t)) |> filter(year != year(Sys.Date()))

  # Labels for faster plotting
  event_cat_label <- event_cat_pixel |> dplyr::select(year) |> distinct()
  
  # testers...
  # event_cat_pixel_test <- filter(event_cat_pixel, year %in% 1982:1986)
  # event_cat_label_test <-  filter(event_cat_label, year %in% 1982:1986)
  
  # Global map of event occurrence
  anim_map <- ggplot(event_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_raster(aes(fill = category), show.legend = FALSE) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey60") +
    geom_label(data = event_cat_label, inherit.aes = FALSE,
               aes(x = -40, y = 75, label = year),
               fill = "white", label.size = 1, size = 10) +
    scale_fill_manual("Category", values = event_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat), max(OISST_ocean_coords$lat))) +
    theme_void() +
    transition_manual(frames = year, cumulative = FALSE)

  # Create video
  gganimate::animate(anim_map, width = 25, height = 14, unit = "in",
                     # res = 100, fps = 1, duration = 10, # For testing
                     res = 300, fps = 1, duration = (nrow(event_cat_label)*2)+2, # For full data
                     end_pause = 1,
                     renderer = av_renderer(paste0("figures/",product,event_file,"_",chosen_clim,"_annual.mp4")))
  # Create GIF
  # gganimate::animate(anim_map, width = 25, height = 14, unit = "in", 
  #                    # res = 100, fps = 1, duration = 10, # For manual testing
  #                    res = 300,fps = 1, duration = (nrow(event_cat_label)*2)+2, # For full data
  #                    end_pause = 2,
  #                    renderer = gifski_renderer(paste0("figures/",product,event_file,"_",chosen_clim,"_annual.gif")))
}

# Render the annual animations
# NB: Only needs to be run once per year
## MHW
# system.time(
# event_annual_state_anim() # 305 seconds
# )
## MCS
# system.time(
# event_annual_state_anim(MHW = FALSE) # 279 seconds
# )

