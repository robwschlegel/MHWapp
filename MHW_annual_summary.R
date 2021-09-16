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
# force_calc = T
# database = F
MHW_annual_state <- function(chosen_year, product, chosen_clim, force_calc = F, database = F){
  
  print(paste0("Started run on ",product, "(", 
               chosen_clim,"): ", chosen_year," at ",Sys.time()))
  
  ## Find file location
    # NB: The database files are created in 'MHW_database.R'
  if(database){
    MHW_cat_files <- dir(paste0("../data/",product,"_cat/", chosen_year), 
                         full.names = T, pattern = chosen_clim)
  } else{
    MHW_cat_files <- dir(paste0("../data/cat_clim/",chosen_year), full.names = T)
  }

  # print(paste0("There are currently ",length(MHW_cat_files)," days of data for ",chosen_year))
  
  ## Create figure title
  if(length(MHW_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  product_name <- product
  if(product == "OISST") product_name <- "NOAA OISST"
  fig_title <- paste0("MHW categories of ",chosen_year, extra_bit,
                      "\n",product_name,"; Climatogy period: ",chosen_clim)
  
  ## Load data
  if(force_calc){
    # system.time(
    MHW_cat <- plyr::ldply(MHW_cat_files, readRDS, .parallel = T) #%>% 
    # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
    # na.omit()
    # ) # 12 seconds
  }
  
  ## Process data
  # Max category per pixel
  if(file.exists(paste0("data/annual_summary/",product,"_cat_pixel_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_pixel <- readRDS(paste0("data/annual_summary/",product,"_cat_pixel_",
                                    chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Filtering out the max category at each pixel and counting sum of intensity; ~14 seconds"))
    
    MHW_intensity <- MHW_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity), .groups = "drop")

    # system.time(
    MHW_cat_pixel <- MHW_cat %>% 
      dplyr::select(-event_no) %>% 
      plyr::ddply(., c("lon"), max_event_date, 
                  .parallel = T, .paropts = c(.inorder = FALSE)) %>% 
      unique() %>%
      left_join(MHW_intensity, by = c("lon", "lat"))
    # ) # 14 seconds
    saveRDS(MHW_cat_pixel, file = paste0("data/annual_summary/",product,"_cat_pixel_",
                                         chosen_clim,"_",chosen_year,".Rds"))
    if(product == "OISST" & chosen_clim == "1982-2011"){
      saveRDS(MHW_cat_pixel, file = paste0("data/annual_summary/MHW_cat_pixel_",chosen_year,".Rds"))
    }
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/annual_summary/",product,"_cat_daily_",
                        chosen_clim,"_",chosen_year,".Rds")) & !force_calc){
    MHW_cat_daily <- readRDS(paste0("data/annual_summary/",product,"_cat_daily_",
                                    chosen_clim,"_",chosen_year,".Rds"))
  } else{
    # print(paste0("Counting the daily + cumulative categories per day; ~3 seconds"))
    
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
      right_join(full_grid, by = c("t", "category")) %>% 
      mutate(first_n = ifelse(is.na(first_n), 0, first_n)) %>% 
      arrange(t) %>% 
      group_by(category) %>%
      mutate(first_n_cum = cumsum(first_n)) %>% 
      ungroup()
    # ) # 1 second
    
    # system.time(
    MHW_cat_daily <- MHW_cat %>% 
      group_by(t) %>% 
      count(category) %>% 
      ungroup() %>% 
      right_join(full_grid, by = c("t", "category")) %>% 
      dplyr::rename(cat_n = n) %>% 
      mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n)) %>% 
      group_by(category) %>% 
      mutate(cat_n_cum = cumsum(cat_n),
             cat_n_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
      ungroup() %>% 
      right_join(MHW_cat_single, by = c("t", "category"))
    # ) # 1 second
    saveRDS(MHW_cat_daily, file = paste0("data/annual_summary/",product,"_cat_daily_",
                                         chosen_clim,"_",chosen_year,".Rds"))
    if(product == "OISST" & chosen_clim == "1982-2011"){
      saveRDS(MHW_cat_daily, file = paste0("data/annual_summary/MHW_cat_daily_",chosen_year,".Rds"))
    }
  }
  
  # Add prop columns for more accurate plotting
  MHW_cat_daily <- MHW_cat_daily %>% 
    mutate(first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
           cat_prop = round(cat_n/nrow(OISST_ocean_coords), 4))
  
  # Extract small data.frame for easier labeling
  MHW_cat_daily_labels <- MHW_cat_daily %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_n_cum = cumsum(first_n_cum_prop))
  
  ## Create figures
  # print("Creating figures")
  
  # Global map of MHW occurrence
  fig_map <- ggplot(MHW_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MHW_colours) +
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
  fig_count <- ggplot(MHW_cat_daily, aes(x = t, y = cat_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Global MHW count\n(non-cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MHW_cat_daily, aes(x = t, y = first_n_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MHW_cat_daily_labels, show.legend = F,
               aes(yintercept = label_first_n_cum, colour = category)) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_colour_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Top MHW category per pixel\n(cumulative)", x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(MHW_cat_daily, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(breaks = round(seq(sum(MHW_cat_daily_labels$cat_n_prop)*0.25,
                                      sum(MHW_cat_daily_labels$cat_n_prop)*0.75, length.out = 3), 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = "Average MHW days per pixel\n(cumulative)", x = "Day of the year") +
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
         filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".png"))
  # ggsave(fig_ALL_cap, height = 12, width = 18, 
         # filename = paste0("figures/",product,"_cat_summary_", chosen_clim,"_",chosen_year,".pdf")) # looks bad...
  
  # print(paste0("Finished run on ",product, "(", 
  #              chosen_clim,"): ", chosen_year," at ",Sys.time()))
}

# Run the current year
MHW_annual_state(chosen_year = as.numeric(lubridate::year(Sys.Date())), 
                 product = "OISST", chosen_clim = "1982-2011", force_calc = T) # 161 seconds
# MHW_annual_state(2016, product = "OISST", chosen_clim = "1982-2011", force_calc = F)

# Run ALL years
# NB: Running this in parallel will cause a proper stack overflow
# OISST
# plyr::l_ply(1982:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "OISST", chosen_clim = "1982-2011") # ~50 seconds for one
# plyr::l_ply(1982:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "OISST", chosen_clim = "1992-2018")

# CCI
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1982-2011") # ~50 seconds for one
# plyr::l_ply(1982:2018, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CCI", chosen_clim = "1992-2018")

# CMC
# plyr::l_ply(1992:2019, MHW_annual_state, force_calc = T, .parallel = F,
#             product = "CMC", chosen_clim = "1992-2018")

# NB: This is okay to run in parallel as it doesn't load/process any data
# NB: This is old code from the previous version of the pipeline...
# plyr::l_ply(1982:2019, MHW_annual_state, force_calc = F, .parallel = T) # ~ 1 minute


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
MHW_total_state <- function(product, chosen_clim){
  
  # Create mean values of daily count
  cat_daily_files <- dir("data/annual_summary", full.names = T,
                         pattern = paste0(product,"_cat_daily_",chosen_clim))
  cat_daily_files <- cat_daily_files[!grepl("total", cat_daily_files)]
  cat_daily_mean <- map_dfr(cat_daily_files, readRDS) %>%
  # cat_daily_mean <- map_dfr(dir("data/annual_summary/v2.0", pattern = "cat_daily",
  #                               full.names = T), readRDS) %>% # The old v2.0 OISST data
    mutate(t = lubridate::year(t)) %>%
    group_by(t, category) %>%
    summarise(cat_n = mean(cat_n, na.rm = T), .groups = "drop") %>%
    ungroup() %>%
    mutate(cat_prop_daily_mean = round(cat_n/nrow(OISST_ocean_coords), 4)) %>% 
    filter(t <= 2020) # Use this to not include the current partial year
  
  # Extract only values from Decemer 31st
  cat_daily <- map_dfr(cat_daily_files, readRDS) %>%
  # cat_daily <- map_dfr(dir("data/annual_summary/v2.0", pattern = "cat_daily",
  # full.names = T), readRDS) %>% # The old v2.0 OISST data
    group_by(lubridate::year(t)) %>% 
    filter(t == max(t)) %>% 
    # filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    mutate(t = lubridate::year(t),
           first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
    left_join(cat_daily_mean, by = c("t", "category")) %>% 
    filter(t <= 2020) # Use this to not include the current partial year
  
  # Save and exit
  saveRDS(cat_daily, paste0("data/annual_summary/",product,
                            "_cat_daily_", chosen_clim,"_total.Rds"))
}

## Run them all
# OISST
# MHW_total_state("OISST", "1982-2011")
# MHW_total_state("OISST", "1992-2018")
# CCI
# MHW_total_state("CCI", "1982-2011")
# MHW_total_state("CCI", "1992-2018")
# CMC
# MHW_total_state("CMC", "1992-2018")

MHW_total_state_fig <- function(df, product, chosen_clim){
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count_historic <- ggplot(df, aes(x = t, y = cat_prop_daily_mean)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    labs(y = "Daily MHW occurrence", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_count_historic

  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum_historic <- ggplot(df, aes(x = t, y = first_n_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    labs(y = "Total MHW occurrence", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_cum_historic

  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop_historic <- ggplot(df, aes(x = t, y = cat_n_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MHW_colours) +
    scale_y_continuous(limits = c(0, 90),
                       breaks = seq(15, 75, length.out = 3)) +
    scale_x_continuous(breaks = seq(1984, 2019, 7)) +
    labs(y = "MHW days/pixel", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_prop_historic

  # Create the figure title
  product_title <- product
  if(product == "OISST") product_title <- "NOAA OISST"
  min_year <- min(df$t)
  max_year <- max(df$t)
  clim_title <- gsub("-", " - ", chosen_clim)
  fig_title <- paste0("MHW category summaries: ",min_year," - ",max_year,
                      "\n",product_title,"; Climatogy period: ",clim_title)
  
  # Stick them together and save
  fig_ALL_historic <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic, fig_prop_historic,
                                        ncol = 3, align = "hv", labels = c("A)", "B)", "C)"), hjust = -0.1,
                                        font.label = list(size = 14), common.legend = T, legend = "bottom")
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_full <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_historic, heights = c(0.25, 1), nrow = 2)
  ggsave(fig_ALL_full, filename = paste0("figures/",product,"_cat_historic_",chosen_clim,".png"), height = 4.25, width = 12)
  # ggsave(fig_ALL_full, filename = paste0("figures/",product,"_cat_historic_",chosen_clim,".eps"), height = 4.25, width = 12)
}

## Run them all
# OISST
# OISST_1982_2011 <- readRDS("data/annual_summary/OISST_cat_daily_1982-2011_total.Rds")
# MHW_total_state_fig(OISST_1982_2011, "OISST", "1982-2011")
# OISST_1992_2018 <- readRDS("data/annual_summary/OISST_cat_daily_1992-2018_total.Rds")
# MHW_total_state_fig(OISST_1992_2018, "OISST", "1992-2018")
# CCI
# CCI_1982_2011 <- readRDS("data/annual_summary/CCI_cat_daily_1982-2011_total.Rds")
# MHW_total_state_fig(CCI_1982_2011, "CCI", "1982-2011")
# CCI_1992_2018 <- readRDS("data/annual_summary/CCI_cat_daily_1992-2018_total.Rds")
# MHW_total_state_fig(CCI_1992_2018, "CCI", "1992-2018")
# CMC
# CMC_1992_2018 <- readRDS("data/annual_summary/CMC_cat_daily_1992-2018_total.Rds")
# MHW_total_state_fig(CMC_1992_2018, "CMC", "1992-2018")

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

