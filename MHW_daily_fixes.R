# This script houses all of the functions that may be used when,
# for whatever reason, something goes sideways with "MHW_dail.R"
# These functions are not meant to be run in mutiples,
# rather they should be run one at a time as spot fixes

source("../MHWapp/MHW_daily_functions.R")


# Fix data at the event/cat lon level -------------------------------------

# tester...
# lon_step <- lon_OISST[359]
MHW_event_cat_fix <- function(lon_step){
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Load current lon slice for event/category
  MHW_event_data <- readRDS(MHW_event_files[lon_row])
  if(MHW_event_data$lon[1] != lon_step) stop("The lon_row indexing has broken down somewhere")
  # MHW_cat_lon <- readRDS(cat_lon_files[lon_row])
  # Load blank
  MHW_cat_lon <- readRDS(cat_lon_files[lon_row-1]) %>% 
    slice(0)
  # if(MHW_cat_lon$lon[1] != lon_step) stop("The lon_row indexing has broken down somewhere")
  
  # Begin the calculations
  print(paste0("Began run on ",MHW_event_files[lon_row]," at ",Sys.time()))
  
  # Extract each pixel time series based on how far back the oldest event occurred for the entire longitude slice
  sst_seas_thresh <- sst_seas_thresh_merge(lon_step, 
                                           start_date = as.Date("1982-01-01"))
  
  # Calculate new event metrics with new data as necessary
  system.time(
  MHW_event_cat <- MHW_event_data %>%
    select(lon, lat) %>% 
    distinct() %>% 
    mutate(lat2 = lat) %>% 
    group_by(lat2) %>% 
    nest() %>% 
    mutate(event_cat_res = map(data, event_calc_all,
                               sst_seas_thresh = sst_seas_thresh)) %>% 
    select(-data, -lat2) %>% 
    unnest() # ~89 seconds to redo everything
  )
  # Save results and exit
  MHW_event_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest()
  if(length(MHW_event_new$lon) != 0) saveRDS(MHW_event_new, file = MHW_event_files[lon_row])
  
  MHW_cat_new <- MHW_event_cat %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest()
  if(length(MHW_cat_new$lon) != 0) saveRDS(MHW_cat_new, file = cat_lon_files[lon_row])
  
  print(paste0("Finished run on MHW.event.",lon_row_pad,".Rda at ",Sys.time()))
}

# Function for extracting correct sst data based on pre-determined subsets
# It also calculates and returns corrected MHW metric results
# df <- previous_event_index[300,]
event_calc_all <- function(df, sst_seas_thresh){
  
  # Extract necessary SST
  sst_step_1 <- sst_seas_thresh %>% 
    filter(lat == df$lat[1])
  
  # Calculate events
  event_base <- detect_event(sst_step_1)
  event_step_1 <- event_base$event %>% 
    mutate(lon = df$lon[1], lat = df$lat[1]) %>% 
    dplyr::select(lon, lat, event_no, duration:intensity_max, intensity_cumulative) %>%
    mutate_all(round, 3)
  
  # Calculate categories
  cat_step_1 <- category(event_base, climatology = T)$climatology %>% 
    mutate(lon = df$lon[1],
           lat = df$lat[1]) %>% 
    select(t, lon, lat, event_no, intensity, category)
  
  # Exit
  event_cat <- list(event = event_step_1,
                    cat = cat_step_1)
  return(event_cat)
}

