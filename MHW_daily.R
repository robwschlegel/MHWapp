# This script combines all preivous work towards the MHWapp into one location
# This script is designed to be run autonomously once per day by a chronjob
# It performs the following tasks:
## 1: Downloads the most recent NOAA OISST data available
##    and updates the local NetCDF files
## 2: Updates MHW event data
## 3: Creates new daily global MHW category file(s)

source("../MHWapp/MHW_daily_functions.R")


# 1: Update OISST data ----------------------------------------------------

# Check for most recent downloaded data against the current date
OISST_update_1 <- OISST_dl()

# Prep for inclusion in NetCDF files
OISST_update_2 <- OISST_prep(OISST_update_1)

# Update the files
plyr::ldply(lon_OISST, .fun = OISST_merge, .parallel = TRUE, df = OISST_update_2)


# 2: Update MHW event data ------------------------------------------------

# lon_step <- lon_OISST[1]
MHW_event_update <- function(lon_step){
  
  # Determine correct lon/row/slice
  lon_row <- which(lon_OISST == lon_step)
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste0("Began run on ",lon_row_pad," at ",Sys.time()))
  
  # Load current lon slice for OISST/thresh/event/cat.clim
  MHW_event <- readRDS(MHW_event_files[lon_row]) %>% 
    filter(date)
  
}


# 3: Update MHW category data ---------------------------------------------


MHW_category_update <- function(){
  
}

