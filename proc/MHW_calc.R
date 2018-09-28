## The purpose of this script is to load OISST data and calculate the MHWs
## for each pixel.
## These results are then saved to be able to be used in the shiny app

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(heatwaveR)


# Load data ---------------------------------------------------------------

## NB: No longer necessary to run most of this script as the calculations
## are currently static.

# These data are stored locally on my (RWS) computer as they are 912MB each
# xaa <- read_csv("~/HONOURSPROJECT/data/xaa.csv",  col_names = c("lon", "lat", "temp", "date"))
# xab <- read_csv("~/HONOURSPROJECT/data/xab.csv",  col_names = c("lon", "lat", "temp", "date"))

# combined_OISST <- rbind(xaa, xab, xac, xad, xae, xaf)
# combined_OISST <- rbind(xaa, xab)
# rm(xaa, xab)
# colnames(combined_OISST)[4] <- "t"

# Check start/end date
# min(combined_OISST$date)
# max(combined_OISST$date)


# Calculate MHWs ----------------------------------------------------------

# Calculate the thresholds for all of the created time series
# system.time(
# MHW_res <- combined_OISST %>% 
#   group_by(lon, lat) %>% 
#   nest() %>%
#   # rename(t = date) %>% 
#   mutate(clim = map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31")),
#          event = map(clim, detect_event)) %>%
#   select(-data, -clim) #%>%
#   # unnest()
# ) # 2966 seconds

# save results
# save(MHW_res, file = "data/MHW_res.RData")
# load("data/MHW_res.RData")
## NB: The above file is not on github as it is 381MB

# Calculate categories ----------------------------------------------------

# system.time(
# MHW_cat <- MHW_res %>%
#   mutate(cat = map(event, category, climatology = T)) %>%
#   select(-event)
# ) # 144 seconds

# save results
# save(MHW_cat, file = "data/MHW_cat.RData")
# load("data/MHW_cat.RData")
## NB: The above file is not on github as it is 91MB
