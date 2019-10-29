# proc/speed_testing.R
# The purpose of this script is to house the code used in speed testing.
# As packages develop, and new packages emerge, it is necessary that the
# MHWapp code be continously improved.


# Loading NetCDF files ----------------------------------------------------

## Testing the speed of tidync as a viable option of NetCDF creation
system.time(
  test_tidync <- hyper_tibble(tidync("../data/OISST/avhrr-only-v2.ts.0001.nc"))
) # ~1 second

# What about csv?

# Creating NetCDF files  --------------------------------------------------

system.time(
OISST_full <- rbind(OISST_final_1, OISST_prelim_1) %>% 
  filter(lon == 0.125)
) # ~2 seconds

system.time(
OISST_ALL <- test_tidync %>% 
  mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
  dplyr::rename(temp = sst, t = time) %>% 
  select(lon, lat, t, temp) %>% 
  filter(!(t %in% unique(OISST_full$t))) %>% 
  bind_rows(OISST_full) %>% 
  na.omit()
) # ~1 second

system.time(
  OISST_ncdf(OISST_ALL)
) # 121 seconds

system.time(
  OISST_merge(lon_step = 0.125, df_prelim = OISST_prelim_1, df_final = OISST_final_1)
) # 0.1 seconds

# As we can see from the speed checks above, there is absolutely no contest between 
# the tidy approach and the direct NetCDF write appraoch
# But what about csv?

system.time(
  data.table::fwrite(OISST_ALL, "proc/test/test.0001.csv")
) # ~0.2 seconds
system.time(
  OISST_ALL <- data.table::fread("proc/test/test.0001.csv", colClasses = c()) %>% 
    mutate(t = as.Date(t))
) # ~0.4 seconds, 10 seconds with date correction

# These read and write times become comparable, but the file sizes are much too large
# It looks like the NetCDF pipeline retains the crown... for now


# Calculating MHWs --------------------------------------------------------


