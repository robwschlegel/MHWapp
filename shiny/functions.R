# Function that loads and merges sst/seas/thresh for a given lon/lat
# testers...
# lon_step <- lon_OISST[721]
# lon_step <- lat_OISST[132]
# lon_step <- xy[1]
# lat_step <- xy[2]
# Overland
# lon_step <- -80.875
# lat_step <- 38.125
# lat_step <- 21.125
sst_seas_thresh_ts <- function(lon_step, lat_step){
  
  lon_row <- which(lon_OISST == lon_step)
  # lat_row <- which(lat_OISST == lat_step)
  
  # OISST data
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_filter(lat = lat == lat_step) %>%
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(ts_x = time, ts_y = sst) %>%
    group_by(lon, lat) %>% 
    group_modify(~heatwaveR:::make_whole_fast(.x)) %>% # This is necessary for the doy column
    ungroup() %>% 
    dplyr::rename(t = ts_x, temp = ts_y) %>%
    select(lon, lat, t, doy, temp)
  
  if(length(na.omit(tidync_OISST)) == 0){
    sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
                                  seas = NA, thresh = NA)
    return(sst_seas_thresh)
  }
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST %>% 
    left_join(hyper_tibble(tidync(seas_thresh_files[lon_row])), 
              by = c("lon", "lat", "doy" = "time")) %>% 
    mutate(temp = round(temp, 2),
           seas = round(seas, 2),
           thresh = round(thresh, 2))
  return(sst_seas_thresh)
}

# Function for correcting the map wrap-around lon issue
# NB: This function assumes lon is the first value and lat is the second
lon_wrap <- function(xy){
    while(xy[1] > 180){
      xy[1] <- xy[1] - 360
    }
    while(xy[1] < -180){
      xy[1] <- xy[1] + 360
    }
  return(xy)
}

