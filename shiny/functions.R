# Function that loads and merges sst/seas/thresh for a given lon/lat
# testers...
# lon_step <- lon_OISST[721]
# lon_step <- lat_OISST[132]
# lon_step <- xy[1]
# lat_step <- xy[2]
# Overland
# lon_step <- -80.875
# lat_step <- 38.125
sst_seas_thresh_ts <- function(lon_step, lat_step){
  
  lon_row <- which(lon_OISST == lon_step)
  lat_row <- which(lat_OISST == lat_step)
  
  # OISST data
  nc_OISST <- nc_open(OISST_files[lon_row])
  # lat_vals <- as.vector(nc_OISST$dim$lat$vals)
  time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
  # time_old_index <- time_index[time_index <= max(current_dates)]
  # time_extract_index <- time_index[which(min(previous_event_index$date_end) == time_index):length(time_index)]
  sst_raw <- ncvar_get(nc_OISST, "sst", start = c(lat_row,1,1), count = c(1,1,-1))
  nc_close(nc_OISST)
  
  if(length(sst_raw[complete.cases(sst_raw)]) == 0){
    sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
                                  seas = NA, thresh = NA)
    return(sst_seas_thresh)
  }
  
  # Prep SST for further use
  sst <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
    mutate(lat = lat_step,
           t = time_index) %>%
    na.omit() %>% 
    dplyr::rename(ts_x = t, ts_y = temp) %>% 
    heatwaveR:::make_whole_fast() %>%
    dplyr::rename(t = ts_x, temp = ts_y)
  
  # seas.thresh data
  nc_seas <- nc_open(seas_thresh_files[lon_row])
  seas <- ncvar_get(nc_seas, "seas", start = c(lat_row,1,1), count = c(1,1,-1))
  dimnames(seas) <- list(doy = nc_seas$dim$time$vals)
  thresh <- ncvar_get(nc_seas, "thresh", start = c(lat_row,1,1), count = c(1,1,-1))
  dimnames(thresh) <- list(doy = nc_seas$dim$time$vals)
  nc_close(nc_seas)
  seas <- as.data.frame(reshape2::melt(seas, value.name = "seas"), row.names = NULL) %>%
    na.omit()
  thresh <- as.data.frame(reshape2::melt(thresh, value.name = "thresh"), row.names = NULL) %>%
    na.omit()
  
  # Merge and exit
  sst_seas_thresh <- sst %>% 
    left_join(seas, by = c("doy")) %>%
    left_join(thresh, by = c("doy")) %>% 
    mutate(temp = round(temp, 2),
           seas = round(seas, 2),
           thresh = round(thresh, 2))
  return(sst_seas_thresh)
}

