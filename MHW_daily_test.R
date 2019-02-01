# This script houses a bunch of code used to poke and prod 
# at the MHW_daily.R and MHW_daily_functions.R scripts


# 2: Testing the MHW event and cat production -----------------------------

# Set the current_dates as desired
# current_dates <- seq(as.Date("1982-01-01"), as.Date("2017-12-31"), by = "day")

## test a single run
MHW_event_cat_update(lon_OISST[340], current_dates = current_dates)

## Visualise output

# Load sst/seas/thresh
nc_OISST <- nc_open(OISST_files[340])
# lat_vals <- as.vector(nc_OISST$dim$lat$vals)
time_index <- as.Date(ncvar_get(nc_OISST, "time"), origin = "1970-01-01")
time_extract_index <- time_index[which("2018-01-01" == time_index):length(time_index)]
sst_raw <- ncvar_get(nc_OISST, "sst", start = c(1,1,(which(min(time_extract_index) == time_index))))
if(length(time_extract_index) == 1) dim(sst_raw) <- c(720,1,1)
dimnames(sst_raw) <- list(lat = nc_OISST$dim$lat$vals,
                          t = time_extract_index)
nc_close(nc_OISST)

# Prep SST for further use
sst <- as.data.frame(reshape2::melt(sst_raw, value.name = "temp"), row.names = NULL) %>%
  mutate(t = as.Date(t, origin = "1970-01-01")) %>%
  na.omit() %>% 
  ## NB: Delete this filter after re-running 2018 data
  filter(t <= "2018-12-31") %>% 
  ##
  dplyr::rename(ts_x = t, ts_y = temp) %>% 
  group_by(lat) %>%
  # heatwaveR:::make_whole_fast()
  nest() %>%
  mutate(whole_data = map(data, heatwaveR:::make_whole_fast)) %>%
  select(-data) %>%
  unnest() %>% 
  dplyr::rename(t = ts_x, temp = ts_y)

# seas.thresh data
nc_seas <- nc_open(seas_thresh_files[1])
seas <- ncvar_get(nc_seas, "seas")
dimnames(seas) <- list(lat = nc_seas$dim$lat$vals,
                       doy = nc_seas$dim$time$vals)
thresh <- ncvar_get(nc_seas, "thresh")
dimnames(thresh) <- list(lat = nc_seas$dim$lat$vals,
                         doy = nc_seas$dim$time$vals)
nc_close(nc_seas)
seas <- as.data.frame(reshape2::melt(seas, value.name = "seas"), row.names = NULL) %>%
  na.omit() %>% 
  dplyr::arrange(lat, doy)
thresh <- as.data.frame(reshape2::melt(thresh, value.name = "thresh"), row.names = NULL) %>%
  na.omit() %>% 
  dplyr::arrange(lat, doy)

# Combine temp/seas/thresh
sst_seas_thresh <- sst %>% 
  left_join(seas, by = c("lat", "doy")) %>%
  left_join(thresh, by = c("lat", "doy")) %>% 
  filter(lat == -29.125)

# Load events
MHW_event <- readRDS(MHW_event_files[340]) %>% 
  filter(lat == -29.125,
         date_start >= "2017-12-31")

# Load a daily slice that should have a MHW
MHW_cat <- readRDS(cat_clim_files[which(as.Date("2018-10-01") == seq(as.Date("1982-01-01"), 
                                            as.Date("2018-12-31"), by = "day"))]) %>% 
  filter(#lat == -29.125,
         lon == lon_OISST[340])

# Visualise
p <- ggplot(data = sst_seas_thresh, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh)) +
  geom_line(colour = "grey20",
            aes(group = 1, text = paste0("Date: ",t,
                                         "<br>Temperature: ",temp,"°C"))) +
  geom_line(linetype = "dashed", colour = "steelblue3",
            aes(x = t, y = seas, group = 1,
                text = paste0("Date: ",t,
                              "<br>Climatology: ",seas,"°C"))) +
  geom_line(linetype = "dotted", colour = "tomato3",
            aes(x = t, y = thresh, group = 1,
                text = paste0("Date: ",t,
                              "<br>Threshold: ",thresh,"°C"))) +
  labs(x = "", y = "Temperature (°C)") +
  scale_x_date(expand = c(0, 0))
if(length(MHW_event$date_start) > 0){
  p <- p+ geom_rug(data = MHW_event, sides = "b", colour = "red3", size = 2,
           aes(x = date_peak, y = min(sst_seas_thresh$temp),
               text = paste0("Event: ",event_no,
                             "<br>Duration: ",duration," days",
                             "<br>Start Date: ", date_start,
                             "<br>Peak Date: ", date_peak,
                             "<br>End Date: ", date_end,
                             "<br>Mean Intensity: ",intensity_mean,"°C",
                             "<br>Max. Intensity: ",intensity_max,"°C",
                             "<br>Cum. Intensity: ",intensity_cumulative,"°C")))
}
p
