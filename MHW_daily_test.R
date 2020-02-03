# This script houses a bunch of code used to poke and prod 
# at the MHW_daily.R and MHW_daily_functions.R scripts

# source("../tikoraluk/MHW_2018.R")
source("MHW_daily_functions.R")


# 1: Testing the downloaded NOAA data -------------------------------------

# NB: This code is for the previous pipeline
# OISST_update_1 <- OISST_dl(c("2019-02-17T00:00:00Z", "2019-02-17T00:00:00Z"))
# OISST_update_2 <- OISST_prep(OISST_update_1)


# 2: Testing the MHW event and cat production -----------------------------

# Set the current_dates as desired
# current_dates <- seq(as.Date("1982-01-01"), as.Date("2017-12-31"), by = "day")

chosen_sub <- 1
chosen_lat <- -5.125

## test a single run
# MHW_event_cat_update(lon_OISST[chosen_sub], current_dates = current_dates)
# MHW_load_proc_save(lon_OISST[chosen_sub])

## Load sst/seas/thresh
sst_seas_thresh <- sst_seas_thresh_merge(lon_step = lon_OISST[chosen_sub], 
                                         as.Date("1982-01-01"))
sst_seas_thresh_sub <- sst_seas_thresh %>% 
  filter(lat == chosen_lat)


## Load events
MHW_event_data <- readRDS(MHW_event_files[chosen_sub]) %>% 
  filter(lat == chosen_lat)#,
         # date_start >= "2018-01-01")


## Load a daily slice that should have a MHW
MHW_cat_data <- readRDS(cat_clim_files[which(as.Date("2018-10-15") == seq(as.Date("1982-01-01"), 
                                            as.Date("2018-12-31"), by = "day"))]) %>% 
  filter(lat == chosen_lat,
         lon == lon_OISST[chosen_sub])

## Load a cat lon slice
MHW_cat_lon <- readRDS(cat_lon_files[chosen_sub])

## Visualise
p <- ggplot(data = sst_seas_thresh_sub, aes(x = t, y = temp)) +
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
if(length(MHW_event_data$date_start) > 0){
  p <- p + geom_rug(data = MHW_event_data, sides = "b", colour = "red3", size = 2,
                    aes(x = date_peak, y = min(sst_seas_thresh_sub$temp),
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


# 3: Testing cat_clim files -----------------------------------------------

# Load a single file
MHW_cat_clim <- readRDS("shiny/cat_clim/2020/cat.clim.2020-02-02.Rda")

# Crude global plot
ggplot(data = MHW_cat_clim, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = F)

