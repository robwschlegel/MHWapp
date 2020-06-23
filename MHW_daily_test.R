# MHW_daily_test.R
# This script houses a bunch of code used to poke and prod 
# at the MHW_daily.R and MHW_daily_functions.R scripts

source("MHW_daily_functions.R")


# 1: Testing the downloaded NOAA data -------------------------------------

# Front nub
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# Download a day of data and test it directly
OISST_test <- plyr::ldply(paste0(OISST_url_month, "202006/oisst-avhrr-v02r01.20200601.nc"),
                          .fun = OISST_url_daily_dl, .parallel = F)
OISST_test$lon <- ifelse(OISST_test$lon > 180, OISST_test$lon-360, OISST_test$lon)

# test visual
ggplot(data = OISST_test, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = temp))

# Function for extracting one day of data
extract_OISST_one <- function(index_val, date_val){
  file_name <- OISST_files[index_val]
  date_int <- as.integer(date_val)
  res <- tidync(file_name) %>%
    hyper_filter(time = time == date_int) %>%
    hyper_tibble() #%>%
    # select(-time, -sst)
}

# Load every pixel for a chosen day
registerDoParallel(cores = 50)
OISST_test <- plyr::ldply(1:1440, extract_OISST_one, .parallel = T,
                          date_val = as.Date("2020-06-21"))

# test visual
ggplot(data = OISST_test, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = sst))

# test visuals for data downloaded via MHW_daily.r script
ggplot(data = filter(OISST_dat, t == "2020-06-20"), aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = temp))

# test the lon lat grid
ggplot(data = lon_lat_OISST, aes(x = lon, y = lat)) +
  geom_tile(fill = "red") +
  borders(fill = "grey70", colour = "black")


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


# 3: Testing global files -------------------------------------------------

# Load a single file
MHW_cat_clim <- readRDS("shiny/cat_clim/2016/cat.clim.2016-01-01.Rda")

# Crude global plot
ggplot(data = MHW_cat_clim, aes(x = lon, y = lat)) +
  borders(fill = "grey70", colour = "black") +
  geom_tile(aes(fill = category)) +
  scale_fill_manual("Category",
                    values = c("#ffc866", "#ff6900", "#9e0000", "#2d0000"),
                    labels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(expand = F)

# MHW_anom <- readRDS("")

