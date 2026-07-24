# MHWapp/shiny/functions.R
# This script houses non-reactive functions used in the shiny app


# Merge sst/seas/thresh for a given lon/lat -------------------------------

# testers...
# lon_step <- lon_OISST[721]
# lat_step <- lat_OISST[132]
# lon_step <- xy[1]
# lat_step <- xy[2]
# Overland
# lon_step <- -80.875
# lat_step <- 38.125
# lat_step <- 21.125
# base_years <- "1982-2011"
sst_seas_thresh_ts <- function(lon_step, lat_step, base_years){
  
  # Get correct baseline files
  MHW_seas_thresh_files_base <- MHW_seas_thresh_files[grepl(base_years, MHW_seas_thresh_files)]
  MCS_seas_thresh_files_base <- MCS_seas_thresh_files[grepl(base_years, MCS_seas_thresh_files)]
  MHW_event_files_base <- MHW_event_files[grepl(base_years, MHW_event_files)]
  MCS_event_files_base <- MCS_event_files[grepl(base_years, MCS_event_files)]
  
  # Set metadata
  date_range <- c(as.Date("1982-01-01"), as.Date(Sys.Date()))
  lon_row <- which(lon_OISST == lon_step)
  
  # Extract daily values
  # OISST data
  nc_OISST <- nc_open(OISST_files[lon_row])
  lat_idx <- which(as.numeric(nc_OISST$dim$lat$vals) == lat_step)
  sst_vals <- as.numeric(ncvar_get(nc_OISST, "sst", start = c(lat_idx, 1, 1), count = c(1, 1, -1)))
  time_vals <- as.numeric(nc_OISST$dim$time$vals)
  nc_close(nc_OISST)
  df_OISST <- data.frame(lon = lon_step, lat = lat_step,
                         t = as.Date(time_vals, origin = "1970-01-01"),
                         temp = sst_vals)
  if(length(na.omit(df_OISST)) == 0){
    sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
                                  seas = NA, thresh = NA)
    return(sst_seas_thresh)
  }
  seas_base_MHW <- heatwave3::hw3_export(MHW_seas_thresh_files_base[lon_row], 
    lat_range = c(lat_step, lat_step))
  seas_base_MCS <- heatwave3::hw3_export(MCS_seas_thresh_files_base[lon_row], 
    lat_range = c(lat_step, lat_step)) |> 
    dplyr::rename(thresh_MCS = thresh)
  
  # Combine and exit
  sst_seas_thresh <- df_OISST |> 
    mutate(doy = lubridate::yday(t),
           year = lubridate::year(t)) |>
    group_by(year) |>
    mutate(doy = ifelse(!lubridate::leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) |>
    ungroup() |>
    left_join(seas_base_MHW, 
      by = join_by(lon, lat, doy)) |>
    left_join(seas_base_MCS, 
      by = join_by(lon, lat, doy, seas)) |>
    mutate(anom = round(temp - seas, 2),
           temp = round(temp, 2),
           seas = round(seas, 2),
           thresh = round(thresh, 2),
           thresh_MCS = round(thresh_MCS, 2)) |>
    dplyr::select(lon, lat, t, doy, temp, seas, thresh, thresh_MCS, anom)
  return(sst_seas_thresh)
}


# Popups and map coords ---------------------------------------------------

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

# The same as lon_wrap() but for leaflet projection
# NB: This function assumes lon is the first value and lat is the second
leaf_lon_wrap <- function(xy){
  while(xy[1] > 20023593){
    xy[1] <- xy[1] - 40047186
  }
  while(xy[1] < -20023593){
    xy[1] <- xy[1] + 40047186
  }
  return(xy)
}

# Show popup on clicks
# tester...
# x <- 10; y <- 31.3
showpos <- function(x = NULL, y = NULL){
  
  # Process click info into pretty OISST coords
  xy_click <- c(x,y)
  xy_wrap <- lon_wrap(xy_click)
  xy <- c(lon_OISST[which(abs(lon_OISST - xy_wrap[1]) == min(abs(lon_OISST - xy_wrap[1])))][1],
          lat_OISST[which(abs(lat_OISST - xy_wrap[2]) == min(abs(lat_OISST - xy_wrap[2])))][1])
  if(xy[1] >= 0) xy_lon <- paste0(abs(xy[1]),"°E")
  if(xy[1] < 0) xy_lon <- paste0(abs(xy[1]),"°W")
  if(xy[2] >= 0) xy_lat <- paste0(abs(xy[2]),"°N")
  if(xy[2] < 0) xy_lat <- paste0(abs(xy[2]),"°S")
  content <- paste0("Lon = ", xy_lon,
                    "<br>Lat = ", xy_lat,
                    "<br>",
                    "<br><b>↓ See time series panel below ↓</b>")
  # content_sub,
  # regional_link,
  # "<hr>",
  # button_text)
  # content <- xy_wrap
  
  # Update lon lat
  # updateNumericInput(session, "lon", value = round(xy_click[1], 2))
  # updateNumericInput(session, "lat", value = round(xy_click[2], 2))
  
  # Add Popup to leaflet
  leafletProxy("leaf_map") |> 
    clearPopups() |> 
    addPopups(lng = xy_click[1], 
              lat = xy_click[2],
              popup = paste(content))
}


# Value boxes -------------------------------------------------------------

# Reacts to map layers and displays coloured percent summaries
# testers...
# event_type <- "MHW"; cat_choice <- "I Moderate"
# value_box_cat <- function(layer_choice, cat_choice, mapCover){
value_box_cat <- function(cat_choice, layer_choice, mapCover){
  
  if(nrow(mapCover) < 2) return()
  
  # Set colour palette
  if(layer_choice == "MCS Category"){
    event_colours <- c(base_colours[2], MCS_colours)
    mapCover$text_class = c("text-dark", "text-dark", "text-light", "text-light", "text-dark", "text-dark")
  } else {
    event_colours <- c(base_colours[1], MHW_colours)
    mapCover$text_class = c("text-dark", "text-dark", "text-light", "text-light", "text-dark")
  }
  names(event_colours)[1] <- "Total cover"
  
  # Get values
  event_col <- event_colours[names(event_colours) == cat_choice]
  mapVal <- filter(mapCover, category == cat_choice)
  
  # Create box
  value_box(title = mapVal$category, value = mapVal$cat_area_prop*100, showcase = bs_icon("percent"),
            style = paste0("background-color: ",event_col,"!important;"), class = mapVal$text_class, min_height = "100px")
}


# Load MHW cat file and include the date ----------------------------------

# Daily files
readRDS_date <- function(file_name){
  file_segments <- length(strsplit(file_name, "/")[[1]])
  file_date <- sapply(strsplit(file_name, "/"), "[[", file_segments)
  file_date <- as.Date(stringr::str_remove_all(file_date, "[daily.cat.clim.Rda.Rds.MCS]"))
  res <- readRDS(file_name) |>
    mutate(t = file_date) |>
    dplyr::select(t, lon, lat, everything())
}

# Annual files
readRDS_year <- function(file_name){
  file_segments <- length(strsplit(file_name, "/")[[1]])
  file_only <- sapply(strsplit(file_name, "/"), "[[", file_segments)
  file_only <- stringr::str_remove_all(file_only, "[.Rds]")
  file_only_segments <- length(strsplit(file_only, "_")[[1]])
  file_year <- as.integer(sapply(strsplit(file_only, "_"), "[[", file_only_segments))
  res <- readRDS(file_name) |>
    mutate(t = file_year) |>
    dplyr::select(t, lon, lat, everything())
}


# Join ts and event -------------------------------------------------------

# Convenience wrapper to add daily event info
ts_event_join <- function(event_df, ts_df){
  ts_res <- ts_df |> 
    filter(t >= event_df$date_start[1], t <= event_df$date_end[1]) |> 
    mutate(event_no = event_df$event_no[1],
           category = event_df$category[1])
  return(ts_res)
}

