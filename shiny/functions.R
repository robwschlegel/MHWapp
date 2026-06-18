# MHWapp/shiny/functions.R
# This script houses non-reactive functions used in the shiny app


# Merge sst/seas/thresh for a given lon/lat -------------------------------

# testers...
# lon_step <- lon_OISST[721]
# lon_step <- lat_OISST[132]
# lon_step <- xy[1]
# lat_step <- xy[2]
# Overland
# lon_step <- -80.875
# lat_step <- 38.125
# lat_step <- 21.125
# base_years <- "1982-2011"
sst_seas_thresh_ts <- function(lon_step, lat_step, base_years){
  
  # Establish metadata
  MHW_seas_thresh_files_base <- MHW_seas_thresh_files[grepl(base_years, MHW_seas_thresh_files)]
  MCS_seas_thresh_files_base <- MCS_seas_thresh_files[grepl(base_years, MCS_seas_thresh_files)]
  lon_row <- which(lon_OISST == lon_step)
  lat_row <- which(lat_OISST == lat_step)
  
  # OISST data
  tidync_OISST <- tidync::tidync(OISST_files[lon_row]) |>
    tidync::hyper_filter(lat = lat == lat_step) |>
    tidync::hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |>
    # mutate(time = as.Date(time, origin = "1970-01-01"),
    mutate(time = as.Date(time),
           year = lubridate::year(time),
           lon = as.numeric(lon),
           lat = as.numeric(lat)) |>
    dplyr::rename(t = time, temp = sst) |>
    mutate(doy = lubridate::yday(t)) |>
    group_by(year) |>
    mutate(doy = ifelse(!lubridate::leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) |>
    ungroup() |>
    dplyr::select(lon, lat, t, doy, temp)

  if(length(na.omit(tidync_OISST)) == 0){
    sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
                                  seas = NA, thresh = NA)
    return(sst_seas_thresh)
  }
  
  # Load MHW and MCS
  seas_base_MHW <- tidync::tidync(MHW_seas_thresh_files_base[lon_row]) |>
  tidync::hyper_filter(lat = lat == lat_step) |>
  tidync::hyper_tibble(drop = FALSE) |>
  mutate(doy = as.numeric(doy),
         lon = as.numeric(lon),
         lat = as.numeric(lat))
  seas_base_MCS <- tidync::tidync(MCS_seas_thresh_files_base[lon_row]) |>
    tidync::hyper_filter(lat = lat == lat_step) |>
    tidync::hyper_tibble(drop = FALSE) |>
    mutate(doy = as.numeric(doy),
           lon = as.numeric(lon),
           lat = as.numeric(lat))
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST |>
    left_join(seas_base_MHW, by = c("lon", "lat", "doy")) |>
    left_join(seas_base_MCS, by = c("lon", "lat", "doy")) |>
    dplyr::select(-seas.y) |>
    dplyr::rename(seas = seas.x, thresh = thresh.x, thresh_MCS = thresh.y) |>
    mutate(anom = round(temp - seas, 2),
           temp = round(temp, 2),
           seas = round(seas, 2),
           thresh = round(thresh, 2),
           thresh_MCS = round(thresh_MCS, 2))
  rm(tidync_OISST, seas_base_MHW, seas_base_MCS); gc()
  
  ## heatwave3 code
  
  # Get correct baseline files
  # MHW_seas_thresh_files_base <- MHW_seas_thresh_files[grepl(base_years, MHW_seas_thresh_files)]
  # MCS_seas_thresh_files_base <- MCS_seas_thresh_files[grepl(base_years, MCS_seas_thresh_files)]
  # MHW_event_files_base <- MHW_event_files[grepl(base_years, MHW_event_files)]
  # MCS_event_files_base <- MCS_event_files[grepl(base_years, MCS_event_files)]
  
  # Set metadata
  # date_range <- c(as.Date("1982-01-01"), as.Date(Sys.Date()))
  # lon_row <- which(lon_OISST == lon_step)
  
  # Extract daily values
  # OISST data
  # tidync_OISST <- tidync::tidync(OISST_files[lon_row]) |>
  #   tidync::hyper_filter(lat = lat == lat_step) |>
  #   tidync::hyper_tibble(na.rm = FALSE, force = TRUE, drop = FALSE) |>
  #   # mutate(time = as.Date(time, origin = "1970-01-01"),
  #   mutate(t = as.Date(time),
  #          lon = as.numeric(lon),
  #          lat = as.numeric(lat)) |>
  #   dplyr::rename(t = time, temp = sst)
  # 
  # if(length(na.omit(tidync_OISST)) == 0){
  #   sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
  #                                 seas = NA, thresh = NA)
  #   return(sst_seas_thresh)
  # }
  # seas_base_MHW <- heatwave3::hw3_export(MHW_seas_thresh_files_base[lon_row],  lat_range = lat_step)
  # seas_base_MCS <- heatwave3::hw3_export(MCS_seas_thresh_files_base[lon_row],  lat_range = lat_step)
  # cat_daily_MHW <- heatwave3::category_daily3(
  #     sst_file = OISST_files[lon_row],
  #     clim_file = MHW_seas_thresh_files_base[lon_row],
  #     event_file =  MHW_event_files_base[lon_row],
  #     time_range = date_range, 
  #     lat_range = c(lat_step, lat_step)) |> 
  #   dplyr::select(lon, lat, t, temp, seas, thresh)
  # cat_daily_MCS <- heatwave3::category_daily3(
  #     sst_file = OISST_files[lon_row],
  #     clim_file = MCS_seas_thresh_files_base[lon_row],
  #     event_file =  MCS_event_files_base[lon_row],
  #     time_range = date_range,
  #     lat_range = c(lat_step, lat_step),
  #     coldSpells = TRUE) |> 
  #   dplyr::select(lon, lat, t, temp, seas, thresh) |> 
  #   dplyr::rename(thresh_MCS = thresh)
  
  # Combine and exit
  # sst_seas_thresh <- left_join(cat_daily_MHW, cat_daily_MCS, 
  #                                  by = join_by(lon, lat, t, temp, seas)) |> 
  # mutate(doy = lubridate::yday(t),
  #        year = lubridate::year(t),
  #        anom = temp - seas) |>
  #   group_by(year) |>
  #   mutate(doy = ifelse(!lubridate::leap_year(year),
  #                       ifelse(doy > 59, doy+1, doy), doy)) |>
  #   ungroup() |> 
  #   mutate(anom = round(temp - seas, 2),
  #          temp = round(temp, 2),
  #          seas = round(seas, 2),
  #          thresh = round(thresh, 2),
  #          thresh_MCS = round(thresh_MCS, 2)) |> 
  #   dplyr::select(lon, lat, t, doy, temp, seas, thresh, thresh_MCS, anom)
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
                    # "<br><b>↓ See time series panel below ↓</b>")#,
                    "<br><b>Time series panel</b>",
                    "<br><b>under construction</b>")#,
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

