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
sst_seas_thresh_ts <- function(lon_step, lat_step){
  
  lon_row <- which(lon_OISST == lon_step)
  # lat_row <- which(lat_OISST == lat_step)
  
  # OISST data
  tidync_OISST <- tidync::tidync(OISST_files[lon_row]) %>% 
    tidync::hyper_filter(lat = lat == lat_step) %>%
    tidync::hyper_tibble() %>%
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = lubridate::year(time)) %>% 
    dplyr::rename(t = time, temp = sst) %>%
    mutate(doy = lubridate::yday(t)) %>% 
    group_by(year) %>% 
    mutate(doy = ifelse(!lubridate::leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) %>% 
    ungroup() %>%
    select(lon, lat, t, doy, temp)
  
  if(length(na.omit(tidync_OISST)) == 0){
    sst_seas_thresh <- data.frame(doy = NA, t = NA, temp = NA,
                                  seas = NA, thresh = NA)
    return(sst_seas_thresh)
  }
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST %>% 
    left_join(tidync::hyper_tibble(tidync::tidync(MHW_seas_thresh_files[lon_row])), 
              by = c("lon", "lat", "doy" = "time")) %>% 
    left_join(readRDS(MCS_seas_thresh_files[lon_row]),
              by = c("lon", "lat", "doy" = "time")) %>%
    dplyr::select(-seas.y) %>% 
    dplyr::rename(seas = seas.x, thresh = thresh.x, thresh_MCS = thresh.y) %>% 
    mutate(temp = round(temp, 2),
           seas = round(seas, 2),
           thresh = round(thresh, 2),
           thresh_MCS = round(thresh_MCS, 2))
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
                    "<br> See time series below.")#,
  # content_sub,
  # regional_link,
  # "<hr>",
  # button_text)
  # content <- xy_wrap
  
  # Update lon lat
  # updateNumericInput(session, "lon", value = round(xy_click[1], 2))
  # updateNumericInput(session, "lat", value = round(xy_click[2], 2))
  
  # Add Popup to leaflet
  leafletProxy("leaf_map") %>%
    clearPopups() %>%
    addPopups(lng = xy_click[1], lat = xy_click[2],
              popup = paste(content))
}


# Value boxes -------------------------------------------------------------

# Reacts to map layers and displays coloured percent summaries
# testers...
# event_type <- "MHW"; cat_choice <- "I Moderate"
value_box_cat <- function(layer_choice, cat_choice, mapCover){
  
  # Set colour palette
  if(layer_choice == "MCS Category"){
    event_colours <- c(base_colours[2], MCS_colours)
  } else {
    event_colours <- c(base_colours[1], MHW_colours)
  }
  names(event_colours)[1] <- "Total cover"
  
  # Get values
  event_col <- event_colours[names(event_colours) == cat_choice]
  mapCover$text_class = c("text-dark", "text-dark", "text-light", "text-light", "text-dark")
  mapVal <- filter(mapCover, category == cat_choice)
  
  # Create box
  value_box(title = mapVal$category, value = mapVal$cat_area_prop*100, showcase = bs_icon("percent"),
            style = paste0("background-color: ",event_col,"!important;"), class = mapVal$text_class)
}

# Load MHW cat file and include the date ----------------------------------

# Daily files
readRDS_date <- function(file_name){
  file_segments <- length(strsplit(file_name, "/")[[1]])
  file_date <- sapply(strsplit(file_name, "/"), "[[", file_segments)
  file_date <- as.Date(stringr::str_remove_all(file_date, "[daily.cat.clim.Rda.Rds.MCS]"))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date) %>% 
    dplyr::select(t, lon, lat, everything())
}

# Annual files
readRDS_year <- function(file_name){
  file_segments <- length(strsplit(file_name, "/")[[1]])
  file_only <- sapply(strsplit(file_name, "/"), "[[", file_segments)
  file_only <- stringr::str_remove_all(file_only, "[.Rds]")
  file_only_segments <- length(strsplit(file_only, "_")[[1]])
  file_year <- as.integer(sapply(strsplit(file_only, "_"), "[[", file_only_segments))
  res <- readRDS(file_name) %>% 
    mutate(t = file_year) %>% 
    dplyr::select(t, lon, lat, everything())
}


# Plotly flames function --------------------------------------------------
# NB: This used to be in the heatwaveR package but needed to be removed
# when the plotly package was orphaned around Christmas time of 2020... 

geom2trace.GeomFlame <- function(data, params, p){
  
  x <- y <- y2 <- NULL
  
  # Create data.frame for ease of use
  data1 <- data.frame(x = data[["x"]],
                      y = data[["y"]],
                      y2 = data[["y2"]])
  
  # Grab parameters
  n <- params[["n"]]
  n_gap <- params[["n_gap"]]
  
  # Find events that meet minimum length requirement
  data_event <- heatwaveR::detect_event(data1, x = x, y = y,
                                        seasClim = y,
                                        threshClim = y2,
                                        minDuration = n,
                                        maxGap = n_gap,
                                        protoEvents = T)
  
  # Detect spikes
  data_event$screen <- base::ifelse(data_event$threshCriterion == FALSE, FALSE,
                                    ifelse(data_event$event == FALSE, TRUE, FALSE))
  
  # Screen out spikes
  data1 <- data1[data_event$screen != TRUE,]
  
  # Prepare to find the polygon corners
  x1 <- data1$y
  x2 <- data1$y2
  
  # # Find points where x1 is above x2.
  above <- x1 > x2
  above[above == TRUE] <- 1
  above[is.na(above)] <- 0
  
  # Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points <- which(diff(above) != 0)
  
  # Find the slopes for each line segment.
  x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]
  
  # # Find the intersection for each segment.
  x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
  y.points <- x1[intersect.points] + (x1.slopes * (x.points - intersect.points))
  
  # Coerce x.points to the same scale as x
  x_gap <- data1$x[2] - data1$x[1]
  x.points <- data1$x[intersect.points] + (x_gap*(x.points - intersect.points))
  
  # Create new data frame and merge to introduce new rows of data
  data2 <- data.frame(y = c(data1$y, y.points), x = c(data1$x, x.points))
  data2 <- data2[order(data2$x),]
  data3 <- base::merge(data1, data2, by = c("x","y"), all.y = T)
  data3$y2[is.na(data3$y2)] <- data3$y[is.na(data3$y2)]
  
  # Remove missing values for better plotting
  data3$y[data3$y < data3$y2] <- NA
  missing_pos <- !stats::complete.cases(data3[c("x", "y", "y2")])
  ids <- cumsum(missing_pos) + 1
  ids[missing_pos] <- NA
  
  # Get the correct positions
  positions <- data.frame(x = c(data3$x, rev(data3$x)),
                          y = c(data3$y, rev(data3$y2)),
                          ids = c(ids, rev(ids)))
  
  # Convert to a format geom2trace is happy with
  positions <- plotly::group2NA(positions, groupNames = "ids")
  positions <- positions[stats::complete.cases(positions$ids),]
  positions <- dplyr::left_join(positions, data[,-c(2,3)], by = "x")
  if(length(stats::complete.cases(positions$PANEL)) > 1) 
    positions$PANEL <- positions$PANEL[stats::complete.cases(positions$PANEL)][1]
  if(length(stats::complete.cases(positions$group)) > 1) 
    positions$group <- positions$group[stats::complete.cases(positions$group)][1]
  
  # Run the plotly polygon code
  if(length(unique(positions$PANEL)) == 1){
    getFromNamespace("geom2trace.GeomPolygon", asNamespace("plotly"))(positions)
  } else{
    return()
  }
}


