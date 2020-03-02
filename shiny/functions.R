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
    left_join(tidync::hyper_tibble(tidync::tidync(seas_thresh_files[lon_row])), 
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


# Load MHW cat file and include the date ----------------------------------

readRDS_date <- function(file_name){
  file_segments <- length(strsplit(file_name, "/")[[1]])
  file_date <- sapply(strsplit(file_name, "/"), "[[", file_segments)
  file_date <- as.Date(stringr::str_remove_all(file_date, "[daily.cat.clim.Rda]"))
  res <- readRDS(file_name) %>% 
    mutate(t = file_date) %>% 
    dplyr::select(t, lon, lat, everything())
}


# Shiny testing -----------------------------------------------------------

# NB: This must be commented out unless being run explicitly
# NB: Doesn't currently work as it can't figure out the shiny server port
# library(shinytest)
# shinytest::recordTest("../shiny/", loadTimeout = 100000)


# Button testing ----------------------------------------------------------

# NB: Currently this only works when run on a local machine without modules

# library(leaflet)
# library(shiny)
# 
# # The issue with all of this is getting it to translate over to a module use case
# 
# ui <- fluidPage(
#   tags$div(id = "garbage"),  # Copy this disposal-div
#   leafletOutput("map"),
#   div(id = "Showcase")
# )
# 
# server <- function(input, output, session) {
#   
#   # --- Just for Show ---
#   
#   output$popup1 <- renderUI({
#     actionButton("Go1", "Go1")
#   })
#   
#   observeEvent(input$Go1, {
#     insertUI("#Showcase", where = "beforeEnd",
#              div("Button 1 is fully reactive."))
#   })
#   
#   output$popup2 <- renderUI({
#     actionButton("Go2", "Go2")
#   })
#   
#   observeEvent(input$Go2, {
#     insertUI("#Showcase", where = "beforeEnd", div("Button 2 is fully reactive."))
#   })
#   
#   output$popup3 <- renderUI({
#     actionButton("Go3", "Go3")
#   })
#   
#   observeEvent(input$Go3, {
#     insertUI("#Showcase", where = "beforeEnd", div("Button 3 is fully reactive."))
#   })
#   
#   output$popup4 <- renderUI({
#     actionButton("Go4", "Anywhere")
#   })
#   
#   observeEvent(input$Go4, {
#     insertUI("#Showcase", where = "beforeEnd", div("Popup button is fully reactive."))
#   })
#   
#   # --- End: Just for show ---
#   
#   # popupMaker is just to lighten code. But here you can see how to insert the popup.
#   popupMaker <- function(id) {
#     as.character(uiOutput(id))
#   }
#   
#   output$map <- renderLeaflet({
#     input$aaa
#     leaflet() %>%
#       addTiles() %>%
#       # addMarkers(lat = c(10, 20, 30),
#       # lng = c(10, 20, 30),
#       # popup = lapply(paste0("popup", 1:3), popupMaker)) %>%
#       
#       # Copy this part - it initializes the popups after the map is initialized
#       htmlwidgets::onRender(
#         'function(el, x) {
#         var target = document.querySelector(".leaflet-popup-pane");
#         
#         var observer = new MutationObserver(function(mutations) {
#         mutations.forEach(function(mutation) {
#         if(mutation.addedNodes.length > 0){
#         Shiny.bindAll(".leaflet-popup-content");
#         }
#         if(mutation.removedNodes.length > 0){
#         var popupNode = mutation.removedNodes[0];
#         
#         var garbageCan = document.getElementById("garbage");
#         garbageCan.appendChild(popupNode);
#         
#         Shiny.unbindAll("#garbage");
#         garbageCan.innerHTML = "";
#         }
#         });
#         });
#         
#         var config = {childList: true};
#         
#         observer.observe(target, config);
#   }')
#   })
#   
#   ### Observer to show pop-ups on click
#   observe({
#     click <- input$map_click
#     if(!is.null(click)){
#       showpos(x = click$lng, y = click$lat)
#     }
#   })
#   
#   ### Show popup on clicks
#   showpos <- function(x = NULL, y = NULL) {
#     xy_click <- c(x,y)
#     
#     
#     ### Add Popup to leaflet
#     leafletProxy("map") %>% 
#       # clearPopups() %>% 
#       addPopups(lng = xy_click[1], lat = xy_click[2],
#                 popup = paste0("Blahblah", "<br>", "<hr>","<div id=\"popup4\" class=\"shiny-html-output\"></div>")) #%>% 
#     #       htmlwidgets::onRender(
#     #         'function(el, x) {
#     #   var target = document.querySelector(".leaflet-popup-pane");
#     # 
#     #   var observer = new MutationObserver(function(mutations) {
#     #     mutations.forEach(function(mutation) {
#     #       if(mutation.addedNodes.length > 0){
#     #         Shiny.bindAll(".leaflet-popup-content");
#     #       }
#     #       if(mutation.removedNodes.length > 0){
#     #         var popupNode = mutation.removedNodes[0];
#     # 
#     #         var garbageCan = document.getElementById("garbage");
#     #         garbageCan.appendChild(popupNode);
#     # 
#     #         Shiny.unbindAll("#garbage");
#     #         garbageCan.innerHTML = "";
#     #       }
#     #     }); 
#     #   });
#     # 
#     #   var config = {childList: true};
#     # 
#     #   observer.observe(target, config);
#     # }')
#   }
# }
# 
# shinyApp(ui, server)