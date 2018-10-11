
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(leaflet)
library(raster)
library(rgdal)
library(htmlwidgets)
library(mapview)
library(RColorBrewer)


# Load data ---------------------------------------------------------------

# load("data/MHW_cat.RData")
load("../../data/MHW_cat.RData")

# The event categories
# MHW_cat_event <- MHW_cat %>% 
#   unnest(cat) %>% 
#   filter(row_number() %% 2 == 0) %>% 
#   unnest(cat)

# The event clims
MHW_cat_clim <- MHW_cat %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(cat) %>% 
  mutate(category = factor(category, levels = c("I Moderate", "II Strong", 
                                                "III Severe", "IV Extreme")))

# The category colour pallette
fillColCat <- c(
  "I Moderate" = "#ffc866",
  "II Strong" = "#ff6900",
  "III Severe" = "#9e0000",
  "IV Extreme" = "#2d0000"
)


# Base map ----------------------------------------------------------------

# start basemap
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  # add another layer with place names
  addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  # add graticules from a NOAA webserver
  addWMSTiles(
    "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
    layers = c("1-degree grid", "5-degree grid"),
    options = WMSTileOptions(format = "image/png8", transparent = TRUE),
    attribution = NULL,group = 'Graticules') %>%
  
  # focus map in a certain area / zoom level
  setView(lng = 25, lat = -35, zoom = 5) %>%
  
  # add layers control
  # addLayersControl(overlayGroups = c('Place names',
  #                                    'Graticules',
  #                                    'Points',
  #                                    'Lines',
  #                                    'Polygons'),
  #                  options = layersControlOptions(collapsed = FALSE),
  #                  position = 'topright') %>%
  
  # list groups to hide on startup
hideGroup(c('Place names'))

val <- c(1, 2)
pal <-  colorFactor(c("#ffc866", "#ff6900"), levels = val,
                    na.color = "transparent", ordered = T)


# UI ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   # titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a calendar input for dates
   pageWithSidebar(
     headerPanel('Map'),
     sidebarPanel(
       dateInput(inputId = "dates",
                 label = "Choose a day to visualise", 
                 value = MHW_cat_clim$t[MHW_cat_clim$intensity == max(MHW_cat_clim$intensity, na.rm = T)][1],
                 # start = min(MHW_cat_clim$t),
                 # end = max(MHW_cat_clim$t),
                 format = "yyyy-mm-dd",
                 startview = "month")
     ),
     # mainPanel(uiOutput('MHW_map')
     leafletOutput("MHW_map")
     )
      
      # # Show a plot of the generated distribution
      # mainPanel(
      #    plotOutput("MHW_map")
      # )
   )
# )


# Server ------------------------------------------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  MHW_sub <- reactive({
    MHW_sub <- MHW_cat_clim
    # Subset date
    date_choice <- input$dates
    MHW_sub <- MHW_sub[MHW_sub$t == date_choice,]
    MHW_sub <- MHW_sub[,c(1,2,5)]
    colnames(MHW_sub) <- c("X","Y","Z")
    MHW_sub$Z <- as.numeric(MHW_sub$Z)
    MHW_sub <- rasterFromXYZ(MHW_sub, res = c(0.25, 0.25), digits = 2, 
                             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") 
  })

  output$MHW_map <- renderLeaflet({
    map %>% 
      # addRasterImage(x = MHW_sub, colors = pal, opacity = 0.5, method = "ngb") %>%
      # leaflet::
      # addCircleMarkers(data = MHW_sub$MHW_sub, ~lon, ~lat,
      #                  weight = 1.0,
      #                  color = "grey",
      #                  fillColor = "white",
      #                  radius = 10,
      #                  fillOpacity = 0.0,
      #                  stroke = T,
      #                  label = ~paste0('Event at: ',
      #                                  as.character(round(lat,3)), ', ',
      #                                  as.character(round(lon,3))),
      #                  group = 'Points') %>%
      addScaleBar() #%>%
      # tileOptions(minZoom = 10) %>% 
      # addLegend(pal = pal, values = val, title = "MHW Category", labels = c("I Moderate", "II Strong"))
  })
  
}


# Run it ------------------------------------------------------------------

# Run the application 
shinyApp(ui = ui, server = server)

