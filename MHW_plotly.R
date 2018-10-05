
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(plotly)


# Load data ---------------------------------------------------------------

load("data/MHW_res.RData")
load("data/MHW_cat.RData")


# Prep data ---------------------------------------------------------------

# The event metrics
MHW_event <- MHW_res %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(event)

# The climatologies
MHW_clim <- MHW_res %>% 
  unnest(event) %>% 
  filter(row_number() %% 2 == 1) %>% 
  unnest(event)

# The event categories
MHW_cat_event <- df %>% 
  unnest(cat) %>% 
  filter(row_number() %% 2 == 0) %>% 
  unnest(cat)

# Visualise ---------------------------------------------------------------

test <- MHW_cat %>% 
  filter(event_no == 1)
tp <- ggplot(data = test, aes(x = lon, y = lat, fill = category)) +
  geom_raster()
tp

ggplotly(tp)


# Alternative 1 -----------------------------------------------------------

library(plotGoogleMaps)
library(shiny)

runApp(list(
  ui = pageWithSidebar(
    headerPanel('Map'),
    sidebarPanel(""),
    mainPanel(uiOutput('mymap')
              )
  ),
  server = function(input, output){
    output$mymap <- renderUI({
      data(meuse)
      coordinates(meuse) = ~x+y
      proj4string(meuse) <- CRS("+init=epsg:28992")
      m <- plotGoogleMaps(meuse, filename = 'myMap1.html', openMap = F)
      tags$iframe(
        srcdoc = paste(readLines('myMap1.html'), collapse = '\n'),
        width = "100%",
        height = "400px"
      )
    })
  }
))


# Alternative 2 -----------------------------------------------------------


