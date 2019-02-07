mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    
    # The plotly option
    # Could potentially replace leaflet with plot_mapbox() or something from plotly
    # Then draw a heatmap on top of the land polygons to show MHW categories
    # absolutePanel(leafletOutput(ns('map')), top = 0, left = 0,
    # plotlyOutput(ns('map'), height = "900px"),#, top = 0, left = 0,
    # right = 0, bottom = 0, height = '100%'),
    
    # The leaflet option
    absolutePanel(top = 0, left = 0, right = 0, bottom = 0, height = 'auto',
                  leafletOutput(ns('map'))),
    
    # absolutePanel(verbatimTextOutput(outputId = ns("click_info"), placeholder = T),
    #               bottom = 300, right = 10, draggable = TRUE),
    
    # sidebarPanel(width = 12,
    absolutePanel(bottom = 20, left = 10, draggable = T,
                  # sidebarPanel(width = 12,
                  checkboxInput(inputId = ns("legend"), label = "Legend", value = TRUE),
                  checkboxGroupInput(inputId = ns("categories"),
                              label = "Categories",
                              choices = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                              selected = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                              inline = T),
                  actionButton(inputId = ns("open_modal"), label = "Pixel information", icon = icon("map-marked")),
                  dateInput(inputId = ns("date_choice"),
                  # sliderInput(inputId = ns("date_choice"), 
                            label = "Date",
                            # value = MHW_cat_clim_sub$t[MHW_cat_clim_sub$intensity == max(MHW_cat_clim_sub$intensity)][1],
                            value = max(current_dates),
                            min = min(current_dates), max = max(current_dates))
                  # verbatimTextOutput(outputId = ns("map_hover"), placeholder = TRUE)
    ),
    uiOutput(ns('uiModal'))
    # bsModal('popUp', '', '', uiOutput(ns('uiModal')))
  )
  
}
# cat("\nmap_ui.R finished")

