mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidPage(
    # The plotly option
    # Could potentially replace leaflet with plot_mapbox() or something from plotly
    # Then draw a heatmap on top of the land polygons to show MHW categories
    # absolutePanel(leafletOutput(ns('map')), top = 0, left = 0,
    # plotlyOutput(ns('map'), height = "900px"),#, top = 0, left = 0,
    # right = 0, bottom = 0, height = '100%'),
    # The leaflet option
    absolutePanel(top = 0, left = 0, right = 0, bottom = 0, height = 'auto',
                  leafletOutput(ns('map'))),
    # The main menu panel
    absolutePanel(id = ns("controls"), class = "panel panel-default",
                  top = menu_panel_top, right = menu_panel_right, draggable = T, width = "200px",
                  # The date input box
                  h2("Controls"),
                  # dateInput(inputId = ns("date_choice"),
                  #           label = "Date",
                  #           value = date_menu_choice,
                  #           min = "1982-01-01", 
                  #           max = max(current_dates)),
                  # The category filtering buttons
                  h5(paste0("Categories")),
                  uiOutput(outputId = ns("moderate")),
                  uiOutput(outputId = ns("strong")),
                  uiOutput(outputId = ns("severe")),
                  uiOutput(outputId = ns("extreme")),
                  # The shiny server instance info
                  h5(paste0("Shiny server instance: ",Sys.getenv("R_SHNYSRVINST"))),
                  # The time series button
                  uiOutput(outputId = ns("button_ts"))

    ),
    shinyWidgets::setSliderColor("BurlyWood", sliderId = 1),
    absolutePanel(bottom = 10, left = 30, draggable = F, width = "80%",
                  # The date input box
                  h1("Date:"),
                  shinyWidgets::sliderTextInput(
                    inputId = ns("date_choice"),
                    label = NULL,
                    grid = TRUE, 
                    force_edges = TRUE,
                    choices = seq(date_menu_choice-4, date_menu_choice, by = "day"), 
                    selected = date_menu_choice-4, 
                    animate = animationOptions(interval = 5000),
                    width = "80%"
                  )
                  # dateInput(inputId = ns("date_choice"),
                  #           label = "Date",
                  #           value = date_menu_choice,
                  #           min = "1982-01-01", 
                  #           max = max(current_dates))
                  # The shiny server instance info
                  # h5(paste0("Shiny server instance: ",Sys.getenv("R_SHNYSRVINST"))),
    ),
    tags$script("$(document).ready(function(){
                setTimeout(function() {$('.slider-animate-button').click()},2000);});"),
    # The welcome popup
    uiOutput(ns('uiStartupModal')),
    # The popup time series panel
    uiOutput(ns('uiModal'))
  )
}
# cat("\nmap_ui.R finished")

