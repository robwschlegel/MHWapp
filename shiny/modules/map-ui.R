mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    tags$div(id = ns("garbage")),  # Copy this disposal-div
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
    absolutePanel(top = menu_panel_top, right = menu_panel_right, draggable = T, width = "200px",
                  # The date input box
                  dateInput(inputId = ns("date_choice"),
                            label = "Date",
                            value = date_menu_choice,
                            min = "1982-01-01", 
                            max = max(current_dates)),
                  # The category filtering buttons
                  h5(paste0("Categories")),
                  uiOutput(outputId = ns("moderate")),
                  uiOutput(outputId = ns("strong")),
                  uiOutput(outputId = ns("severe")),
                  uiOutput(outputId = ns("extreme")),
                  # The shiny server instance info
                  h5(paste0("Shiny server instance: ",Sys.getenv("R_SHNYSRVINST")))#,
                  # The time series button
                  # uiOutput(outputId = ns("button_ts"))

    ),
    # The welcome popup
    uiOutput(ns('uiStartupModal')),
    # The popup time series panel
    uiOutput(ns('uiModal'))
  )
}
# cat("\nmap_ui.R finished")

