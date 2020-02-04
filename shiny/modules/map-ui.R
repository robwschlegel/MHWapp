mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidPage(
    shinyWidgets::chooseSliderSkin(skin = 'Flat', color = "#ffc866"),
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
                  top = menu_panel_top, right = menu_panel_right, draggable = T, width = "150px",
                  # The date input box
                  h2("Controls"),
                  dateInput(inputId = ns("date"),
                            label = "Date",
                            value = date_menu_choice,
                            min = "1982-01-01",
                            max = date_menu_choice),
                  numericInput(inputId = ns("lon"), label = "lon", value = initial_lon),
                  numericInput(inputId = ns("lat"), label = "lat", value = initial_lat),
                  numericInput(inputId = ns("zoom"), label = "zoom", value = initial_zoom),
                  ## NB: This date format would be ideal but it hangs on loading for some reason...
                  # shinyWidgets::airDatepickerInput(
                  #   inputId = ns("date"),
                  #   label = "Date",
                  #   inline = TRUE,
                  #   value = date_menu_choice,
                  #   minDate = "1982-01-01",
                  #   maxDate = date_menu_choice,
                  #   update_on = 'close'
                  # ),
                  # The category filtering buttons
                  h5(paste0("Categories")),
                  uiOutput(outputId = ns("moderate")),
                  uiOutput(outputId = ns("strong")),
                  uiOutput(outputId = ns("severe")),
                  uiOutput(outputId = ns("extreme")),
                  # Animation UI
                  h5(""),
                  # shinyWidgets::actionBttn(inputId = ns("check_animate"), label = "Animate",
                  #                          style = "jelly", color = "royal", size = "md"),
                  # shinyWidgets::awesomeCheckbox(inputId = ns("check_animate"), 
                                                # label = "Animate", status = 'primary'),
                  shinyWidgets::materialSwitch(inputId = ns("check_animate"), 
                                               label = "Animate", status = "primary"),
                  # The time series button
                  h5(""),
                  # uiOutput(outputId = ns("button_ts")),
                  # The shiny server instance info
                  h5(paste0("Shiny server instance: ",Sys.getenv("R_SHNYSRVINST")))
    ),
    # The separate animation menu
    uiOutput(ns('date_animator')),
    # The welcome popup
    uiOutput(ns('uiStartupModal')),
    # The popup time series panel
    uiOutput(ns('uiModal'))
  )
}
# cat("\nmap_ui.R finished")

