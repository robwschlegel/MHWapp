mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidPage(

    shinyjs::useShinyjs(),
  
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
                  # plotOutput(ns('map_pixels'))),
    # The map background changing button
    absolutePanel(id = ns("map_controls"), top = menu_panel_top, left = 10,
                  # Change map background
                  shinyWidgets::dropdownButton(

                    tags$h3("Map backgrounds"),
                    
                    shinyWidgets::prettyRadioButtons(inputId = ns("map_back"),
                                                     label = NULL,
                                                     choiceNames = list("Default", "Grey", "Countries", "Oceans"),
                                                     choiceValues = list("Default", "Grey", "Countries", "Oceans"),
                                                     selected = "Default",
                                                     status = 'primary', shape = "curve", animation = "tada"),

                    circle = TRUE, status = "primary",
                    icon = icon("map-marked"), width = "50px", right = FALSE, up = FALSE,
                    label = "Click to choose map background", tooltip = TRUE)
                  ),
    # The main menu panel
    absolutePanel(id = ns("controls"), class = "panel panel-default",
                  top = menu_panel_top, right = menu_panel_right, draggable = T, width = "150px",
                  # The date input box
                  h2("Controls"),
                  # hr(),
                  uiOutput(outputId = ns("date_reactive")),
                  ## NB: This date format would be ideal but it hangs on loading for some reason...
                  # shinyWidgets::airDatepickerInput(
                  #   inputId = "defaultValue",
                  #   label = "With a default date:",
                  #   value = Sys.Date()-7
                  # ),
                  # Animation UI
                  shinyWidgets::materialSwitch(inputId = ns("check_animate"), 
                                               label = "Animate", status = "primary"),
                  
                  ## NB: Currently hiding the lon/lat controls
                  shinyjs::hidden(
                    div(id = ns("coords"),
                        numericInput(inputId = ns("lon"), label = "Lon", value = initial_lon, step = 10),
                        numericInput(inputId = ns("lat"), label = "Lat", value = initial_lat, step = 10),
                        numericInput(inputId = ns("zoom"), label = "Zoom", value = initial_zoom)
                        )
                  ),
                  # numericInput(inputId = ns("zoom"), label = "Zoom", value = initial_zoom),
                  # The category filtering buttons
                  h5("Categories"),
                  uiOutput(outputId = ns("moderate")),
                  uiOutput(outputId = ns("strong")),
                  uiOutput(outputId = ns("severe")),
                  uiOutput(outputId = ns("extreme")),
                  # hr(),
                  # The time series button
                  h5(""),
                  uiOutput(outputId = ns("button_ts")),
                  # hr(),
                  # Map data download
                  h5(""),
                  downloadButton(outputId = ns("download_map"),
                                 label = "Map data", class = 'small-dl'),
                  # hr(),
                  # The shiny server instance info
                  h5(paste0("Shiny server instance: ",Sys.getenv("R_SHNYSRVINST")))
    ),
    # The separate animation menu
    uiOutput(ns('date_animator')),
    # The welcome popup
    uiOutput(ns('uiStartupModal')),
    # The interactive time series panel
    uiOutput(ns('uiModal'))
  )
}
# cat("\nmap_ui.R finished")

