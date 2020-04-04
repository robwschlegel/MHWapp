mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidPage(

    ### Actvivate underlying javascript to R functionality
    shinyjs::useShinyjs(),
  
    ### Set the skin for the sliders used
    shinyWidgets::chooseSliderSkin(skin = 'Flat', color = "#ffc866"),

    ### The leaflet layer
    absolutePanel(top = 0, left = 0, right = 0, bottom = 0, height = 'auto',
                  leafletOutput(ns('map'))),
    
    ### The control panel
    absolutePanel(id = ns("controls"), class = "panel panel-default",
                  top = menu_panel_top, left = menu_panel_left, draggable = T, width = "150px",
                  # Controls header
                  # h2("Controls"),
                  div(class = "controlsbutton",
                      shinyWidgets::actionBttn(inputId = ns("toggle"), 
                                               h3("Controls"),
                                               style = "minimal", 
                                               color = "primary", 
                                               size = "sm", block = T)),
                  # Container that hides the controls
                  div(id = ns("control_menu"),
                      # Date selector
                      h5(""),
                      uiOutput(outputId = ns("date_reactive")),
                      # Animation UI
                      shinyWidgets::materialSwitch(inputId = ns("check_animate"), 
                                                   label = "Animate", status = "primary"),
                      # Layer selector
                      h5(""),
                      uiOutput(outputId = ns("layer_UI")),
                      # Trend layer selector
                      # h5(""),
                      # uiOutput(outputId = ns("trend_layer_UI")),
                      # Map data download
                      h5(""),
                      uiOutput(outputId = ns("download_map_UI")),
                      # The background selector
                      h5(""),
                      shinyWidgets::dropdownButton(
                        shinyWidgets::prettyRadioButtons(inputId = ns("map_back"),
                                                         label = h3("Background"),
                                                         choiceNames = list("Default", "Grey", "Land features", "Ocean features"),
                                                         choiceValues = list("Default", "Grey", "Land features", "Ocean features"),
                                                         selected = "Default",
                                                         status = 'primary', shape = "curve", animation = "tada"),
                        circle = FALSE, status = "primary",
                        #icon = icon("map-marked"), width = "50px", 
                        right = FALSE, up = FALSE,
                        label = "Background", tooltip = FALSE),
                      # The lon/lat/zoom controls
                      h5(""),
                      shinyWidgets::dropdownButton(
                        # div(id = ns("coords"),
                            numericInput(inputId = ns("lon"), label = "Lon", value = initial_lon, step = 10),
                            numericInput(inputId = ns("lat"), label = "Lat", value = initial_lat, step = 10),
                            numericInput(inputId = ns("zoom"), label = "Zoom", value = initial_zoom),
                            circle = FALSE, status = "primary",
                            #icon = icon("map-marked"), width = "50px", 
                            right = FALSE, up = FALSE,
                            label = "Coordinates", tooltip = FALSE),
                        # )
                      # The category filtering buttons
                      # h5("Categories"),
                      h5(""),
                      uiOutput(outputId = ns("moderate")),
                      uiOutput(outputId = ns("strong")),
                      uiOutput(outputId = ns("severe")),
                      uiOutput(outputId = ns("extreme")),
                      # The time series button
                      h5(""),
                      uiOutput(outputId = ns("button_ts"))#,
                      # The shiny server instance info
                      # hr(),
                      # h5(paste0("Instance: ",Sys.getenv("R_SHNYSRVINST")))
                  )
    ),
    
    ### The separate animation menu
    uiOutput(ns('date_animator')),
    
    ### The welcome popup
    uiOutput(ns('uiStartupModal')),
    
    ### The interactive time series panel
    uiOutput(ns('uiModal'))
  )
}

#cat("\nmap_ui.R finished")

