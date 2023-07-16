mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  # fluidPage(
    
    ### Activate underlying javascript to R functionality
    # shinyjs::useShinyjs(),
  
    ### Set the skin for the sliders used
    # shinyWidgets::chooseSliderSkin(skin = 'Flat', color = "#ffc866"),

    ### The leaflet layer + its control panel
    card(
      full_screen = TRUE,
      # card_header("Map"),
      layout_sidebar(
        sidebar = sidebar(
          id = "control_menu",   
          title = "Controls",
          position = "right", 
          open = "closed",
          # Date selector
          h5(""),
          uiOutput(outputId = ns("date_reactive")),
          # Animation UI
          h5(""),
          uiOutput(outputId = ns("check_animate_UI")),
          # Layer selector
          h5(""),
          uiOutput(outputId = ns("layer_UI")),
          # Map data download
          h5(""),
          uiOutput(outputId = ns("download_map_UI")),
          # The background selector
          h5(""),
          shinyWidgets::dropdownButton(inputId = ns("map_back_menu"),
                                       shinyWidgets::prettyRadioButtons(inputId = ns("map_back"),
                                                                        label = h3("Background"),
                                                                        choiceNames = list("Default", "Grey", "Land features", "Ocean features"),
                                                                        choiceValues = list("Default", "Grey", "Land features", "Ocean features"),
                                                                        selected = "Default",
                                                                        status = 'primary', shape = "curve", animation = "tada"),
                                       circle = FALSE, status = "primary",
                                       right = FALSE, up = FALSE,
                                       label = "Background", tooltip = FALSE),
          # The lon/lat/zoom controls
          h5(""),
          shinyWidgets::dropdownButton(inputId = ns("coords"),
                                       # div(id = ns("coords"),
                                       numericInput(inputId = ns("lon"), label = "Lon", value = initial_lon, step = 10),
                                       numericInput(inputId = ns("lat"), label = "Lat", value = initial_lat, step = 10),
                                       numericInput(inputId = ns("zoom"), label = "Zoom", value = initial_zoom),
                                       circle = FALSE, status = "primary",
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
        ),
        leafletOutput(ns('map'))
      )
    )
    
    ### The separate animation menu
    uiOutput(ns('date_animator'))
    
    ### The interactive time series panel
    uiOutput(ns('uiModal'))
}

#cat("\nmap_ui.R finished")

