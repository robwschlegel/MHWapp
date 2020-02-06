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
    
    ### The map background changing button
    absolutePanel(id = ns("map_controls"), top = menu_panel_top, left = 10,
                  shinyWidgets::dropdownButton(
                    shinyWidgets::prettyRadioButtons(inputId = ns("map_back"),
                                                     label = h3("Background"),
                                                     choiceNames = list("Default", "Grey", "Countries", "Oceans"),
                                                     choiceValues = list("Default", "Grey", "Countries", "Oceans"),
                                                     selected = "Default",
                                                     status = 'primary', shape = "curve", animation = "tada"),

                    circle = TRUE, status = "primary",
                    icon = icon("map-marked"), width = "50px", right = FALSE, up = FALSE,
                    label = "Click to choose map background", tooltip = TRUE)
                  ),
    
    ### The control panel
    absolutePanel(id = ns("controls"), class = "panel panel-default",
                  top = menu_panel_top, right = menu_panel_right, draggable = T, width = "150px",
                  
                  # actionButton(ns("toggleText"), "Toggle text"),
                  # textInput(ns('text'), 'Name', value = "Dean"),
                  
                  # Controls header
                  # h2("Controls"),
                  # actionButton("toggle", h2("Controls")),
                  shinyWidgets::actionBttn(
                    inputId = ns("toggle"),
                    label = h3("Controls"), 
                    style = "minimal",
                    color = "royal"
                  ),
                  # Container that hides the controls
                  div(id = ns("control_menu"),
                  # conditionalPanel(
                    # condition = "input.toggle % 2 == 0",
                    # "This text gets toggled on and off"
                  # ),
                  # hr(),
                  # Date selector
                  h5(""),
                  uiOutput(outputId = ns("date_reactive")),
                  # Animation UI
                  shinyWidgets::materialSwitch(inputId = ns("check_animate"), 
                                               label = "Animate", status = "primary"),
                  # The category filtering buttons
                  # h5("Categories"),
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

