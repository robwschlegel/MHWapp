mapUI <- function(id, label = 'map') {
  ns <- NS(id)
  
  fluidRow(
    #   absolutePanel(class = 'zoomPanel',
    #                 top = 60, left = "auto", right = 10, bottom = 'auto',
    #                 width = "auto", height = "auto",
    #                 verticalLayout(actionButton(ns("map_zoom_in"), "+", class = 'small-button', width = 35),
    #                                actionButton(ns("map_zoom_out"), "-", class = "small-button", width = 35))
    #   ),
    
    # fluidRow(
    #   absolutePanel(class = 'zoomPanel',
    #                 top = 60, left = 10, right = 'auto', bottom = 'auto',
    #                 width = "auto", height = "auto",
    #                 div(id = "divSearch",
    #                     inline(selectizeInput(inputId = ns("search"), label = NULL, 
    #                                           choices = c("", sites$Station),
    #                                           selected = "",
    #                                           options = list(
    #                                             placeholder = 'Search stations or click marker to begin...'),
    #                                           width = 300)),
    #                     inline(actionButton(ns("zoom_to"), label = NULL, icon  = icon('search'))),
    #                     bsTooltip(ns("zoom_to"), "Zoom to location", placement = "bottom", trigger = "hover",
    #                               options = NULL)))
    # ),
    
    # The plotly option
    # Could potentially replace leaflet with plot_mapbox() or something from plotly
    # Then draw a heatmap on top of the land polygons to show MHW categories
    # absolutePanel(leafletOutput(ns('map')), top = 0, left = 0,
    # plotlyOutput(ns('map'), height = "900px"),#, top = 0, left = 0,
    # right = 0, bottom = 0, height = '100%'),
    
    # The leaflet option
    absolutePanel(leafletOutput(ns('map')), top = 0, left = 0,
                  right = 0, bottom = 0, height = 'auto'),
    
    # absolutePanel(verbatimTextOutput(outputId = ns("click_info"), placeholder = T),
    #               bottom = 300, right = 10, draggable = TRUE),
    
    absolutePanel(top = 70, right = 10, draggable = TRUE, 
                  dateInput(inputId = ns("date_choice"),
                            label = "Date",
                            # value = MHW_cat_clim_sub$t[MHW_cat_clim_sub$intensity == max(MHW_cat_clim_sub$intensity)][1],
                            value = max(current_dates),
                            min = min(current_dates), max = max(current_dates)
                            # min = min(MHW_cat_clim_sub$t), max = max(MHW_cat_clim_sub$t)
                  ),
                  checkboxGroupInput(inputId = ns("categories"),
                              label = "Categories",
                              choices = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                              selected = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                              inline = T),
                  # verbatimTextOutput(outputId = ns("map_hover"), placeholder = TRUE)
                  checkboxInput(inputId = ns("legend"), label = "Legend", value = TRUE)
    ),
    
    uiOutput(ns('uiModal'))
    # bsModal('popUp', '', '', uiOutput(ns('uiModal')))
  )
  
}