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
    
    # The main menu panel
    absolutePanel(top = menu_panel_top, right = menu_panel_right, draggable = T, width = "200px",
                  dateInput(inputId = ns("date_choice"),
                            label = "Date",
                            # value = MHW_cat_clim_sub$t[MHW_cat_clim_sub$intensity == max(MHW_cat_clim_sub$intensity)][1],
                            value = date_menu_choice,
                            min = min(current_dates), max = max(current_dates)),
                  # radioButtons(inputId = ns("pixels"),
                  #                    label = "Pixels",
                  #                    choices = c("Smooth", "Course"),
                  #                    selected = c("Course"),
                  #                    inline = T),
                  shinyWidgets::checkboxGroupButtons(inputId = ns("categories"),
                                       label = "Categories",
                                       choices = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                       selected = c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                       # status = "primary",
                                       checkIcon = list(yes = icon("ok", lib = "glyphicon"),
                                                        no = icon("remove", lib = "glyphicon"))),
                  shinyWidgets::materialSwitch(inputId = ns("legend"), label = "Legend", inline = F, 
                                 value = TRUE, status = "success"),
                  uiOutput(outputId = ns("button_ts"))
    ),
    uiOutput(ns('uiModal'))
  )
  
}
# cat("\nmap_ui.R finished")

