# MHWapp/shiny/ui.R

ui <- page_fluid(
  
  autoWaiter(html = spin_pixel(), color = "grey"),
  
  # Main info ---------------------------------------------------------------

  title = "Marine Heatwave Tracker",
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  fillable = FALSE, 
  # header = list(cicerone::use_cicerone()), # Start guided tour
  

  # Summary boxes -----------------------------------------------------------

  # Summary boxes
  # TODO: Dropdown within total box to allow for choice of different base statistics
    # Could also allow choice of time period.
  uiOutput("valueBoxes"),
  
  
  # Map card ----------------------------------------------------------------

  # TODO: Make sidebar tab button larger
  # TODO: Get annual summaries back in
  # TODO: Allow downloading annual summary PDF figures with a text caption explaining the panels
  # TODO: Could show MCS and MHW at the same time
    # This could get tricky.. I suppose that is what the anomaly layer is for
    # Or rather just have MHW stats etc. dominate when both layers are shown.
  card(fill = FALSE, 
       full_screen = TRUE,
       min_height = "600px",
       layout_sidebar(
         # fillable = FALSE,
         class = "p-0",
         sidebar = sidebar(
           title = "Controls",
           bg = "grey",
           width = "15%",
           # class = "fw-bold font-monospace", 
           open = "open",
           accordion(
             multiple = TRUE, 
             open = "datePanel",
             # Ice mask
             accordion_panel(
               "Ice mask", icon = bsicons::bs_icon("snow2"),
               shinyWidgets::prettySwitch(inputId = "iceMask", label = NULL, value = FALSE, 
                                          status = "info", fill = TRUE, bigger = TRUE)
             ),
             # Baseline
             accordion_panel(
               "Baseline", icon = bsicons::bs_icon("calendar-range"),
               shinyWidgets::radioGroupButtons(inputId = "baseLine", label = NULL,
                                               choices = c("1982-2011", "1991-2020"),
                                               selected = "1982-2011")
             ),
             # Date selector
             accordion_panel(
               "Date", icon = bsicons::bs_icon("menu-app"), value = "datePanel",
               # uiOutput(outputId = "date_reactive")
               dateInput(inputId = "date",
                         label = NULL, width = '100%',
                         # value = date_menu_choice,
                         value = max(current_dates),
                         min = "1982-01-01",
                         max = max(current_dates))
               ),
             # Map layer selector
             # TODO: Consider rather having a MHW/MCS switch (colour changing appropriately)
             # And then the map layer selector was for daily, annual, etc.
             # Or anomalies , trends etc. (other) as a third option
             accordion_panel(
               "Map layer", icon = bsicons::bs_icon("map"),
               prettyRadioButtons(inputId = "layer", label = NULL,
                                  # choices = c(cat_layers, rb_layers),
                                  choices = c("MHW Category", "MCS Category"),
                                  selected = "MHW Category",
                                  # selected = "MCS Category", # For testing
                                  status = "primary",
                                  shape = "curve",
                                  inline = F)
             ),
             accordion_panel(
               "Download", icon = bsicons::bs_icon("save"),
               # Pick date range
               daterangepicker::daterangepicker(
                     inputId = "map_download_date",
                     label = "Date range",
                     options = daterangepicker::daterangepickerOptions(maxSpan = list("days" = 62)),
                     start = max(current_dates), end = max(current_dates),
                     min = "1982-01-01", max = max(current_dates)),
               # Choose download type
               prettyRadioButtons(inputId = "map_download_type",
                                  label = "File type",
                                  choiceNames = list(".csv", ".Rds"),
                                  choiceValues = list("csv", "Rds"),
                                  selected = "csv", inline = TRUE,
                                  status = 'primary', shape = "curve", animation = "tada"),
               downloadButton(outputId = "download_map",
                              label = "Download", class = 'small-dl')
               ),
             # About popup
             accordion_panel(
               "About", icon = bsicons::bs_icon("question-circle"),
               actionBttn("about_btn", label = NULL, icon = icon("book"), color = "primary", style = "bordered")
             )
           )
         ),
         leafletOutput("leaf_map")
       )
  ),


  # Time series card --------------------------------------------------------

  navset_card_tab(full_screen = TRUE,
                  height = "600px",
                  title = "Pixel data",
                  sidebar = sidebar(
                    title = "Controls",
                    bg = "grey",
                    width = "15%",
                    # class = "fw-bold font-monospace",
                    open = "open",
                    accordion(
                      multiple = TRUE,
                      open = NULL,
                      accordion_panel(
                        "Date range", icon = bsicons::bs_icon("menu-app"),
                        daterangepicker::daterangepicker(
                          label = NULL, inputId = "from_to",
                          options = daterangepicker::daterangepickerOptions(linkedCalendars = FALSE),
                          start = max(current_dates)-730, end = max(current_dates),
                          min = "1982-01-01", max = max(current_dates))
                      ),
                      accordion_panel(
                        "Download", icon = bsicons::bs_icon("save"), 
                        downloadButton(outputId = "download_clim",
                                       label = "Climatology & Threshold (csv)", class = 'small-dl'),
                        downloadButton(outputId = "download_event",
                                       label = "Event data (csv)", class = 'small-dl')
                      )
                    )#,
                    # sidebar = sidebar(position = "right", open = FALSE, title = "Select Y axis value",
                    #                   selectInput(inputId = "lolliY", label = NULL,
                    #                               choices = c("Max. Intensity (°C)", "Cum. Intensity (°C)"),
                    #                               selected = "Max. Intensity (°C)", multiple = FALSE))
                  ),
                  nav_panel(title = "Time series", value = "ts",
                            plotOutput("tsPlot")
                            # withSpinner(plotOutput("tsPlot"), type = 6, color = "#b0b7be")),
                            # plotlyOutput("tsPlotly")
                            ),
                  # TODO: Add y-axis control for lolliplot
                  nav_panel(title = "Lolliplot", value = "lolli", 
                            plotlyOutput("lolliPlotly")),
                  nav_panel(title = "Table", value = "table", 
                            DT::dataTableOutput("tsTable"))
  ),
)

