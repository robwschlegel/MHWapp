# MHWapp/shiny/ui.R

ui <- page_sidebar(
  
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
               shinyWidgets::prettySwitch(inputId = "iceMask", label = NULL, value = TRUE, 
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
               )
           )
         ),
         leafletOutput("leaf_map")
         # withSpinner(leafletOutput("leaf_map", height = "598px"), type = 6, color = "#b0b7be")
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
                      # TODO: Get these to be in a right sidebar
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
                            # NB: plotly is misbehaving itself again
                            plotOutput("tsPlot")
                            # withSpinner(plotOutput("tsPlot"), type = 6, color = "#b0b7be")),
                            # withSpinner(plotlyOutput("tsPlotly", height = "465px"), type = 6, color = "#b0b7be")),
                            # plotlyOutput("tsPlotly")
                            ),
                  # TODO: Add y-axis control for lolliplot
                  nav_panel(title = "Lolliplot", value = "lolli", 
                            plotlyOutput("lolliPlotly")),
                            # withSpinner(plotlyOutput("lolliPlotly", height = "465px"), type = 6, color = "#b0b7be")),
                  nav_panel(title = "Table", value = "table", 
                            DT::dataTableOutput("tsTable"))
                            # withSpinner(DT::dataTableOutput("tsTable", height = "465px"), type = 6, color = "#b0b7be"))
  ),
  

  # About sidebar -----------------------------------------------------------

  # About info served as a sidebar
  sidebar = sidebar(open = "closed", width = "75%", position = "right",
                    h2(tags$b("Welcome!")),
                    br(),
                    p("You have arrived at the Marine Heatwave (MHW) Tracker. This web application shows the occurrence of MHWs 
                    around the world in near-real-time (roughly a one-two day delay). The Tracker also shows the historic daily records
                    for the whole planet going back to January 1st, 1982. It is also possible to look at the current and historic records
                    for Marine Cold-Spells (MCS). Please see the 'How do I use this?' section below for details."),
                    br(),
                    h2(tags$b("What is a Marine Heatwave (MHW)?")),
                    br(),
                    p("A MHW is generally defined as when the temperature in a given location is in the top 10% of temperatures 
                    recorded during that time of year for at least 5 straight days. For example, if the coastal waters off
                    Durban in South Africa are roughly 22°C on any given April 1st, if temperatures in excess of perhaps 28°C 
                    are recorded there in 2019 over March 28th to April 10th, this could be flagged as a MHW. This is a definition
                    for MHWs first put forward by ", 
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "https://www.sciencedirect.com/science/article/pii/S0079661116000057", "Hobday et al. (2016)"),
                      ". For a more detailed explanation with visuals please follow this ", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "http://www.marineheatwaves.org/all-about-mhws.html", "link"), 
                      ". An interactive demo for how to detect a MHW is available at this ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "http://193.50.85.71:3838/demoMHW/", "link"),"."),
                    br(),
                    h2(tags$b("What do these colours mean?")),
                    br(),
                    p("In the map panel of the Marine Heatwave Tracker we can see that there is a particular colour palette being used. Each of 
                      these four colours corresponds to increasing categories of MHWs as first proposed in ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.jstor.org/stable/26542662", "Hobday et al. (2018)"), 
                      ". The first category, 'I Moderate', is somewhat common and no category one MHWs have yet been recorded as causing lasting 
                      ecological or financial damage. The second category of MHWs, 'II Strong', are increasing in occurrence the most rapidly 
                      of the four categories and are on course to become as common as category one MHWs were when record keeping began in the 80's. 
                      Fortunately, category two MHWs have rarely been documented to cause lasting damage. The third category, 'III Severe', 
                      are less common but can be devastating when they persist for more than a month. The last category, 'IV Extreme', 
                      is thankfully a rare occurrence. It is known that less than three months of a persistent category four MHW can wipe 
                      out entire coastal ecosystems. If we switch the map layer to MCSs we will see a fifth category 'V Ice'. This was an 
                      addition to the category naming convention from ",
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "https://www.sciencedirect.com/science/article/abs/pii/S0079661121001683", "Schlegel et al. (2021)"),".
                      This category shows when a cold event is being detected at a threshold below -1.7°C. This basically means we are 
                      just looking at ice that is a bit colder than normal. Not really a proper MCS by the intended spirit of the definition."),
                    br(),
                    h2(tags$b("Why should I care?")),
                    br(),
                    p("If you've found your way to this website then you are likely interested in the effects we are having on the worlds 
                    oceans and probably have your own personal reasons to care about their health. There are many anthropogenic (human caused) 
                    threats to the health of the oceans, which include but are not limited to: over-fishing, chemical run-off from land, 
                    and climate change in the form of extreme warm water events known as MHWs. All of these different threats may impact 
                    the ocean in different ways, but one of the main concerns is that we are changing the oceans so much that we will not 
                    be able to repair them ourselves. MHWs are not new to our oceans, but our ability to quantify them and put them up on 
                    a website like this is. Thanks to this tool we can now see for ourselves in near-real-time where in the world extreme 
                    temperatures may be threatening the parts of the oceans where we work, live, and play. It is our hope that this website 
                    can be used around the world to help anyone that is interested to see if the changes they have noticed in their ocean are
                    due to a MHW or not."),
                    br(),
                    h2(tags$b("How do I use this?")),
                    br(),
                    p("This site works similarly to the Google maps we use in day-to-day life. Click and drag to move around the world. 
                    Use the mouse scroll wheel, or +/- buttons, to zoom in or out. The 'Date' box in the 'Controls' panel tells us which 
                    date is being shown on the map. Clicking in this box we can choose any date from the near present back to January 1st, 1982.
                    Click on 'Map layer' to bring up the options for which type of data to visualise. Curently one may choose between daily MHW or 
                    MCS data. Please see the 'What are these different map layers?' section below for more detail. Click on the 'Map data' button 
                    to download data for the chosen map layer over a range of dates. Note that downloads are limited to 62 days of data. To disable
                    white ice mask on the map, click on 'Ice mask' and toggle the switch off."),
                    p("If one of the pixels on the map catches your eye, clicking on it will give more information in the window below the map. 
                    There we can find three panels. The first, 'Time series', shows the daily data for the pixel we clicke don. Using the 
                    'Date range' selector we can extend the time period we want to display. The second tab, 'Lolliplot', shows all of the 
                    MHW/MCS in the selected date range, but focuses on just the events. This plot is interactive and we can move our mouse
                    over the points to see more info. Lastly, the 'Table' tab shows the metrics for all of the events in the chosen date range. 
                    We can sort the events by clicking on the different columns. We can download the climatology & threshold data as well as 
                    the MHW/MCS data for the chosen pixel with the download buttons at the bottom of the control panel. 
                    To download the temperature data please go to the", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "NOAA website"),"."),
                    br(),
                    h2(tags$b("Where do these data come from?")),
                    br(),
                    p("The global satellite product used in the Marine Heatwave Tracker is the daily Optimally Interpolated Sea Surface Temperature
                    (OISST) v2.1 data on an even 1/4 degree grid that may be downloaded from the National Oceanic and Atmospheric Administration (NOAA). 
                    The daily values go back as far as September 1st, 1981, but the MHW Tracker only hosts results starting on January 1st, 1982 
                    as this is the first full year of data. More information about these data may be found ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "here"),"."),
                    p("Please note that the data are released in near-real-time, and then go through a second layer of quality control that
                    takes roughly two weeks. Therefore any MHW results shown in this app within two weeks of the current date may be 
                    subject to minor changes. All MHW results older than two weeks may be taken as final. In practice, the difference between
                    the preliminary results and the final results are almost always negligible."),
                    p("A tutorial for how to download these data in R may be found ", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html", "here"),
                      ", and a tutorial on how to calculate MHWs from these data in R may be found ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html", "here"),"."),
                    p("The MHW/MCS results on the Marine Heatwave Tracker are calculated with the R version of the Hobday et al. (2016, 2018) definition 
                      briefly outlined above. Extensive documentation on the R code may be found ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/index.html", "here"),". This algorithm is also available for", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://github.com/ecjoliver/marineHeatWaves", "python ")," and ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://github.com/ZijieZhaoMMHW/m_mhw1.0", "MATLAB"),
                      ". The climatology period used for calculating MHWs and MCS is the current recommended WMO 
                      climate normal of 1991-01-01 to 2020-12-31."),
                    br(),
                    h2(tags$b("What are these different map layers?")),
                    br(),
                    p("The default map layer, 'MHW Category', shows the categories of the MHW occurring at each pixel on the chosen day. 
                      One may also choose 'MCS Category', which shows the same, but for cold events. Annual summaries and daily anomaly values
                      also exist in the MHW Tracker database, but this functionality has not yet been reintroduced to v2.0 of the Tracker."),
                    ### To add back in once these layers have been reactivated. Also add ability to download annual summary .png files
                    # p("The 'Summary' layer shows the highest category MHW that occurred at each pixel over the course of the chosen year.
                    # The 'Anomaly' layer shows the temperature anomalies for the chosen day against the daily climatology from 
                    # 1982-01-01 to 2011-12-31. The various 'Trend' layers show the annual trend in the change of the MHW metric referred to.
                    # For example, a value of 0.2 at a pixel when looking at the 'Trend: Count' layer would mean that for the last 30+ years
                    # MHWs at that pixel have been increasing by a count of 0.2 every year. Or in other words, every five years (1/0.2) there is
                    # one additional MHW occurring in that pixel per year. For another example let’s look at the 'Trend: Duration' layer.
                    # If we see a pixel here with a value of one, this means that, on average, MHWs in this pixel are becoming one day longer each year.
                    # Conversely, a value of negative one, as seen throughout the eastern Pacific, would mean that MHWs are shortening by one day each year.
                    # The data for these trends are the underlying data for Figure 1 in ",
                    #   a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.nature.com/articles/s41467-018-03732-9/", "Oliver et al., 2018"),
                    #   " and a more detailed description for them may be found in that publication."),
                    br(),
                    h2(tags$b("Who made this?")),
                    br(),
                    p("The Marine Heatwave Tracker was developed by ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://theoceancode.netlify.app/", "Robert Schlegel"),
                      " and is an outcome of the ", 
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "http://www.marineheatwaves.org/", "Marine Heatwaves International Working Group"),
                      ". Therefore, this work has been directly and indirectly supported by several governmental, academic, and private
                      organisations/funding bodies. The full list with links to further information is provided below in alphabetical order:"),
                    column(1), p("AU - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.aber.ac.uk/en/", 
                                            "Aberystwyth University")),
                    column(1), p("AIMS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.aims.gov.au/", 
                                              "Australian Institute of Marine Science")),
                    column(1), p("CLEX - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://climateextremes.org.au/", 
                                              "ARC Centre of Excellence for Climate Extremes")),
                    column(1), p("CSIRO - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.csiro.au/", 
                                               "Commonwealth Scientific and Industrial Research Organisation")),
                    column(1), p("DAL - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.dal.ca/", 
                                             "Dalhousie University")),
                    column(1), p("MBA - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.mba.ac.uk/", 
                                             "Marine Biological Association")),
                    column(1), p("MEOPAR - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://meopar.ca/",
                                                "Marine Environmental Observation, Prediction and Response Network")),
                    column(1), p("NESP - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://nespclimate.com.au/", 
                                              "National Environmental Science Programme")),
                    column(1), p("OFI - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.oceanfrontierinstitute.com/", 
                                             "Ocean Frontier Insitute")),
                    column(1), p("SAMS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.sams.ac.uk/", 
                                              "The Sottish Association for Marine Science")),
                    column(1), p("UC - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.canterbury.ac.nz/", 
                                            "University of Canterbury")),
                    column(1), p("UNSW - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.unsw.edu.au/", 
                                              "University of New South Wales")),
                    column(1), p("UTAS - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://www.utas.edu.au/", 
                                              "University of Tasmania")),
                    column(1), p("UW - ", a(target = '_blank', rel = 'noopener noreferrer', href = "http://www.washington.edu/", 
                                            "University of Washington")),
                    column(1), p("UWA - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.uwa.edu.au/", 
                                             "The University of Western Australia")),
                    column(1), p("UWC - ", a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.uwc.ac.za", 
                                             "University of the Western Cape")),
                    br(),
                    h2(tags$b("Bugs?")),
                    br(),
                    p("This app was developed for the Firefox web browser. Therefore the first fix for any observed bugs
                      is to re-open the Marine Heatwave Tracker in Firefox."),
                    p("The Tracker is visually heavy and may appear clumsy on smaller screens (e.g. cell phones).
                      The app has been optimised for use on mobile devices as much as is possible."),
                    p("Occasionally the marine heatwave polygons in the time series plots do not render correctly. 
                      Changing the date selection range will allow the figure to re-render correctly."),
                    p("Very rarely when the app starts up no MHW pixels will be displayed. Refreshing the website will fix this."),
                    # p("The map panel of the app may not render on a machine running on an Apple operating System. This is likely due to a 
                    #   Javascript extension issue and is not caused by the MHW Tracker."),
                    p("The map panel may not show up on some computeres running on the Windows operating systems. This is usually remedied
                      by accessing the Tracker in Internet Explorer / Microsoft Edge."),
                    p("If the MHW pixels appear, but the map remains grey, this is usually due to internet speed. Refreshing often allows the map
                      tiles to render correctly."),
                    p("If problems with viewing the map panel persist, one is encouraged to update the javascript plugins on one's browser of choice."),
                    br(),
                    p("To report any bugs or to provide any other feedback on the app please contact the developer at:
                      robwschlegel@gmail.com"),
                    br(),
                    h2(tags$b("How do I cite this app?")),
                    br(),
                    p("To ",
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "https://github.com/robwschlegel/MHWapp/blob/master/CITATION", "cite"),
                      " the app itself please use:"),
                    p("Schlegel, R. W. (2020). Marine Heatwave Tracker. http://www.marineheatwaves.org/tracker. doi: 10.5281/zenodo.3787872"),
                    br(),
                    h2(tags$b("Is the code for the Tracker open source?")),
                    br(),
                    p("Yes. The source code for the MHW Tracker is freely available in a ",
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://github.com/robwschlegel/MHWapp", "GitHub repo"), 
                      "and is protected under the terms of the",
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "https://github.com/robwschlegel/MHWapp/blob/master/LICENSE.md","MIT License"), ". For questions about the
                      use or adaptation of the source code please contact the developer at: robert.schlegel@imev-mer.fr"),
                    br(),
                    h2(tags$b("In the press")),
                    br(),
                    p("A press release was issued for the Marine Heatwave Tracker on May 27th, 2019. A link to the initial release on
                      the Ocean Frontier Institute (OFI) page may be found ", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://oceanfrontierinstitute.com/news/news/the-ocean-feels-the-heat", "here"), "."),
                    br(),
                    h2(tags$b("References")),
                    br(),
                    p("Any use of the NOAA OISST data should be accompanied by the following two reference:"),
                    p("Reynolds, R. W., Smith, T. M., Liu, C., Chelton, D. B., Casey, K. S., and Schlax, M. G. (2007). 
                      Daily high-resolution-blended analyses for sea surface temperature. J. Clim. 20, 5473–5496. doi: 10.1175/2007JCLI1824.1"),
                    p("Huang, B., Liu, C., Freeman, E., Graham, G., Smith, T., & Zhang, H. M. (2021). Assessment and intercomparison of 
                      NOAA daily optimum interpolation sea surface temperature (DOISST) version 2.1. Journal of Climate, 34(18), 7421-7441."),
                    br(),
                    p("The marine heatwave data displayed in this app were calculated in R with the package heatwaveR. To cite
                      heatwaveR in publications please use:"),
                    p("Schlegel, R. W., and Smit, A. J. (2018). heatwaveR: a central algorithm for the detection of heatwaves 
                      and cold-spells. J. Open Sour. Softw. 3:821. doi: 10.21105/joss.00821"),
                    br(),
                    p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
                    p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C. J., et al. (2016). 
                      A hierarchical approach to defining marine heatwaves. Progr. Oceanogr. 141, 227–238. doi: 10.1016/j.pocean.2015.12.014"),
                    p("Hobday, A. J., Oliver, E. C. J., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., et al. (2018). 
                      Categorizing and naming marine heatwaves. Oceanography 31, 162–173. doi: 10.5670/oceanog.2018.5205"),
                    ### To add back if the stats layers are re-introduced.
                    # br(),
                    # p("The various trend map layers are visualisations of the base results from:"),
                    # p("Oliver, E. C., Donat, M. G., Burrows, M. T., Moore, P. J., Smale, D. A., Alexander, L. V., ... & Holbrook, N. J. (2018).
                    #   Longer and more frequent marine heatwaves over the past century. Nature communications, 9(1), 1-12."),
                    br(),
                    h3(tags$b("Shiny server instance: ")),
                    h5(paste0(Sys.getenv("R_SHNYSRVINST"))),
                    # br()
                    # h2(tags$b("Session info")),
                    # # p(textOutput(ns("sessionInfo")))
                    # actionButton(ns("button"), "don't press the button"),
                    # hidden(
                    #   div(id=ns('text_div'),
                    #       verbatimTextOutput(ns("text"))
                    #   )
                    # ),
                    # br(),
                    br()
  )
)

