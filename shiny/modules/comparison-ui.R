comparisonUI <- function(id, label = 'comparison') {
  ns <- NS(id)
  
  dashboardPage(skin = "black",
                
                # The app title
                dashboardHeader(disable = TRUE),
                
                # The primary options
                dashboardSidebar(
                  sidebarMenu(id = ns("comparisonMenu"),
                              menuItem("Annual", tabName = "annual", icon = icon("chart-bar")),
                              menuItem("Daily", tabName = "daily", icon = icon("chart-bar")),
                              menuItem("Map", tabName = "map", icon = icon("map")),
                              # menuItem("Tables", tabname = "tables", icon = icon("table")),
                              menuItem("About", tabName = "about", icon = icon("question"), selected = TRUE),
                              # The reactive controls based on the primary option chosen
                              uiOutput(outputId = ns("sidebar_controls")))
                ),
                
                # The dashboard
                dashboardBody(
                  tabItems(
                    
                    
                    # Total figures -----------------------------------------------------------
                    
                    tabItem(tabName = "annual",
                            fluidRow(box(plotlyOutput(ns("totalCount")), width = 12, 
                                         title = "Average daily MHW occurrence per year (% of ocean)",
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("totalFirst")), width = 12, 
                                         title = "Total coverage per year (% of ocean)",
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("totalCum")), width = 12, 
                                         title = "Total MHW days per pixel per year",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE))),
                    
                    
                    # Daily figures -----------------------------------------------------------
                    
                    tabItem(tabName = "daily",
                            fluidRow(box(plotlyOutput(ns("dailyCount")), width = 12, title = "Daily count (% of ocean)",
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("dailyFirst")), width = 12, title = "Total coverage (% of ocean)",
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("dailyCum")), width = 12, title = "MHW days per pixel",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE))),
                    
                    # Map figures -------------------------------------------------------------
                    
                    tabItem(tabName = "map",
                            fluidRow(box(plotOutput(ns("compOISST")), width = 4, title = "OISST",
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotOutput(ns("compCCI")), width = 4, title = "CCI",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotOutput(ns("compCMC")), width = 4, title = "CMC",
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE)),
                            fluidRow(box(plotlyOutput(ns("compLat")), width = 12, title = "Latitude",
                                         status = "danger", solidHeader = TRUE, collapsible = TRUE))),
                    
                    
                    # Tables ------------------------------------------------------------------
                    
                    # tabItem(tabname = "tables",
                    #         # tableOutput("resultsKable"),
                    #         h2("test")),
                    
                    
                    # App explanation ---------------------------------------------------------
                    
                    tabItem(tabName = "about", 
                            fluidPage(
                              column(12,
                                     h2(tags$b("About")),
                                     p("This tab of the MHW Tracker contains comparisons of three different sea surface 
                                       temperature (SST) products:",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.ncdc.noaa.gov/oisst", "NOAA OISST"), ", ",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.eea.europa.eu/data-and-maps/data/external/esa-sst-cci-level-4", "ESA CCI SST"), ", and ",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.eea.europa.eu/data-and-maps/data/external/esa-sst-cci-level-4", "CMC SST"), ".
                                       The purpose of these comparisons is to determine how consistent the detection of MHWs may be between these
                                       different products. Because these products are all based on infrared SST detection any large differences 
                                       in the results should be based on the different data assimilation methods used by these different organisations.
                                       It was revealed through these comparisons that the most important such difference was the handling of ocean pixels 
                                       that experienced any ice cover throughout the year. It appears that there are fundamental differences in the 
                                       ice cover algorithms used between these products and any MHW results ~60°N/S or further poleward are not
                                       comparable between these three products. The following sections explain the functioning of this part of the MHW
                                       Tracker."),
                                     h2(tags$b("Annual")),
                                     p("Clicking on the 'Annual' tab at the top of the menu bar to the left will open a new tab screen that
                                       shows three line graphs. The x-axis for each line graph is in years. The first line graph shows the average daily 
                                       % of the ocean that experienced MHWs during a given year. The second line graph shows the total % of the ocean that
                                       experienced at least one MHW in a given year. The third line graph shows the average number of MHW days experienced
                                       by the entire ocean. On each graph their are separate lines for each category of MHW and each product. One may turn
                                       lines on and off by clicking on the corresponding category/product name in the legend. One may also zoom in on any
                                       area of a plot by clicking and dragging like a selection tool. Hovering over any area of a plot will bring up 
                                       additional information. Hovering over any graph will also bring up a menu bar on the top right with additional
                                       controls to explore. In the menu bar on the left one may also choose which climatology period to use for the 
                                       comparison of the annual results. The climatology period that most closely matches the current WMO standard would
                                       be '1982-2011' however; the first full year of the CMC product is 1992, meaning that to compare all three the
                                       climatology period '1992-2018' must be used."),
                                     h2(tags$b("Daily")),
                                     p("This tab contains a similar set of three line graphs as to the 'Annual' tab above it. The primary difference being
                                       that the x-axis of each figure is now in days, rather than years. One may now choose in the menu bar which 
                                       year to display. Note that the range of data available for the CMC product is 1992 - 2019, and for CCI it is
                                       1982 - 2018. All years are available for the OISST product."),
                                     h2(tags$b("Map")),
                                     p("The final tab, 'Map', shows three side-by-side global maps of the three SST products. The product shown
                                       is written in the top left of each panel. The maps themselves show the highest category MHW experienced per pixel
                                       for the year selected from the menu on the left. Below these three maps is a line graph that shows a comparison of
                                       the total number of pixels at each 1/4 degree longitude step from pole to pole that experienced at least one MHW
                                       during the selected year. Only the highest category event experienced is shown. One may interact with this line
                                       graph in the same way as the line graphs in the other two tabs."),
                                     p(tags$b("NB:")," The maps may take up to one minute to render due to their size."),
                                     h2(tags$b("Bugs")),
                                     p("To report any bugs or to provide any other feedback on the app please contact 
                                       the developer at: robert.schlegel@dal.ca"),
                                     h2(tags$b("References")),
                                     h4(tags$b("MHW")),
                                     p("The marine heatwave data displayed in this app were calculated with the heatwaveR R package. To cite
                                       heatwaveR in publications please use:"),
                                     p("Schlegel, R. W., and Smit, A. J. (2018). heatwaveR: a central algorithm for the detection of heatwaves 
                                       and cold-spells. J. Open Sour. Softw. 3:821. doi: 10.21105/joss.00821"),
                                     p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
                                     p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C. J., et al. (2016). 
                                       A hierarchical approach to defining marine heatwaves. Progr. Oceanogr. 141, 227–238. 
                                       doi: 10.1016/j.pocean.2015.12.014"),
                                     p("Hobday, A. J., Oliver, E. C. J., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., et al. (2018).
                                       Categorizing and naming marine heatwaves. Oceanography 31, 162–173. doi: 10.5670/oceanog.2018.5205"),
                                     h4(tags$b("NOAA OISST")),
                                     p("Any use of the NOAA OISST data should be accompanied by the following reference:"),
                                     p("Reynolds, R. W., Smith, T. M., Liu, C., Chelton, D. B., Casey, K. S., and Schlax, M. G. (2007). 
                                       Daily high-resolution-blended analyses for sea surface temperature. J. Clim. 20, 5473–5496. 
                                       doi: 10.1175/2007JCLI1824.1"),
                                     p("The use of v2.0 data should additionally be accompanied with this reference:"),
                                     p("Banzon, V., Smith, T. M., Chin, T. M., Liu, C., and Hankins, W. (2016). A long-term record of blended 
                                       satellite and in situ sea-surface temperature for climate monitoring, modeling and environmental studies. 
                                       Earth Syst. Sci. Data 8, 165–176. doi: 10.5194/essd-8-165-2016"),
                                     p("The use of v2.1 data should refer to this publication:"),
                                     p("Banzon, V., Smith, T. M., Steele, M., Huang, B., & Zhang, H. M. (2020). Improved Estimation of Proxy 
                                       Sea Surface Temperature in the Arctic. Journal of Atmospheric and Oceanic Technology, 37(2), 341-349. 
                                       doi: 10.1175/JTECH-D-19-0177.1"),
                                     h4(tags$b("ESA CCI SST")),
                                     p("Any use of the ESA CCI SST data should be accompanied by the following reference:"),
                                     p("Merchant, C. J., Embury, O., Bulgin, C. E., Block, T., Corlett, G. K., Fiedler, E., ... & Eastwood, S. 
                                       (2019). Satellite-based time-series of sea-surface temperature since 1981 for climate applications. 
                                       Scientific data, 6(1), 1-18. doi: 10.1038/s41597-019-0236-x"),
                                     h4(tags$b("CMC SST")),
                                     p("Any use of the CMC SST data should be accompanied by the following reference:"),
                                     p("Meissner, T., Wentz, F. J., Scott, J., & Vazquez-Cuervo, J. (2016). Sensitivity of ocean surface 
                                       salinity measurements from spaceborne L-band radiometers to ancillary sea surface temperature. 
                                       IEEE Transactions on Geoscience and Remote Sensing, 54(12), 7105-7111. doi: 10.1109/TGRS.2016.2596100")
                                     )
                              )
                            )
                    )
                  )
  )
}

