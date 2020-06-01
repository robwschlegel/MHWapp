summaryUI <- function(id, label = 'summary') {
  ns <- NS(id)
  
  dashboardPage(skin = "black",
                
                # The app title
                dashboardHeader(disable = TRUE),
                
                # The primary options
                dashboardSidebar(
                  sidebarMenu(id = ns("summaryMenu"),
                              menuItem("Annual", tabName = "summary_annual", icon = icon("chart-bar"), selected = TRUE),
                              menuItem("Total", tabName = "summary_total", icon = icon("chart-bar")),
                              menuItem("How To", tabName = "summary_about", icon = icon("question")),
                              # The reactive controls based on the primary option chosen
                              uiOutput(outputId = ns("summarySidebarControls"))
                              )
                ),
                
                # The dashboard
                dashboardBody(
                  tabItems(
                    
                    # Annual figures ----------------------------------------------------------
                    
                    tabItem(tabName = "summary_annual",
                            fluidRow(box(shinycssloaders::withSpinner(plotOutput(outputId = ns("summary_map")), type = 6, color = "#b0b7be"),
                                         width = 12, title = "Highest MHW category", status = "danger", solidHeader = TRUE, collapsible = TRUE)),
                            fluidRow(box(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("summary_daily_count")), type = 6, color = "#b0b7be"), 
                                         width = 4, title = "Daily MHW occurrence", status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(shinycssloaders::withSpinner(plotlyOutput(outputId = ns("summary_cum_perc")), type = 6, color = "#b0b7be"), 
                                         width = 4, title = "Total MHW coverage", status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(shinycssloaders::withSpinner(plotlyOutput(ns("summary_cum_days")), type = 6, color = "#b0b7be"), 
                                         width = 4, title = "Average MHW days per pixel", status = "success", solidHeader = TRUE, collapsible = TRUE))
                            ),
                    
                    
                    # Total figures -----------------------------------------------------------
                    
                    tabItem(tabName = "summary_total",
                            # fluidRow(box(imageOutput(outputId = ns("total_all")), 
                                         # width = 12, title = "Total summary", status = "primary", solidHeader = TRUE, collapsible = TRUE))
                            fluidRow(box(plotlyOutput(outputId = ns("total_daily_count")), width = 4, 
                                         title = "Daily MHW occurrence", 
                                         status = "primary", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(outputId = ns("total_cum_perc")), width = 4, 
                                         title = "Total MHW coverage", 
                                         status = "warning", solidHeader = TRUE, collapsible = TRUE),
                                     box(plotlyOutput(ns("total_cum_days")), width = 4, 
                                         title = "Average MHW days per pixel",
                                         status = "success", solidHeader = TRUE, collapsible = TRUE))#,
                            # fluidRow(box(textOutput(outputId = ns("test_box"))))
                            ),
                    
                    
                    # App explanation ---------------------------------------------------------
                    
                    tabItem(tabName = "summary_about", 
                            fluidPage(
                              column(12,
                                     h2(tags$b("About")),
                                     p("This portion of the MHW Tracker allows one to look through individual summaries of the following three 
                                       sea surface temperature (SST) products:",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.ncdc.noaa.gov/oisst", "NOAA OISST"), ", ",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.eea.europa.eu/data-and-maps/data/external/esa-sst-cci-level-4", "ESA CCI SST"), ", and ",
                                       a(target = '_blank', rel = 'noopener noreferrer', 
                                         href = "https://www.eea.europa.eu/data-and-maps/data/external/esa-sst-cci-level-4", "CMC SST"), ". Note that the 
                                       data seen in the main map of the MHW Tracker are from NOAA OISST. The following sections provide additional information."),
                                     h2(tags$b("Annual")),
                                     p("The 'Annual' tab window contains four panels and provides three choices in the side menu. The first choice is a radio
                                       button that allows one to choose between two climatology periods when looking at the annual or total summaries.
                                       Note that the period of '1982-2011' most closely matches the WMO standard but this is not available for the CCI data as
                                       the first full year of this product was 1992. The other two options allow one to select the SST product to visualise as well as
                                       the chosen year. The top panel in this tab window shows a global map of the highest category MHW experienced per pixel in the 
                                       chosen year for the chosen product. The bottom three panels each show stacked bar plots of the occurrence of MHWs on each
                                       calendar day of the selected year. The left panel shows the daily occurrence of MHWs throughout the ocean as a % total cover.
                                       The middle panel shows the % of the ocean that had experienced it's largest MHW of the year by the date shown on the x-axis.
                                       The right panel shows the average MHW days experienced by the ocean on a given day. The three bottom panels all provide an 
                                       interactive interface where more information will become available when hovering the mouse over the graph. Clicking on
                                       any of the items in the legend will turn off/on the corresponding parts of the graph."),
                                     h2(tags$b("Total")),
                                     p("This tab window contains similar graphs to the 'Annual' tab with the exclusion of the map panel. The x-axis of each graph
                                       are in units of years, rather than days, and so they show the same MHW summaries as in the 'Annual' tab, but for annual
                                       summaries rather than daily. One may interact with these graphs in the same manner as for those in the 'Annual' tab. 
                                       The menu on the left allows one to choose between the two climatology periods and the three SST products."),
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
