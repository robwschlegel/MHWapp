# MHWapp/shiny/ui.R

ui <- page_sidebar(
  

  # Main info ---------------------------------------------------------------

  title = "Marine Heatwave Tracker",
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  fillable = TRUE,
  # h1("Marine Heatwave Tracker"),
  # header = list(cicerone::use_cicerone()), # Start guided tour
  

  # Summary boxes -----------------------------------------------------------

  # Summary boxes
  layout_column_wrap(
    width = "250px",
    fill = FALSE,
    value_box(title = "Total cover", value = "...", showcase = bs_icon("percent"),
              style = paste0("background-color: ",base_colours[1],"!important;")),
    value_box(title = "I Moderate", value = "...", showcase = bs_icon("percent"),
              style = paste0("background-color: ",MHW_colours[1],"!important;")),
    value_box(title = "II Strong", value = "...", showcase = bs_icon("percent"),
              style = paste0("background-color: ",MHW_colours[2],"!important;")),
    value_box(title = "III Severe", value = "...", showcase = bs_icon("percent"),
              style = paste0("background-color: ",MHW_colours[3],"!important;")),
    value_box(title = "IV Extreme", value = "...", showcase = bs_icon("percent"),
              style = paste0("background-color: ",MHW_colours[4],"!important;"))
  ),


  # Map card ----------------------------------------------------------------

  card(full_screen = TRUE, fill = TRUE,
       layout_sidebar(
         fillable = TRUE,
         class = "p-0",
         sidebar = sidebar(
           title = "Map controls",
           bg = "grey",
           width = "15%",
           class = "fw-bold font-monospace", 
           open = "closed",
           accordion(
             multiple = TRUE, 
             open = "closed",
             # Date selector
             accordion_panel(
               "Date", icon = bsicons::bs_icon("menu-app"),
               # uiOutput(outputId = "date_reactive")
               dateInput(inputId = "date",
                         label = NULL, width = '100%',
                         # value = date_menu_choice,
                         value = max(current_dates),
                         min = "1982-01-01",
                         max = max(current_dates))
               ),
             # Map layer selector
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
             )
           )
         ),
         # TODO: Change colour based on selected later/colour palette
         # leafletOutput("leaf_map")
         withSpinner(leafletOutput("leaf_map"), type = 6, color = "#b0b7be")
       )
  ),


  # Time series card --------------------------------------------------------

  # card(full_screen = TRUE,
  # nav_menu(full_screen = TRUE, selected = "ts",
  navset_card_tab(full_screen = TRUE,
                  # navset_card_pill(
                  # navset_tab(
                  # full_screen = TRUE,
                  # title = "Selected pixel",
                  # layout_sidebar(
                  sidebar = sidebar(
                    title = "Controls",
                    # bg = "grey",
                    # width = "15%",
                    # class = "fw-bold font-monospace",
                    # open = "open",
                    # accordion(
                    #   multiple = FALSE,
                    #   open = "open",
                    #   # Date selector
                    #   accordion_panel(
                    #     "Date range", icon = bsicons::bs_icon("menu-app"),
                    dateRangeInput(
                      label = NULL, inputId = "from_to",
                      start = max(current_dates)-365, end = max(current_dates),
                      min = "1982-01-01", max = max(current_dates))
                  ),
                  #   )
                  # ),
                  # withSpinner(plotlyOutput("tsPlotly"), type = 6, color = "#b0b7be")
                  nav_panel(title = "Time series", value = "ts", withSpinner(plotlyOutput("tsPlotly"), type = 6, color = "#b0b7be")),
                  nav_panel(title = "Lolliplot", value = "lolli", withSpinner(plotlyOutput("lolliPlotly"), type = 6, color = "#b0b7be")),
                  nav_panel(title = "Table", value = "table", withSpinner(DT::dataTableOutput("tsTable"),type = 6, color = "#b0b7be"))
                  # )
  ),
  

  # About sidebar -----------------------------------------------------------

  # About info served as a sidebar
  sidebar = sidebar(open = "closed", width = "75%", position = "right",
                    h2(tags$b("Welcome!")),
                    br(),
                    p("You have arrived at the Marine Heatwave (MHW) Tracker. This web application shows the occurrence of MHWs 
             around the world in near-real-time (roughly a one-two day delay). The Tracker also shows daily the 
             historic records for the whole planet going back to January 1st, 1982. There are several other data layers
             offered in the Tracker. Please see the 'How do I use this?' section below for details."),
                    br(),
                    h2(tags$b("What is a Marine Heatwave (MHW)?")),
                    br(),
                    p("A MHW is generally defined as when the temperature in a given location is in the top 10% of temperatures 
             ever recorded during that time of year for at least 5 straight days. For example, if the coastal waters off
             Durban in South Africa are roughly 22°C on any given April 1st, if temperatures in excess of perhaps 28°C 
             are recorded there in 2019 over March 28th to April 10th, this could be flagged as a MHW. This is a definition
             for MHWs first put forward by ", 
                      a(target = '_blank', rel = 'noopener noreferrer', 
                        href = "https://www.sciencedirect.com/science/article/pii/S0079661116000057", "Hobday et al. 2016"),
                      ". For a more detailed explanation with visuals please follow this ", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "http://www.marineheatwaves.org/all-about-mhws.html", "link"), "."),
                    br(),
                    h2(tags$b("What do these colours mean?")),
                    br(),
                    p("In the map panel of the Marine Heatwave Tracker we can see that there is a particular colour palette being used. Each of 
             these four colours corresponds to increasing categories of MHWs as first proposed in ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.jstor.org/stable/26542662", "Hobday et al. 2018"), 
                      ". The first category, 'I Moderate', is somewhat common
             and no category one MHWs have yet been recorded as causing lasting ecological or financial damage. The second
             category of MHWs, 'II Strong', are increasing in occurrence the most rapidly of the four categories and are on course 
             to become as common as category one MHWs were when record keeping began in the 80's. Fortunately, category two MHWs 
             have rarely been documented to cause lasting damage. The third category, 'III Severe', are less common but can be 
             devastating when they persist for more than a month. The last category, 'IV Extreme', is thankfully a rare occurrence.
             It is now known that less than three months of a persistent category four MHW can wipe out entire coastal ecosystems."),
                    br(),
                    h2(tags$b("Why should I care?")),
                    br(),
                    p("If you've found your way to this website then you are likely interested in the effects we are having on
             the worlds oceans and probably have your own personal reasons to care about their health. There are
             many anthropogenic (man-made) threats to the health of the oceans, which include but are not limited to:
             over-fishing, chemical run-off from land, and climate change in the form of extreme warm water events known as
             MHWs. All of these different threats may impact the ocean in different ways, but one of the 
             main concerns is that we are changing the oceans so much that we will not be able to repair them ourselves. 
             MHWs are not new to our oceans, but our ability to quantify them and put them up on a website like this is. 
             Thanks to this tool we can now see for ourselves in near-real-time where in the world extreme temperatures may 
             be threatening the parts of the oceans where we work, live, and play. It is our hope that this website can be used
             around the world to help anyone that is interested to see if the changes they have noticed in their ocean are 
             due to a MHW or not."),
                    br(),
                    h2(tags$b("How do I use this?")),
                    br(),
                    p("This site works similarly to the Google maps we use in day-to-day life. Click and drag to move 
             around the world. Use the mouse scroll to zoom in or out. The 'Date' box in the 'Controls' panel tells us which 
             date is being shown on the map. Clicking in this box we can choose any date from the near present back to January 1st, 1982. 
             We can also choose to filter out the different categories of MHWs by clicking on the corresponding category 
             buttons near the bottom of the vontrol panel. If one of the pixels on the map catches your eye, clicking on it creates a popup that shows the 
             longitude, latitude, and category of the MHW there. To get more information click on the 'Plot pixel' button
             underneath the 'Category' buttons. This will bring up a new window with more detailed information. 
             This window contains three tabs: 'Time series', 'Lolliplot', and 'Table'."),
                    p("We can hover over the figure in the 'Time series' tab to get immediate feedback on what the temperature, climatology, and 
             threshold were on any given day. At the peak date of every MHW is a small red tick mark on the bottom of the figure. If we 
             hover over this tick mark it will show us summary values for that MHW. We can also select any range of 
             dates we want to look at from the date selection boxes in the bottom left of the panel. The 'Lolliplot' tab shows the MHWs
             with no adtional information, allowing us to focus on the events themselves. The 'Table' tab shows a spreadsheet
             of all of the MHWs that have ever occurred at the chosen longitude/latitude. We can sort the events by clicking on the 
             different columns. This event information may be downloaded by clicking the button in the bottom left corner of the panel.
             We can download the figures, climatology & threshold data, and the MHW data for the chosen pixel with the 
             download buttons at the bottom of the window. To download the temperature data please go to the", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "NOAA website"),"."),
                    p("Going back to the main map we see that there are also 'Map layer', 'Map data', 'Background', and 'Coordinates' buttons.
             Clicking on the 'Map layer' button will bring up several options for which type of data to visualise. Please see the 
             'What are these different map layers?' section below for more detail. If one clicks on the
             'Map data' button it will bring up a small interface with which to download data for the chosen map layer. One may chose a
             range of dates to download simultaneously. The 'Summary' data may be downloaded as either the max event recorded at each pixel for
             the chosen year, or as the count of each category of MHWs at each pixel for the year. The count data may also be downloaded for the
             boreal (January - December) or austral (July - June) portions of the year. The 'Background' button provides one with a few different
             backgrounds to chose from for the map. The 'Coordinates' button opens up a menu that allows the user to input the exact 
             longitude/latitude values desire to jump to a specific point on the map."),
                    br(),
                    h2(tags$b("Where do these data come from?")),
                    br(),
                    p("The global satellite product used in the Marine Heatwave Tracker is the daily Optimally Interpolated Sea Surface Temperature 
             (OISST) data that may be downloaded from the National Oceanic and Atmospheric Administration (NOAA). Specifically these are 
             the AVHRR-ony v2.0 data from 1982 to 2015 and the AVHRR-only v2.1 data from 2016 to present day. These data are 
             on an even 1/4 degree grid over the surface of the planet. The daily values go back as far as September 1st, 1981,
             but the MHW Tracker only hosts results starting on January 1st, 1982 as this is the first full year of data.
             More information about these data may be found ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.ncdc.noaa.gov/oisst", "here"),"."),
                    p("Please note that the data are released in near-real-time, and then go through a second layer of quality control that
             takes roughly two weeks. Therefore any MHW results shown in this app within two weeks of the current date may be 
             subject to minor changes. All MHW results older than two weeks may be taken as final. In practice the difference between
             the preliminary results and the final results are negligible."),
                    p("A tutorial for how to download these data in R may be found ", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html", "here"),
                      ", and a tutorial on how to calculate MHWs from these data in R may be found ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html", "here"),"."),
                    p("The MHW results on the Marine Heatwave Tracker were calculated with the R version of the Hobday et al. 2016 definition 
             briefly outlined above. Extensive documentation on the R code may be found ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://robwschlegel.github.io/heatwaveR/index.html", "here"),". This algorithm is also available for", 
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://github.com/ecjoliver/marineHeatWaves", "python ")," and ",
                      a(target = '_blank', rel = 'noopener noreferrer',
                        href = "https://github.com/ZijieZhaoMMHW/m_mhw1.0", "MATLAB"),
                      ". The climatology period used for calculating the MHWs is 1982-01-01 to 2011-12-31."),
                    br(),
                    h2(tags$b("What are these different map layers?")),
                    br(),
                    p("The default map layer, 'Category', shows the categories of the MHW occurring at each pixel on the chosen day. 
             The 'Summary' layer shows the highest category MHW that occurred at each pixel over the course of the chosen year. 
             The 'Anomaly' layer shows the temperature anomalies for the chosen day against the daily climatology from 
             1982-01-01 to 2011-12-31. The various 'Trend' layers show the annual trend in the change of the MHW metric referred to. 
             For example, a value of 0.2 at a pixel when looking at the 'Trend: Count' layer would mean that for the last 30+ years 
             MHWs at that pixel have been increasing by a count of 0.2 every year. Or in other words, every five years (1/0.2) there is 
             one additional MHW occurring in that pixel per year. For another example let’s look at the 'Trend: Duration' layer. 
             If we see a pixel here with a value of one, this means that, on average, MHWs in this pixel are becoming one day longer each year. 
             Conversely, a value of negative one, as seen throughout the eastern Pacific, would mean that MHWs are shortening by one day each year. 
             The data for these trends are the underlying data for Figure 1 in ",
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://www.nature.com/articles/s41467-018-03732-9/", "Oliver et al., 2018"),
                      " and a more detailed description for them may be found in that publication."),
                    br(),
                    h2(tags$b("Who made this?")),
                    br(),
                    p("The Marine Heatwave Tracker was developed by ", 
                      a(target = '_blank', rel = 'noopener noreferrer', href = "https://theoceancode.netlify.com/", "Robert Schlegel"),
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
                    p("This app is visually heavy and so appears clumsy on a screen smaller than a laptop (e.g. cell phones or tablets).
             It has been optimised for use on mobile devices as much as is possible."),
                    p("Occasionally the marine heatwave polygons in the time series plots do not render correctly. 
             Changing the date selection will allow the figure to re-render correctly."),
                    p("Very rarely when the app starts up no MHW pixels will be displayed. Refreshing the website will fix this."),
                    p("The map panel of the app may not render on a machine running on an Apple operating System. This is likely due to a 
             Javascript extension issue and is not caused by the MHW Tracker."),
                    p("It has also been noted that the map panel may not show up on Windows operating systems either. This is usually remedied
             by accessing the Tracker in either Internet Explorer or Microsoft Edge."),
                    p("If the MHW pixels appear, but the map remains grey, one may manually change the map selection with the 'Background' 
             button in the control panel to the left of the screen."),
                    p("If problems with viewing the map panel persist, one is encouraged to update the javascript plugins on one's browser of choice."),
                    br(),
                    p("To report any bugs or to provide any other feedback on the app please contact the developer at:
             robert.schlegel@imev-mer.fr"),
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
                    p("Any use of the NOAA OISST data should be accompanied by the following reference:"),
                    p("Reynolds, R. W., Smith, T. M., Liu, C., Chelton, D. B., Casey, K. S., and Schlax, M. G. (2007). 
             Daily high-resolution-blended analyses for sea surface temperature. J. Clim. 20, 5473–5496. doi: 10.1175/2007JCLI1824.1"),
                    p("The use of v2.0 data should additionally be accompanied with this reference:"),
                    p("Banzon, V., Smith, T. M., Chin, T. M., Liu, C., and Hankins, W. (2016). 
             A long-term record of blended satellite and in situ sea-surface temperature for climate monitoring, 
             modeling and environmental studies. Earth Syst. Sci. Data 8, 165–176. doi: 10.5194/essd-8-165-2016"),
                    p("The use of v2.1 data should refer to this publication:"),
                    p("Banzon, V., Smith, T. M., Steele, M., Huang, B., & Zhang, H. M. (2020). 
             Improved Estimation of Proxy Sea Surface Temperature in the Arctic. 
             Journal of Atmospheric and Oceanic Technology, 37(2), 341-349. doi: 10.1175/JTECH-D-19-0177.1"),
                    br(),
                    p("The marine heatwave data displayed in this app were calculated with the heatwaveR R package. To cite 
             heatwaveR in publications please use:"),
                    p("Schlegel, R. W., and Smit, A. J. (2018). heatwaveR: a central algorithm for the detection of heatwaves 
             and cold-spells. J. Open Sour. Softw. 3:821. doi: 10.21105/joss.00821"),
                    br(),
                    p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
                    p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C. J., et al. (2016). 
             A hierarchical approach to defining marine heatwaves. Progr. Oceanogr. 141, 227–238. doi: 10.1016/j.pocean.2015.12.014"),
                    p("Hobday, A. J., Oliver, E. C. J., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., et al. (2018). 
             Categorizing and naming marine heatwaves. Oceanography 31, 162–173. doi: 10.5670/oceanog.2018.5205"),
                    br(),
                    p("The various trend map layers are visualisations of the base results from:"),
                    p("Oliver, E. C., Donat, M. G., Burrows, M. T., Moore, P. J., Smale, D. A., Alexander, L. V., ... & Holbrook, N. J. (2018). 
             Longer and more frequent marine heatwaves over the past century. Nature communications, 9(1), 1-12."),
                    br(),
                    h5(paste0("Instance: ",Sys.getenv("R_SHNYSRVINST"))),
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

