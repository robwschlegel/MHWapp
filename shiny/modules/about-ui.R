aboutUI <- function(id, label = 'About') {
  ns <- NS(id)
  
  fluidRow(
    column(1,
           HTML("")),
    column(10,
           h2("Welcome!"),
           br(),
           p("You have arrived at the Marine Heatwave (MHW) Tracker. This web application has been designed to show the 
             most up-to-date information available on the occurrence of MHWs around the world, which is currently a 
             two-week delay. The Marine Heatwave Tracker also hosts all of the historic MHW records for the entire planet going 
             back to January 1st, 1982."),
           br(),
           h2("What is a Marine Heatwave (MHW)?"),
           br(),
           p("A MHW is generally defined as when the temperature in a given location is in the top 10% of temperatures 
             ever recorded during that time of year for at least 5 straight days. For example, if the coastal waters off
             Durban in South Africa are roughly 22°C on any given April 1st, then if temperatures in excess of perhaps 28°C 
             are recorded there in 2019 over March 28th to April 10th, this could be flagged as a MHW. This is a definition
             for MHWs first put forward by ", 
             a(href = "https://www.sciencedirect.com/science/article/pii/S0079661116000057", "Hobday et al. 2016"),
             ". For a more detailed explanation with visuals please follow this ", 
             a(href = "http://www.marineheatwaves.org/all-about-mhws.html", "link"), "."),
           br(),
           h2("Why should I care?"),
           br(),
           p("If you've found your way to this website then you are likely interested in the effects we are having on
             the worlds oceans and probably have your own personal reasons to care about the health of the oceans. There are
             many anthropogenic (man-made) threats to the health of the oceans, which include but are not limited to:
             over-fishing, chemical run-off from land, and climate change in the form of extreme warm water events known as
             Marine heatwaves (MHWs). All of these different threats may impact the ocean in different ways, but one of the 
             main concerns is that we are changing the oceans so much that we will not be able to repair them ourselves. 
             MHWs are not new to our oceans, but our ability to quantify them and put them up on a website like this is. 
             Thanks to this tool we can now see for ourselves in near real time where in the world extreme temperatures may 
             be threatening the parts of the ocean where we work, live, and play. It is our hope that this website can be used
             around the world to help anyone that is interested to see if the changes they have noticed in their ocean are 
             due to a MHWs or not."),
           br(),
           h2("How do I use this?"),
           br(),
           p("This site works similarly to the Google maps we use in day-to-day life. Click and drag to move 
             around the world. Use the mouse scroll wheel or click the plus/minus buttons on the left side of the screen 
             to zoom in or out. The date box in the top right corner tells us which day is being shown on the map. Clicking 
             in this box we can choose any date from the near present back to January 1st 1982. We can also choose to screen out the 
             different categories of MHWs (see below) by clicking on the check boxes next to each category underneath the 
             'Time sereis' button. The legend can also be turned off by unchecking the legend option underneath the categories check-boxes.
             If one of the pixels on the map catches your eye, click on it to popup a little window that shows the 
             longitude, latitude, and category of the MHW there. To get more information click on the 'Time series' button in the top right
             of the screen, underneath the date box. This will bring up a new window that contains more detailed information. 
             This window currently contains two tabs: 'plot' and 'table'. We can download the climatology and
             threshold data for the chosen pixel when in the plot tab by clicking the download button at the bottom of the window. 
             To download the temperature data please go to the", a(href = "https://www.ncdc.noaa.gov/oisst", "NOAA website"),".
             The figure in the plot tab allows us to mouse-over the time series information to get immediate feedback on what the 
             temperature, climatology, and threshold were on any given day. We can also select any desired range of 
             dates to look at from the date selectors in the bottom left of the panel. The table tab shows a table of all of 
             the MHWs that have ever occurred at the chosen longitude/latitude. This information is shown as a simple table and 
             may be downloaded with by clicking the button in the bottom left corner of the of the window."),
           br(),
           h2("What do these colours mean?"),
           br(),
           p("In the map panel of the Marine Heatwave Tracker we can see that there is a particular colour palette being used. Each of 
             these four colours corresponds to increasing categories of MHWs as first proposed in ", 
             a(href = "https://www.jstor.org/stable/26542662", "Hobday et al. 2018"), ". The first category, 'I Moderate', is somewhat common
             and no category one MHWs have yet been recorded as causing lasting ecological or financial damage. The second
             category of MHWs, 'II Strong', are increasing the most rapidly of the four categories and are on course to become as 
             common as category one MHWs were when record keeping began in the 80's. Luckily, no category two MHWs 
             have yet been documented as being destructive either. Category three MHWs, 'III Severe', are less common but can be 
             devastating when they persist for multiple months. The last category, 'IV Extreme', is thankfully a rare occurrence.
             It is now known that less than three months of a persistent category four MHW can destroy coastal ecosystems."),
           br(),
           h2("Where do these data come from?"),
           br(),
           p("The global satellite product used in the Marine Heatwave Tracker are the daily Optimally Interpolated Sea Surface Temperature 
             (OISST) data that we can download from the National Oceanic and Atmospheric Administration (NOAA). These data are 
             on an even 1/4 degree grid over the surface of the planet. The daily values go back as far as September 1st, 1981.
             More information about these data may be found ", a(href = "https://www.ncdc.noaa.gov/oisst", "here"),"."),
           p("A tutorial for how to download these data in R may be found ", 
             a(href = "https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html", "here"),
             ", and a tutorial on how to calculate marine heatwaves from these data in R may be found ",
             a(href = "https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html", "here"),"."),
           p("The marine heatwaves were calculated using the Hobday et al. 2016 definition briefly outlined above. The results on the MHW
             Tracker were calculated with the R version of this algorithm and extensive documentation on this may be found ",
             a(href = "https://robwschlegel.github.io/heatwaveR/index.html", "here"),". This algorithm is also available for", 
             a(href = "https://github.com/ecjoliver/marineHeatWaves", "python ")," and ",
             a(href = "https://github.com/ZijieZhaoMMHW/m_mhw1.0", "MATLAB"),"."),
           br(),
           h2("Who made this?"),
           br(),
           p("The Marine Heatwave Tracker was developed by ", a(href = "https://theoceancode.netlify.com/", "Robert Schlegel"),
             " and is an outcome of the ", a(href = "http://www.marineheatwaves.org/", "Marine Heatwaves International Working Group"),
             ". Therefore, this work has been directly and indirectly supported by several governmental, academic, and private 
             organisations/funding bodies. The full list with links to further information is provided below in alphabetical order:"),
           column(1), p("AU - ", a(href = "https://www.aber.ac.uk/en/", 
                                     "Aberystwyth University")),
           column(1), p("AIMS - ", a(href = "https://www.aims.gov.au/", 
                                       "Australian Institute of Marine Science")),
           column(1), p("CLEX - ", a(href = "https://climateextremes.org.au/", 
                                     "ARC Centre of Excellence for Climate Extremes")),
           column(1), p("CSIRO - ", a(href = "https://www.csiro.au/", 
                                      "Commonwealth Scientific and Industrial Research Organisation")),
           column(1), p("DAL - ", a(href = "https://www.dal.ca/", 
                                    "Dalhousie University")),
           column(1), p("MBA - ", a(href = "https://www.mba.ac.uk/", 
                                     "Marine Biological Association")),
           column(1), p("NESP - ", a(href = "http://nespclimate.com.au/", 
                                     "National Environmental Science Programme")),
           column(1), p("OFI - ", a(href = "https://www.oceanfrontierinstitute.com/", 
                                    "Ocean Frontier Insitute")),
           column(1), p("SAMS - ", a(href = "https://www.sams.ac.uk/", 
                                     "The Sottish Association for Marine Science")),
           column(1), p("UC - ", a(href = "https://www.canterbury.ac.nz/", 
                                     "University of Canterbury")),
           column(1), p("UNSW - ", a(href = "https://www.unsw.edu.au/", 
                                     "University of New South Wales")),
           column(1), p("UTAS - ", a(href = "http://www.utas.edu.au/", 
                                     "University of Tasmania")),
           column(1), p("UW - ", a(href = "http://www.washington.edu/", 
                                    "University of Washington")),
           column(1), p("UWA - ", a(href = "https://www.uwa.edu.au/", 
                                    "The University of Western Australia")),
           column(1), p("UWC - ", a(href = "www.uwc.ac.za", 
                                    "University of the Western Cape")),
           br(),
           h2("Bugs?"),
           br(),
           p("Unfortunately this app is not currently optimised for use on mobile devices (e.g. cell phones or tablets).
             It will therefore appear rather clumsy if being viewed on a screen smaller than a laptop. There is currently 
             not a plan to address this issue."),
           p("Occasionally the marine heatwave polygons do not render correctly. Changing the date selection will cause the 
             figure to re-render correctly."),
           br(),
           p("To report any bugs or to provide any other feedback on the app please contact the developer at:
             robert.schlegel@dal.ca"),
           br(),
           h2("How do I cite this app?"),
           br(),
           p("To cite the app itself please use:"),
           p("Schlegel, R. W. (2018). Marine Heatwave Tracker: The app to see when and where marine heatwaves are happening around the world. 
             http://www.marineheatwaves.org/tracker"),
           br(),
           h2("References"),
           br(),
           p("The marine heatwave data displayed in this app were calculated with the `heatwaveR` R package. To cite 
             `heatwaveR` in publications use:"),
           p("Schlegel, R. W., & Smit, A. J. (2018). heatwaveR: A central algorithm for the detection of heatwaves and cold-spells. 
             The Journal of Open Source Software, 3, 821."),
           br(),
           p("The definition and categorisation of marine heatwaves may be found in the following two papers:"),
           p("Hobday, A. J., Alexander, L. V., Perkins, S. E., Smale, D. A., Straub, S. C., Oliver, E. C., ... & Holbrook, N. J. (2016). 
             A hierarchical approach to defining marine heatwaves. Progress in Oceanography, 141, 227-238."),
           p("Hobday, A. J., Oliver, E. C., Gupta, A. S., Benthuysen, J. A., Burrows, M. T., Donat, M. G., ... & Smale, D. A. (2018). 
             Categorizing and naming marine heatwaves. Oceanography, 31(2), 162-173."),
           br(),
           br()
    ),
    column(1,
           HTML(""))
  )
}