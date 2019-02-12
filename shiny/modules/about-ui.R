aboutUI <- function(id, label = 'About') {
  ns <- NS(id)
  
  fluidRow(
    column(1,
           HTML("")),
    column(10,
           h4("Welcome!"),
           br(),
           p("You have arrived at the Marine Heatwave (MHW) Tracker. This web application has been designed to show the 
             most up-to-date information available on the occurrence of MHWs around the world, which is currently a 
             two-week delay. The MHW tracker also hosts all of the historic MHW records for the entire planet going 
             back to January 1st, 1982."),
           br(),
           h4("How do I use this?"),
           br(),
           p("This site works similarly to any Google map one may have used in day-to-day life. Click and drag to move 
             around the world. Use the middle mouse button or click the plus/minus buttons on the left side of the screen 
             to zoom in or out. The date menu in the bottom left corner allows the user to chose any date from the near 
             present, back to January 1st 1982 to show the map of MHWs on that day. One can also choose to screen out the 
             different categories of MHWs (see below) by clicking on the check boxes next to each category underneath the 
             date menu. The legend can also be disabled by unchecking the legend option underneath the categories check-boxes.
             If one of the pixels on the map catches your attention, click once on it to create a popup that will show the 
             longitude, latitude, and category of the MHW there. To get more information click on the button in the lower left
             of the screen labelled 'More information' to bring up a new window that contains more detailed information. 
             This new pop-up window currently contains two tabs: plot and table. One may also download the climatology and
             threshold data for the chosen pixel by using the download buttons at the bottom of the window. To download the
             temperature data please go to the", a(href = "https://www.ncdc.noaa.gov/oisst", "NOAA website"),".
             The plot tab allows the user to mouse-over the time series information to get immediate feedback on what the 
             temperature, climatology, and threshold were on any given day. One can also select any desired range of 
             dates to look at from the date selectors in the bottom left of the panel. One may also download
             The table tab shows a table of all of the MHWs that have ever occurred at the chosen longitude/latitude. 
             This information is shown as a simple table and may be downloaded as a .csv file with a button
             in the bottom left corner of the of the window."),
           br(),
           h4("What is a Marine Heatwave (MHW)?"),
           br(),
           p("A MHW is generally defined as when the temperature in a given location is in the top 10% of temperatures 
             ever recorded during that time of year for at least 5 straight days. For example, if the coastal waters off
             Durban in South Africa are roughly 22°C on any given April 1st, then if temperatures in excess of perhaps 28°C 
             are recorded there in 2019 over March 28th to April 10th, this could be flagged as a MHW. This is a definition
             for MHWs first put forward by Hobday et al. 2016. An interested reader may find the paper ", 
             a(href = "https://www.sciencedirect.com/science/article/pii/S0079661116000057", "here"),"."),
           br(),
           h4("What do these colours mean?"),
           br(),
           p("One may see in the map panel of the MHW tracker that there is a particular colour palette being used. Each of 
             these four colours corresponds to increasing categories of MHWs. The first category, severe, is somewhat common
             and no category one MHWs have yet been recorded as causing lasting ecological or financial damage. The second
             category of MHWs, strong, are increasing the most rapidly of the four categories and are becoming as 
             common as category one MHWs were when record keeping began in the 80's. Luckily, no category two MHWs 
             have yet been documented as being destructive either. Category three MHWs, severe, are less common but can be 
             devastating when they persist for multiple months. The last category, extreme, is thankfully a rare occurrence.
             It is now known that less than three months of a persistent category four MHW can destroy hundreds of kilometres 
             of ocean ecosystems."),
           br(),
           h4("Where do these data come from?"),
           br(),
           p("The global satellite product used in the MHW Tracker are the daily Optimally Interpolated Sea Surface Temperature 
             (OISST) data that one may download from the National Oceanic and Atmospheric Administration (NOAA). These data are 
             on an even 1/4 degree grid over the surface of the planet. The daily values go back as far as September 1st, 1981.
             More information about these data may be found ", a(href = "https://www.ncdc.noaa.gov/oisst", "here"),"."),
           p("The MHWs were calculated using the Hobday et al. 2016 definition briefly outlined above. The results on the MHW
             Tracker were calculated with the R version of this algorithm and extensive documentation on this may be found ",
             a(href = "https://robwschlegel.github.io/heatwaveR/index.html", "here"),". This algorithm is also available for", 
             a(href = "https://github.com/ecjoliver/marineHeatWaves", "python "), " as well as ",
             a(href = "https://github.com/ZijieZhaoMMHW/m_mhw1.0", "MATLAB"),"."),
           br(),
           h4("Who made this?"),
           br(),
           p("The MHW Tracker is a product of the work of the", 
             a(href = "http://www.marineheatwaves.org/", "Marine Heatwaves International Working Group"),". Therefore,
             this work has been directly and indirectly supported by several governmental, academic, and private 
             organisations/funding bodies. The full list with links to further information is provided below in
             alphabetical order:"),
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
                                    "Ocean Frontiers Insitute")),
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
           h4("Snags"),
           br(),
           p("Unfortunately this app is not currently optimised for use on mobile devices (e.g. cell phones or tablets).
             It will therefore appear rather clumsy if being viewed on a small screen. This has been noted and an item
             has been opened to address this issue."),
           br(),
           h4("Citation"),
           br(),
           p("The marine heatwave data displayed in this app were calculated witht the `heatwaveR` R package. To cite 
             `heatwaveR` in publications use:"),
           p("Robert W. Schlegel and Albertus J. Smit (2018). heatwaveR: A central algorithm for the detection of 
             heatwaves and cold-spells. Journal of Open Source Software, 3(27), 821, https://doi.org/10.21105/joss.00821"),
           p("A BibTeX entry for LaTeX users is:"),
           p("@Article{,
             title = {{heatwaveR}: A central algorithm for the detection of heatwaves and cold-spells},
             author = {Robert W. Schlegel and Albertus J. Smit},
             journal = {Journal of Open Source Software},
             year = {2018},
             volume = {3},
             number = {27},
             pages = {821},
             doi = {10.21105/joss.00821},
             }"),
           p("To cite the app itself one may use:"),
           p("Robert W. Schlegel (2018). mhwtracker: Global daily updating on the occurrence of marine heatwaves. 
             https://www.marineheatwavetracker.com"),
           br(),
           hr(),
           h6("The MHW tracker was developed by ", a(href = " https://theoceancode.netlify.com/", "Robert Schlegel"))
          ),
    column(1,
           HTML("")))
}