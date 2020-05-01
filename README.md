# MHWapp

The code found in this GitHub repo contains everything required to download, process, and visualise the daily global occurrence of marine heatwaves (MHWs). The MHWs are visualised through an interactive leaflet map that uses a Shiny interface. All of this is done exclusively through the R programming language. The primary data stream currently used for the detection of MHWs is the NOAA OISST product. The climatology period used for the calculations is 1982 - 2011. Detailed instructions on the use of the app may be found in the 'About' tab within the app itself. The remainder of this README file outlines the purpose of each folder and script in this repository. It should also be noted that all scripts also have a header that explains the usage of that script. One will also note that most folders contain their own unique .gitignore files. It was decided to not have a central top-level .gitignore file in order to allow for a more flexible system of ignoring version control on non-essential files.

/MHW_daily.R - This script is run daily via a cron job. It downloads and processes the global MHW results.
This scrip shows the step-by-step workflow used to create the data that are then fed to the MHW Tracker.
|
/MHW_daily_functions.R - This script houses the functions used by MHW_daily.R.
|
/MHW_daily_fixes.R - Occassionally the daily processing hits a bump. 
This script contains functions used to smooth out those issues as they arise. This occurrs less than once a month.
|
/MHW_daily_test.R - This script houses code used to poke and prod at the MHW_daily.R and MHW_daily_functions.R scripts.
|
/MHW_annual_summary.R - This script contains code used to calculate annual summaries for MHWs.
The figures created in this script are saved in /figures/ and the data files are saved in /data/annual_summary/.
|
/LICENSE.md - The MIT License for the MHW Tracker open source software.
This applies to all files found within this GitHub repository.
|
/README.md - That's this file, the one you are reading now.
|
/data/ - This folder contains data files and scripts used in addition to the primary purpose of the Tracker.
|
| -- /annual_summary/ - This folder contains different types of annual summaries of MHW results.
     | -- MHW_cat_pixel_*.Rds - These files contain annual summaries of MHW statistics per pixel of the ocean.
     | -- MHW_cat_daily_*.Rds - These files contain annual summaries of MHW statistics per day of that year.
     | -- MHW_cat_count_N_*.Rds - These files contain the count of MHWs per pixel based on the max category reached by each event. 
          The files marked with an 'N' are annual summaries over the boreal calendar season (Northern Hemisphere, January - December). 
          The files marked with an 'S' are for the autral calendar year (Southern Hemishpere, July - June).
|    
| -- /published/ - This folder contains the script used to process data from MHW related publications for use/display with the Tracker.
     | -- published.R - This is the script used to do the procesing.
     | -- Oliver_2018.Rds - Results from Oliver et al. 2018 (Nature; https://www.nature.com/articles/s41467-018-03732-9/)
| -- /extract_spatial.R - This script houses code used to extract bespoke bits of results and convert them to spatial type files.
|
/figures/ - This folder contains pre-rendered annual summaries of MHWs.
| -- MHW_cat_historic.png - This is a summary of the annual results of MHWs from 1982 to the present.
| -- MHW_cat_summary_*.png - These figures contain the visual summary og each year.
|
/metadata/ - This folder contains metadata used for the processing and visualisation of daily MHWs.
| -- final_dates.Rdata - This file contains the dates for all of the final processed OISST data available.
| -- prelim_dates.Rdata - This file contains the dates of the preliminary OISST data. 
     Usually this will be for the most recent two weeks until the data become finalised.
| -- map_base.Rdata - This is a global map that works nicely with _ggplot2_. It is not used in the Tracker.
| -- OISST_no_ice_coords.Rdata - These are the coordinates for all pixels in OISST with no ice present.
| -- OISST_ocean_coords.Rdata - These are all OISST corrdinates for pixels in the ocean.
|
/shiny/ - This folder contains the scripts that constitute the MHW Tracker.
| -- /modules/ - Due to the complexity of the MHW Tracker, it is necessary to break it up into multiple smaller scripts.
     These scripts are called modules and for each tab of the Tracker there is a corresponding -ui.R and -server.R.
     The -ui.R files show the code that generates the UI for each tab.
     The -server.R files show the code used on the server side of each tab.
| -- functions.R - This script contains a few custom made R functions used in the Shiny code
| -- global.R - This script sets up the environmnet within which the Shiny app runs
| -- server.R - This is the top level server side of the Shiny app. It calls the -server.R modules.
| -- ui.R - This is the top level UI for the Shiny app. It calls the -ui.R modules.
| -- style.css - The CSS for the Shiny app.

# Updates

* May 1st, 2020
  * The MHW Tracker is now covered under the MIT License for open source software 
  * Cleaned up the code base

* April 30th, 2020
  * Added summaries for the count of MHWs/pixel/year
  * Updated the UI so these files can be downloaded

* April 6th, 2020
  * Added colourbar for red-blue layers
  * Upated 'About' section with info for map layers
  * Removed static time series modal tab
  * Added download for time series and lolliplot figures
  * Minor bug fix to event filtering for modal panel figures

* April 4th, 2020
  * Added Oliver et al. 2018 MHW trend layers

* March 2nd, 2020
  * Changed the control panel position to accommodate the Chrome web browser
  * Added functionality to download the anomaly data
  
* March 1st, 2020
  * Added anomaly layer to leaflet map

* February 7th, 2020
  * Improvements to Summary tab UI

* February 6th, 2020
  * UI even more mobile friendly with option to hide controls
  * Users may download map data for a given range of dates

* February 5th, 2020
  * Users may now change the map background (provider tiles)
  * Improved popups
  * Added static plot and lolliplot tabs to pixel plotting modal window
  * Becoming increasingly mobile friendly

* February 3rd, 2020
  * Added an animation menu and functionality
  * Added a summary tab that allows users to choose pre-rendered annual summaries and download the figures

* September 23rd, 2019
  * When one clicks over the eastern Pacific or Mediterranean a link will now be in the popup that allows the user to go to another website that provides a regional analysis of MHWs there

* July 22nd, 2019
  * Added more error trapping

* July 17th, 2019
  * Some minor improvements

* June 6th, 2019
  * Added the climatology period used to the about section

* May 31st, 2019
  * Attempting to connect Google Analytics

* May 29th, 2019
  * Corrected typo in About section

* May 28th, 2019
  * Edited the text in the About section
  * Added a link to the press release

* May 27th, 2019
  * Changed use of `plyr::ldply` to `plyr::l_ply` as this was causing issues

* May 23rd, 2019
  * Corrected a couple of typos in the welcome message
  * Changed welcome popup to a modal window for more stability

* May 21st, 2019
  * MHW_daily.R now running at 22:00 AST via a cron job
    * Reduced verbosity of output so that logs are more readable

* May 16th, 2019
  * Added coloured category filtering buttons
  * Added info popup on start up
  * Fixed weakness in logic trapping for daily updating script

* May 15th, 2019
  * Playing with coloured button options
  * Added category colours to time series plot
  * Removed category legend from map

* March 18th, 2019
  * Updated the animation script to create chosen animations of infamous MHWs

* March 8th, 2019
  * More aggressive error trapping
  * Preventing access to pre-1985 data as these are apparently error prone

* March 6th, 2019
  * Added logic breaks to catch when the NOAA data are not up on the ERDDAP server
  * Added screen to filter out erroneously high OISST data
  * Added functionality to download preliminary data
  * Updated pipeline to create new app files based on final data
  * Preliminary data now a live part of the MHW results
  * These changes are accounted for in the `about` section

* February 14th, 2019
  * Some tweaks to the daily scripts
  * Added a "Why should I care?" section in the about panel
  * Added a break in the category screening that should help with speed
  
* February 13th, 2019
  * Two bug fixes
  * A CSS overhaul
  * Project-wide improvements to UI
  * Edited about section to reflect changes

* February 12th, 2019
  * Updates to about section
  * Added a bit more safety to daily script

* February 11th, 2019
  * Addressed heatspike issue
  * Changed data syncing with app so no git pulling is required
  * Smoothed out bugs in daily updating script
  * Minor about section edits
  
* February 7th, 2019
  * More snag list

* February 6th, 2019
  * Working through the snag list

* February 5th, 2019
  * Massive update to the About section

* January 31, 2019
  * More optimising of daily script

* January 30, 2019
  * Daily script complete and all 2018 data processed

* January 29, 2019
  * Began writing the daily script

* January 25, 2019
  * Solved the raster wrap-around issue
  * Fixed/added several UI features

* January 24, 2019
  * Added full geom_flame visual output
  * Expanded on label information

* January 10, 2019
  * Finished first draft of UI
  * Experimenting with an entirely plotly based product

* January 10, 2019
  * Finished NetCDF back-end
  * Finished native R back-end

* January 9, 2019
  * Work on shiny UI
  * Work on NetCDF database

* January 8, 2019
  * More work on efficient SQL database construction
  * Testing NetCDF lon slices as a viable option for daily OISST data

* January 4, 2019
  * Experimenting with visualising options
  * Massive overhaul to the foundation of the app

* January 3, 2019
  * Playing around with SQLite data appending workflow

* December 18, 2018
  * Played around with mapping options
  * All of 2017 data now live 

* December 17, 2018
  * Created basic SQL databases to deal with RAM issues

* December 10, 2018
  * Sorted out a basic NetCDF workflow
  * Integrated NetCDF files into app
  * Changed file pathways

* December 5, 2018
  * Scaled the data back to one month again
  * Removed rate of onset and decline and introduced cumulative intensity
  * Working on NetCDF data storage

* December 4, 2018
  * UI now allows for showing categories or event metrics per pixel

* November 30, 2018
  * Some old changes that needed pushing
  * Playing with UI
  * Added subset of event data

* October 19, 2018
  * Attempted to host all of 2017 data but it fell over
  * Created a second instance at "MHWtracker"

* October 18, 2018
  * Global proof of concept
  * Basic clickability added

* October 11, 2018
    * Playing about with popping the leaflet into shiny
    * Basic product up and running

* October 5, 2018
    * Rather moving towards leaflet
    * Made some baby steps
    
* September 25, 2018
    * First push of initial MHW calculations for South Africa region
    * An attempt was made at a basic plotly diagram
    
    