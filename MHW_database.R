# MHW_database.R
# This script houses the code used to establish the database used in the MHW Tracker
# Note that the code in this script is only meant to be run once
# 1: Setup the environment
# 2: OISST database
# 3: CCI database
# 4: CMC database
# 5: Nog een


# 1: Setup ----------------------------------------------------------------

# Libraries
library(tidyverse)
library(tidync)

# The CCI files
CCI_files <- dir("../data/CCI", full.names = T)


# 2: OISST database -------------------------------------------------------

# The NOAA OISST database for the MHW Tracker was created while the foundational
# code base was still being developed. For this reason one will not find the code
# here that downloads and establishes the file structure used in the Tracker. I
# do intend to write this process out at some point in the future.

# The code needed to download and prep the NOAA OISST data for MHW calculations 
# may be found here: https://theoceancode.netlify.app/post/dl_env_data_r/


# 3: CCI database ---------------------------------------------------------

# The code used to download the CCI data may also be found in the same post:
# https://theoceancode.netlify.app/post/dl_env_data_r/

# Another blog post is used for the calculation of MHWs in gridded data:
# https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html


