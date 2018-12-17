# The purpose of this script is to provide the space in which the 
# database used by the app is created


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dbplyr)
library(DBI)
library(RSQLite)
# library(fasttime)


# Data --------------------------------------------------------------------

load("data/MHW_cat_clim_sub.RData")
load("data/MHW_event_sub.RData")
# load("data/MHW_clim_sub.RData") # This is massive...


# Database ----------------------------------------------------------------

# Initiales database
MHW_db <- src_sqlite("data/MHW_db.sqlite", create = TRUE)
# MHW_db

# Fill it full of data
copy_to(MHW_db, MHW_cat_clim_sub, temporary = FALSE)
copy_to(MHW_db, MHW_event_sub, temporary = FALSE)
# copy_to(MHW_db, MHW_clim_sub, temporary = FALSE) # Not convinced this should be included...
# MHW_db


# # Test connection -------------------------------------------------------

MHW_test <- DBI::dbConnect(RSQLite::SQLite(), "data/MHW_db.sqlite")
src_dbi(MHW_test)

date_filter <- as.integer(as.Date("2017-06-17"))

cat_clim <- tbl(MHW_test, "MHW_cat_clim_sub")
cat_clim_1 <- cat_clim %>%
  filter(t == date_filter) %>% 
  collect() %>% 
  mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                "III Severe", "IV Extreme")))
head(cat_clim_1)

ggplot(cat_clim_1, aes(x = lon, y = lat, fill = category)) +
  geom_raster()

