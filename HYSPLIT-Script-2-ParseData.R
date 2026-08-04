## Generic 'run hysplit' script series
## 2 - Parsing outputs

# Matt Harris
# matt.harris@earthsciences.nz
# Last modified 04/08/2026

##### SETUP #####

## Load and/or install packages. 
# Go to the 'run models' script for install lines for github packages.

## CORE PACKAGES: SYNTAX
pacman::p_load(dplyr,magrittr,lubridate,tibble,R.utils,tidyverse,openair,chron, here)

## Other packages 
pacman::p_load(splitr, openair, trajSpatial)

# PACKAGES: DISPLAY
# Optional, probably not used in this particular script, but will be later on.
pacman::p_load(raster,sf,sp,mapproj,ggplot2,mapdata,viridis)

## PACKAGES: SYNTAX
pacman::p_load(lubridate,tibble,dplyr,R.utils,chron, zoo)

## Directories
proj_dir <- paste0(here::here(),"/")
# Previous directory where collated outputs were exported
data_dir <- paste0(proj_dir,"collated_exports/")

##### Read in data #####

## Section notes
# Following collation the outputs of a model run are fairly easy to load in with read.csv

# Load in the collated csv
PHtst_1500m_24hr <- read.csv(file = paste0(data_dir,"PHtst-2015-2026-06-05_24hr1TPD1500m.csv")) 

##### Create data subsets #####

## Section notes
# It can be useful to subset the main dataset. E.g., shorten run duration or pull out certain seasons.
# For this I use dplyr, with the magrittr pipe %>%

# Make new variables to track starting day, month, year
PHtst_1500m_24hr <- PHtst_1500m_24hr %>%
  mutate(start_month = month(as.Date(date.start)),
         start_year = year(as.Date(date.start)))

# Reduce duration to a certain number of hours (12 in this case)
PHtst_1500m_12hr <- PHtst_1500m_24hr %>% 
  filter(abs(hour.inc) <= 12)

# You can of course filter for any other part of the variables. E.g., year ranges.
# These are redundant in the case of the example data as the data only covers 2015.
PHtst_1500m_24hr <- PHtst_1500m_24hr %>% 
  filter(year > 2014 & year < 2016)

# Select some months for e.g., seasonal parsing for approx austral seasonal divs
summer_months <- c(11,12,1,2,3)
winter_months <-c(4,5,6,7,8)

# Parse in bulk
# This is a bulk parse; no attention paid to starting month.
PHtst_1500m_24hr_summer <- PHtst_1500m_24hr[PHtst_1500m_24hr$month %in% (summer_months),]
PHtst_1500m_24hr_winter <- PHtst_1500m_24hr[PHtst_1500m_24hr$month %in% (winter_months),]
# Or, parse based on starting month
PHtst_1500m_24hr_summer <- PHtst_1500m_24hr[PHtst_1500m_24hr$start_month %in% (summer_months),]
PHtst_1500m_24hr_winter <- PHtst_1500m_24hr[PHtst_1500m_24hr$start_month %in% (winter_months),]

# That's all there is to it really! 

