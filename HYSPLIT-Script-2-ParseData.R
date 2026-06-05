## Generic 'run hysplit' script series
## 2 - Parsing outputs

# Matt Harris
# matt.harris@earthsciences.nz
# Last modified 05/06/2026

##### SETUP #####

## Load and/or install packages. 

## CORE PACKAGES: SYNTAX
pacman::p_load(dplyr,magrittr,lubridate,tibble,R.utils,tidyverse,openair,chron, here)

## TrajSpatial package, for a bunch of Matt's HYSPLIT related functions. Some will overwrite the hys