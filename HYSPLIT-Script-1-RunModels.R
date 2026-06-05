## Generic 'run hysplit' script series
## 1 - Running the model contiguously

# Matt Harris
# matt.harris@earthsciences.nz
# Last modified 05/06/2026

# This script can be run in separate R program instances to effectively run the model in parallel. 
# A standard Windows PC will use a distinct CPU core for each invocation of the hysplit program
#   by R, which happens when you open a whole new R session and run the model. 
# Just pick different time periods for each session of R.
# You must specify a different working directory in the hysplit directory for 
#   each instance (e.g., "C:/hysplit/working/", "C:/hysplit/working2/", etc.)

##### Prerequisites #####

## 1: HYSPLIT
# First, you'll need a working installation of HYSPLIT running locally on your PC. 
# This requires quite a few bits of software.
# Follow the instructions on https://www.ready.noaa.gov/documents/Tutorial/html/install_win.html.
# No need to register, just install an unregistered copy.
# Where possible, use defaults (install locations, etc.).
# Don't try to use splitr until you have HYSPLIT working in its own GUI. 
# In general, the install has the following steps:
# [ ] - Install tcl/Tk UI 
# [ ] - Install Ghostscript ps converter 
# [ ] - Install GSview (if using; I recommend just using ghostscript and making the directory changes covered in the tutorial)
# [ ] - Install ImageMagick 
# [ ] - Install HYSPLIT (public, unregistered is fine)
# [ ] - Check HYSPLIT install: launch GUI and run the example. If it fails, see step 5 in the NOAA guide and make the 
#        necessary changes. 

## 2: Some reanalysis data
# Information on how to download bits of reanalysis data can be found here: https://www.ready.noaa.gov/archives.php
# For NCEP/NCAR reanalysis data, the most straightforward option is to navigate to https://www.ready.noaa.gov/data/archives/reanalysis/
#   and download files.
# You can also set up an ftp in windows explorer, which can be a bit finnicky but is much faster. 
# It is also possible to scrape the files directly via R but that's quite involved. 
# Note that while splitr can technically download met files on an as-needed basis,
#   this is extremely slow. Definitely best to get the files in advance.
# You can also contact me at matt.harris@earthsciences.nz as I have a personal collection of met data 
#   in a OneDrive folder if the arl FTP is too tricky.

##### SCRIPT SETUP [ALWAYS RUN] #####

## Section notes
# Generic script setup for the packages required.
# Feel free to hash out the package installs after first use.

## PACMAN
# install.packages("pacman")
library(pacman)

# install.packages('pkgload')
# install.packages('installr')

## Package uninstall + reinstall lines, if needed
# p_load(pkgload,installr,devtools)
# pkgload::unload('splitr')
# installr::uninstall.packages('splitr')
# devtools::install_github("MRPHarris/splitr@main")

## CORE PACKAGES: SYNTAX
pacman::p_load(dplyr,magrittr,lubridate,tibble,R.utils,tidyverse,openair,chron, here)

## CORE PACKAGES: MODELLING
# For running splitr, I recommend using my personal fork of the package as I've 
#   solved a few bugs over the years. Refer to the package uninstall lines above if 
#   you need to get rid of a previous splitr installation first.
# install.packages('pak')
# pak::pak("MRPHarris/splitr@main")
# pak::pak("MRPHarris/trajSpatial")
pacman::p_load(splitr, openair, trajSpatial)

# PACKAGES: DISPLAY
# Optional, probably not used in this particular script. 
# pacman::p_load(raster,rgdal,mapproj,ggplot2,mapdata,viridis)

## PACKAGES: SYNTAX
pacman::p_load(lubridate,tibble,dplyr,R.utils,chron, zoo)

## WORKING DIRECTORY: DEFAULT HYSPLIT INSTALL FOLDER
wd <- "C:/HYSPLIT/working/"
setwd(wd)

## PROJECT DIRECTORY
# Used in place of working directory as splitr requires the wd to be the hysplit folder.
# Put all the scripts here (see source call below for the hysplit-functions script).
proj_dir <- paste0(here::here(),"/")

## METEOROLOGICAL FILE DIRECTORIES
# Set these depending on where you've put things
met_dir_main <- "D:/Data/" # For wherever you're putting your met data
reanalysis_met_dir <- paste0(met_dir_main,"Reanalysis Meteorological Data/") # reanalysis
# gdas1_met_dir <- paste0(met_dir_main,"GDAS1 Meteorological Data/") # gdas, if you're using it
if(!dir.exists(reanalysis_met_dir)){message("The directory 'reanalysis_met_dir' does not exist or is not valid. Check the path and confirm it is a real directory.")}
# if(!dir.exists(gdas1_met_dir)){message("The directory 'reanalysis_met_dir' does not exist or is not valid. Check the path and confirm it is a real directory.")}

## EXPORT FILE DIRECTORY
# Export directory; outputs will typically be sent here. 
# I've set this to be a sub-directory of the project folder by default.
#    dir.create() doesn't overwrite.
export_dir <- paste0(proj_dir,"collated_exports/")
dir.create(export_dir)

## SOURCE REQUIRED FUNCTIONS
# Various functions are required, which are packaged in a separate script.
# Source that script.
source(paste0(proj_dir,"hysplit-functions-20260528.R"))

##### Running HYSPLIT #####

## Section notes
# The code in this script uses a range of extra functions built on top of the 
#   splitr package to perform runs contiguously (i.e., back-to-back) for as long
#   a time period as the user specifies, and has met data available for.

# Generally, this code can be set up to run and, if everything works, left to its
#   own devices until the model is finished. It is recommended to check your PC's settings
#   on sleep and powering off to make sure it won't go to sleep, which will terminate the model.

# Set your latitude and longitude. Here I have placeholder values for Patriot Hills.
PH_lat = -80.3
PH_lon = -81.3

# Iteration specific directories
# Specify a custom sub-directory to export to, if you so wish.
export_dir <- export_dir

# Create a sequence of years to run to. I recommend testing for a small 
# subset (say, 5 years) before committing to multi-decadal runs.
year_list <- seq(2015,2015,1)

# The code below operates around a loop for each year.
# The initial lines check for the current system time and do not attempt to
#   run if the computations risk going through midnight (1 hour - replace with
#   however long your PC takes to run 1 year of trajectories), as this can mess with the metadata. 
# Within the loop, there is a function, Yearly_Site_HYPSLIT(), that will run
#   the model for all days in the year using the specified parameters.
# After the model has run for the year, a second function, Export_YSH(), that
#   grabs all the endpoint files from the hysplit working directory and collates
#   them into a formatted .csv in your specified export_dir (see function params).


# I have set trajectories per day to 1. There is an underlying function that 
#   adds a unique identifier to each trajectory and that requires a different
#   parameter specification if you choose ntraj per day > 1. See the 
#   hysplit_functions script for that function. 

## Main trajectory parameters
start_latitude = -80.3
start_longitude = -81.3
site_run_name = "PHtst"
traj_duration_hrs = 24
start_height_magl = 1500
traj_direction = 'backward'
met_data_type = 'reanalysis'

## Run main loop
it_list <- vector('list',length = length(year_list))
for(y in seq_along(it_list)){
  # What are the times encompassing an hours worth of computation?
  time_check_hours <- 1
  time_start <- Sys.time() # Current time
  time_end <- time_start + (60*60*time_check_hours) # Time an hour from now. The execution time will depend on the system.
  # Get the dates
  date_exec_start <- format(as.POSIXct(time_start), format = "%Y-%m-%d")
  date_exec_end <- format(as.POSIXct(time_end), format = "%Y-%m-%d")
  if(date_exec_end > date_exec_start){
    message(paste0("A day change will occur in the next ",time_check_hours," hours. Pausing execution."))
    Sys.sleep(60*60*time_check_hours)
    time_sleep_complete <- Sys.time()
    message(paste0("Pause completed at ",time_sleep_complete,". Resuming execution."))
  } else {
    message("No day change in the next hour. Proceeding without system pause.")
  }
  # Computations
  met_dir_it <- reanalysis_met_dir
  Yearly_Site_HYSPLIT(site_name = site_run_name,
                      year = year_list[y],
                      site_lat = start_latitude,
                      site_lon = start_longitude,
                      traj_duration = traj_duration_hrs,
                      start_height = start_height_magl,
                      direction = traj_direction,
                      met_type = met_data_type,
                      ntraj_per_day = 1,
                      ntraj_1_midday = TRUE,
                      year_handle = NULL,
                      met_dir = met_dir_it,
                      output_folder = "C:/hysplit/working/1/")
  message("Trajectories complete for ",site_run_name,": ",year_list[y]," | Year ",y,"/",length(year_list))
  Export_YSH(site_name = site_run_name,
             year = year_list[y],
             lat = start_latitude,
             lon = start_longitude,
             traj_duration = traj_duration_hrs,
             start_height = start_height_magl,
             direction = traj_direction,
             met_type = met_data_type,
             ntraj_per_day = 1,
             met_dir = met_dir_it,
             output_folder = "C:/hysplit/working/1/",
             save_dir = export_dir)
  message("Trajectory data saved for ",site_run_name,": ",year_list[y]," | Year ",y,"/",length(year_list))
}

# If all goes to plan, when the loop completes, you'll have a bunch of per-year 
# .csv files in your export_dir.

##### Troubleshooting #####


# Here's the function. At the moment, the parts that read the outputs back into the workspace have been removed and added as a second function.
# Make sure the functions above are read into the environment.
Yearly_Site_HYSPLIT <- function(site_name, 
                                year, 
                                site_lat, 
                                site_lon, 
                                traj_duration, 
                                start_height, 
                                direction, 
                                met_type, 
                                ntraj_per_day,
                                ntraj_1_midday = TRUE,
                                year_handle = NULL,
                                met_dir, 
                                output_folder = "C:/hysplit4/working/1/", 
                                verbose = FALSE){
  ## test vars
  site_name = site_run_name
  year = year_list[y]
  site_lat = start_latitude
  site_lon = start_longitude
  traj_duration = traj_duration_hrs
  start_height = start_height_magl
  direction = traj_direction
  met_type = met_data_type
  ntraj_per_day = 1
  ntraj_1_midday = TRUE
  year_handle = NULL
  met_dir = met_dir_it
  output_folder = "C:/hysplit/working/1/"
  verbose = TRUE
  ##
  
  if(ntraj_per_day > 1){
    ntraj_1perday = FALSE
  } else{
    ntraj_1perday = TRUE
  }
  if(nchar(site_name) <= 5){
  } else{
    stop("Please shorten the site_name to 5 characters or less.")
  }
  if(direction == "forward" || direction == "backward"){
  } else{
    stop("Please enter a valid direction: either forward or backward")
  }
  if(met_type == "gdas1" || met_type == "reanalysis"){
  } else{
    stop("Please enter a valid met type: gdas1 or reanalysis")
  }
  if(ntraj_per_day == 1 || ntraj_per_day == 2 || ntraj_per_day == 3 || ntraj_per_day == 4 || ntraj_per_day == 6 || ntraj_per_day == 8 || ntraj_per_day == 10 || ntraj_per_day == 12){
  } else{
    stop("trajectories per day (ntraj_per_day) must be a whole divisor of 24")
  }
  if(ntraj_per_day <= 24){
  } else {
    stop("number of trajectories per day cannot be greater than 24!")
  }
  # Setup for months, days, years, hours. Hours conditional on 1 traj per day and midday specification in function params.
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29
  } # leap year + century feb month adjustment
  if(isTRUE(ntraj_1perday)){
    if(isTRUE(ntraj_1_midday)){
      # Alter hourly increment to be 12.
      hours_increments = 12
    } else {
      hours_inc <- seq(0,24,by = (24/ntraj_per_day))  # trajectories per day setting
      hours_increments <- hours_inc[1:(length(hours_inc)-1)]
    }
  } else {
    hours_inc <- seq(0,24,by = (24/ntraj_per_day))  # trajectories per day setting
    hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  }
  # Main loop! Runs HYSPLIT for every month.
  for(i in seq_along(month_init)){
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    ## Omit the first (hours/24) days from the calculation if it is the first month of the first year.
    if(!is.null(year_handle) && year == year_handle && month_init == 1){
      start_date_day_formatted <- traj_duration/24 + 1
    } else {
      start_date_day_formatted <- 1
    }
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i])
    i_date <- Sys.Date()
    i_date <- format(as.Date(i_date, "%d-%m-%y"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    month_trajset <- hysplit_trajectory(
      lat = site_lat,
      lon = site_lon,
      height = start_height,
      duration = traj_duration,
      days = seq(
        lubridate::ymd(start_date),
        lubridate::ymd(end_date),
        by = "1 day"
      ),
      daily_hours = hours_increments,
      direction = direction,
      met_type = met_type,
      extended_met = TRUE,
      clean_up = FALSE,
      met_dir = met_dir,
      traj_name = set_name
    )
    message(site_name,year,": ",i,"/12")
  }
}
