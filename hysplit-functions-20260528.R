## Various functions used in Matt's HYSPLIT work
# Please do not distribute

# matt.harris@earthsciences.nz
# Last substantively updated 19/02/2025.

# Some of these are a part of the trajSpatial package.

##### General utility #####

## Section notes
# Functions used by others for cutting, dicing, slicing.

# Format data for geom_step.
stepformat <- function(plotdata, trail = TRUE){
  plotdata2 <- plotdata %>%
    mutate_at(vars(everything()), as.numeric)
  ## Steps:
  if(isTRUE(trail)){
    # Add repeated entry at tail
    plotdata2 <- plotdata2 %>%
      dplyr::arrange(desc(.[[1]]))
    newrow = plotdata2[nrow(plotdata2),]
    newrow[1,1] = newrow[1,1]  - 1
    plotdata2 <- rbind(plotdata2,newrow)
  } else if(!isTRUE(trail)){
    plotdata2 <- plotdata2 %>%
      dplyr::arrange(desc(.[[1]]))
    # Add leading entry
    newrow <- plotdata2[1,]
    newrow[1,1] = plotdata2[1,1] + 1
    plotdata2 <- rbind(newrow,plotdata2)
  }
  ## reorder
  # Is it in descending order?
  if(!is.unsorted(plotdata[,1])){
    ## Descending. Change.
    plotdata2 <- plotdata2 %>%
      dplyr::arrange(.[[1]])
  } else {
    # Ascending. no change.
  }
  plotdata2
}
# Generate sd polygons for plotting monthly means.
stmonth_sd_polygon <- function(stacked_monthmeans){
  t<- data.frame(x = c(rep(stacked_monthmeans$month), rev(rep(stacked_monthmeans$month))),
                 y = c(stacked_monthmeans$month_varmean + stacked_monthmeans$month_varsd, rev(stacked_monthmeans$month_varmean - stacked_monthmeans$month_varsd)))
  t
}

extract_trajmean_monthly <- function(collated_trajdat,colname){
  collated_trajdat$diffs <- cumsum(c(0,as.numeric(diff(collated_trajdat$month))!=0)) + 1
  var_months <- collated_trajdat %>%
    dplyr::group_by(month) %>%
    mutate(month_varmean = mean(get(colname), na.rm = T)) %>%
    mutate(month_varsd = sd(get(colname), na.rm = T))
  avvar_months <- data.frame(month = unique(var_months$month),
                             month_varmean = unique(var_months$month_varmean),
                             month_varsd = unique(var_months$month_varsd))
  avvar_months
}
# add_traj_identifier for cumulative sum referencing
Add_traj_identifier <- function(x, ntraj_1 = FALSE){
  # This function assumes that convert_openair() has been used on the dataset.
  if(!is.data.frame(x)){
    x <- as.data.frame(x)
  }
  # Compute trajectory number. Starts at 1.
  if(!isTRUE(ntraj_1)){
    x$start.time.minutes <- substr(x[,12],12,19)
    x$start.time.minutes <- 60*24*as.numeric(chron::times(x$start.time.minutes))
    x$trajectory <- cumsum(c(0, as.numeric(diff(x$start.time.minutes)) != 0)) + 1
  } else {
    x$diffs <- 1
    x$diffs[2:nrow(x)] <- diff(x$hour.inc)
    x$trajectory <- cumsum(c(0,as.numeric(x$diffs[2:nrow(x)]) != -1)) + 1
  }
  #  x_new <<- as.data.frame(x)
  x
  #  rm(x_new, envir = parent.frame())
}
# Trim path from SampleQueue
trim_path <- function(filenames){
  if(length(filenames) > 1){
    it_list <- vector(mode = "list", length = length(filenames))
    trimmed_filenames <- vector(mode = "character", length = length(filenames))
    for(f in seq_along(filenames)){
      filename_trimmed <- unlist(strsplit(filenames[f],"/"))[length(unlist(strsplit(filenames[f],"/")))]
      trimmed_filenames[f] <- filename_trimmed
    }
    trimmed_filenames
  } else if(length(filenames) == 1){
    filename <- filenames
    filename_trimmed <- unlist(strsplit(filename,"/"))[length(unlist(strsplit(filename,"/")))]
    filename_trimmed
  } else{
    message("Empty object; no path to trim.")
  }
}
# Adds sequential variables for starting hour, day, month, year for a given set of trajectories.
format_trajdata <- function(traj_data, verbose = FALSE){
  if(isTRUE(verbose)){
    message("Formatting trajectory data. This may take some time for larger tables.")
  }
  formatted_traj_data <- traj_data %>%
    mutate(hour_start = lubridate::hour(date.start)) %>%
    mutate(day_start = lubridate::day(date.start)) %>%
    mutate(month_start = lubridate::month(date.start)) %>%
    mutate(year_start = lubridate::year(date.start)) %>%
    mutate(hour_seq = cumsum(c(0, as.numeric(diff(lubridate::hour(date.start))) != 0)) + 1) %>%
    mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1) %>%
    mutate(month_seq = cumsum(c(0, as.numeric(diff(lubridate::month(date.start))) != 0)) + 1) %>%
    mutate(year_seq = cumsum(c(0, as.numeric(diff(lubridate::year(date.start))) != 0)) + 1)
  formatted_traj_data
}
# get folder names. From SampleQueue package
get_names <- function(directory,
                      type = "files",
                      string = NULL,
                      full_names = TRUE,
                      recursive = FALSE){
  ## input check type
  if((!isTRUE(type == "files")) && (!isTRUE(type == "folders"))){
    stop("Please specify type 'files' or 'folders'")
  }
  ## main fn
  if(type == "files"){
    # File handling
    names <- list.files(path = directory, full.names = TRUE, recursive = FALSE)
    names <- names[!file.info(names)$isdir]
    names <- names[!is.na(names)]
    if(length(names) == 0){
      stop("No files in the given directory. Try recursive?")
    }
    if(!is.null(string)){
      names <- names[grepl(string, names, fixed = FALSE)]
    }
  } else if(type == "folders"){
    # Folder handling
    names <- list.dirs(path = directory, full.names = full_names, recursive = recursive)
    # double slash handling - not necessarily needed, but why not?
    if(any(str_detect(names,"//"))){
      names <- str_replace(string = names, pattern = "//",replacement = "/")
    }
    if(!is.null(string)){
      names <- names[grepl(string, names, fixed = FALSE)]
    }
  }
  if(!isTRUE(full_names)){
    names <- str_replace(string = names,
                         pattern = directory,
                         replacement = "")
    if(any(str_detect(names,"/"))){ # slash handling - not necessarily needed, but why not?
      names <- str_replace(string = names, pattern = "/",replacement = "")
    }
  }
  return(names)
}
# Replace the current path in a filename with another
replace_path <- function(filename_full, new_path){
  filename_nopath <- trim_path(filename_full)
  filename_new = paste0(ensure_path_slash(new_path),filename_nopath)
  filename_new
}
# Ensure a path ends in a slash.
ensure_path_slash <- function(string, os = "win"){
  if(os == "win"){
    if(substr(string, nchar(string)-1+1, nchar(string)) != "/"){
      newstring <- paste0(string,"/")
    } else {
      newstring <- string
    }
  } else {
    stop("Oops, OS support only includes windows at this stage.")
  }
  newstring
}




##### Running trajectories #####

## Section notes
# Functions used for the yearly site hysplit 'parallel' setup used to run the hysplit model.

YSH_info <- function(){
  message("INFO ON FUNCTION: Yearly_Site_HYSPLIT()",
          "\n","------",
          "\n", "The Yearly_Site_HYSPLIT() function generates a yearly trajectory set for a given location somewhere in the world.",
          "\n","Read below for a rundown/reminder on the input parameters. You must have a working install of HYSPLIT.","\n","------")
  message("site_name      | name or identifier for the site/trajectory set. Will be present in output names. MUST BE ONE WORD in quotations, with less than 5 characters!!", "\n",
          "year           | the year for which you want to generate a set of trajectories.","\n",
          "site_lat       | the latitude of the target site.","\n",
          "site_lon       | the longitude of the target site.","\n",
          "traj_duration  | the duration, in hours, of each trajectory.","\n",
          "start_height   | the height of the trajectory release/endpoint, in metres. e.g. '50'","\n",
          "run_direction  | run model forwards or backwards? Forwards for standard trajectories, backwards for back-trajectories.","\n",
          "met_type       | what type of met data are you using? At the moment only gdas1 and reanalysis are recommended. Remove the associated if(){stop} statement to use other types.","\n",
          "ntraj_per_day  | number of trajectories per day. Must be a whole divisor of 24. They will be spread evenly across the day.", "\n",
          "ntraj_1_midday | TRUE/FALSE. If running 1 trajectory per day, set the start time to 12 midday rather than midnight.",
          "year_handle    | NULL or numeric. Optionally set a year to add a buffer for the first (traj_duration/24) days in Jan to prevent HYSPLIT from trying to access met data that doesn't exist.",
          "met_dir        | path to the folder in which the met files are stored","\n",
          "output_folder  | path to the folder where the trajectories will be collated. Default is C:/hysplit/working/1/","\n",
          "verbose        | set to TRUE to generate a yes/no to proceed prompt when running the function. Default is FALSE.", "\n","------", "\n",
          "The function requires the following packages: pacman, splitr, lubridate, tibble, dplyr, R.utils, rgdal, chron","\n","------")
}
# YSH_info()
# A simple format conversion function that turns the trajectories into a more easily readable format, that is also compatible with the openair package.
convert_openair <- function(x) {
  drop_columns_openair = c(
    "lat_i","lon_i","theta","rainfall","mixdepth","rh","sp_humidity",
    "h2o_mixrate","terr_msl","sun_flux", "air_temp"
  )
  x <- x[ , !(names(x) %in% drop_columns_openair)]
  x$year = 2000 + x$year
  x$receptor = 1
  x <- x %>% rename(hour.inc = hour_along, date = traj_dt_i, date2 = traj_dt)
  x
}
# Adds an identifier to each trajectory. 
Add_traj_identifier <- function(x, ntraj_1 = FALSE, direction = "forward"){
  # This function assumes that convert_openair() has been used on the dataset. 
  x_new <- as.data.frame(x)
  # Compute trajectory number. Starts at 1. 
  if(!isTRUE(ntraj_1)){
    x_new$start.time.minutes <- substr(x_new[,12],12,19)
    x_new$start.time.minutes <- 60*24*as.numeric(chron::times(x_new$start.time.minutes))
    x_new$trajectory <- cumsum(c(0, as.numeric(diff(x_new$start.time.minutes)) != 0)) + 1
  } else {
    x_new$diffs <- 1
    x_new$diffs[2:nrow(x_new)] <- diff(x_new$hour.inc)
    if(direction == "forward"){
      x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != 1)) + 1
    } else if(direction == "backward"){
      x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != -1)) + 1
    }
  }
  x_new
}

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
  library(pacman)
  pacman::p_load(splitr,lubridate,tibble,dplyr,R.utils,rgdal,chron)
  if(ntraj_per_day > 1){
    ntraj_1perday = FALSE
  } else{
    ntraj_1perday = TRUE
  }
  if(isTRUE(verbose)){
    PromptContinue <- if(interactive()){
      askYesNo("Generate yearly trajectories for the target year using user specifications? This could take a while!", 
               default = TRUE, prompts = getOption("askYesNo", gettext(c("Y/N/C"))))
    }
    if(!isTRUE(PromptContinue)){
      stop("User terminated function")}
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
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
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

# Export function for yearly site hysplit.
# I wrote this like 4 years ago don't judge me
Export_YSH <- function(site_name, year, lon, lat, traj_duration, 
                       start_height, direction = "forwards", met_type = "gdas1", 
                       ntraj_per_day, met_dir, output_folder, save_dir){
  message("Make sure i_date is the correct day if trajectories were not run today!")
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29} # leap year + century feb month adjustment
  hours_inc <- seq(0,24,by = (24/ntraj_per_day))
  hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  for(i in seq_along(month_init)){
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    start_date_day_formatted <- 1
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i])
    i_date <- Sys.Date() # iterative output name for each monthly trajectory bundle
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    traj_reread <- trajectory_read(output_folder = paste0(output_folder,set_name))
    traj_converted <- convert_openair(traj_reread)
    assign(paste0("Trajset_converted_",month_init[i]), traj_converted, envir = environment())
    message(i, "completed")
  }
  Yearly_trajset <- rbind(Trajset_converted_1,Trajset_converted_2,Trajset_converted_3,
                          Trajset_converted_4,Trajset_converted_5,Trajset_converted_6,
                          Trajset_converted_7,Trajset_converted_8,Trajset_converted_9,
                          Trajset_converted_10,Trajset_converted_11,Trajset_converted_12)
  if(ntraj_per_day == 1){
    traj1 = TRUE
  } else {
    traj1 = FALSE
  }
  Yearly_trajset <- Add_traj_identifier(Yearly_trajset, ntraj_1 = traj1, direction = direction)
  colnames(Yearly_trajset)[11]<- "date.inc"
  colnames(Yearly_trajset)[12] <- "date.start"
  save_dir = save_dir
  output_timestamp <- Sys.Date()
  file_output <- paste0(save_dir,site_name,"-",year,"-",output_timestamp,"_",traj_duration,"hr",ntraj_per_day,"TPD",start_height,"m.csv")
  write.csv(Yearly_trajset,file = file_output, row.names = FALSE)
}

# Add yearfix to ensure 'year' column output by HYSPLIT matches date.inc
collate_YSH <- function(target_folder,
                        update_traj_identifier = TRUE,
                        export_dir = NULL,
                        verbose = TRUE,
                        yearfix = TRUE){
  # Test vars
  # target_folder = paste0(traj_data_dir,"outputs/PH_1500m_240hr_reanalysis_files")
  # update_traj_identifier = TRUE
  # export_dir = paste0(traj_data_dir,"collated_outputs/")
  # verbose = TRUE
  # yearfix = TRUE
  #
  target_files <- get_names(target_folder, type = "files")
  file_list <- vector("list", length = length(target_files))
  for(f in seq_along(file_list)){
    name <- unlist(lapply(str_split(trim_path(target_files[f]),"[.]"),"[[",1))
    if(f == 1){
      frame <- read.csv(target_files[f])
      # Compare date in 'year' column and the year from the date.inc colunm
      if(yearfix){
        frame <- frame %>%
          mutate(year = year(as_date(date.inc)))
      }
    } else {
      frameit <- read.csv(target_files[f])
      if(yearfix){
        frameit <- frameit %>%
          mutate(year = year(as_date(date.inc)))
      }
      frame <- rbind(frame,frameit)
    }
    if(verbose){message("File ",f,"/",length(file_list)," appended")}
  }
  if(isTRUE(update_traj_identifier)){
    frame <- frame %>%
      dplyr::select(-trajectory) %>%
      add_traj_identifier(ntraj_1 = TRUE,direction = "backward")
  }
  if(!is.null(export_dir)){
    write.csv(x = frame,
              file = paste0(export_dir,trim_path(target_folder),"_collated.csv"),
              row.names = F)
  }
  if(verbose){message("Operation completed.")}
}


##### Clustering #####

## Note that many of these are now incorporated into the trajSpatial package.

### Reading individual files
## Read a single mean cluster tdump file
read_meanclus_tdump <- function(meanclus_tdump, header_string = "RESSURE",
                                type = "hysplit"){
  ## borrowed from splitr and other trajSpatial functions
  # file_i_path <- meanclus_tdump
  if(type == "hysplit"){
    file_lines <- readLines(meanclus_tdump, encoding = "UTF-8", skipNul = TRUE)
    ## Get header line
    header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), USE.NAMES = FALSE, function(x) grepl(header_string, x)) %>% which()
    tab <- read.delim(meanclus_tdump, sep = "", skip = header_line, header = F) %>%
      dplyr::rename(cluster = V1,
                    lat = V10,
                    lon = V11,
                    height = V12,
                    hour.inc = V9) %>%
      dplyr::select(-contains("V"))
    tab
  } else if(type == "analogue"){
    file <- read.table(meanclus_tdump) %>%
      dplyr::select(c(cluster,hour.inc,lat,lon,height))
    file
  } else {
    stop("type not recognised. Please supply one of either 'hysplit' for GUI-generated clusmeans, or 'analogue' for analogue files.")
  }
}
## Read a single CLUSLIST file
read_cluslist <- function(CLUSLIST_file){
  cluslist <- read.table(file = CLUSLIST_file)
  cluslist
}

### MASS-READING FILES
## Read in and collate ALL clusmean files
# get_clusmeans_tdump <- function(cluster_dir = "C:/hysplit/cluster/working/",
#                                 type = "hysplit",
#                                 header_string = "RESSURE"){
#   tdumps_full <- list.files(cluster_dir, full.names = TRUE)
#   tdumps_short <- list.files(cluster_dir, full.names = FALSE)
#   tdump_means <- tdumps_full[which(stringr::str_detect(tdumps_short,"Cmean"))]
#   tdump_means_short <- tdumps_short[which(stringr::str_detect(tdumps_short,"Cmean"))]
#   if(type == "analogue"){
#     tdump_means <- tdump_means[which(stringr::str_detect(tdump_means_short,"_anlg"))]
#     tdump_means_short <- tdump_means_short[which(stringr::str_detect(tdump_means_short,"_anlg"))]
#   }
#   mean_tabs <- vector('list', length = length(tdump_means))
#   for(mn in seq_along(mean_tabs)){
#     mean_tabs[[mn]] <- read_meanclus_tdump(tdump_means[mn],
#                                            type = type,
#                                            header_string = header_string)
#   }
#   names(mean_tabs) <- unlist(lapply(str_split(tdump_means_short,"[.]"),"[[",1))
#   mean_tabs
# }
# ## Read in and collate ALL CLUSLIST files
# get_cluslists <- function(cluster_wd){
#   files <- list.files(cluster_wd, full.names = TRUE)
#   files_short <- list.files(cluster_wd, full.names = FALSE)
#   cluslist_files <- files[which(stringr::str_detect(files_short,"CLUSLIST"))]
#   cluslist_files_short <- files_short[which(stringr::str_detect(files_short,"CLUSLIST"))]
#   cluslist_tabs <- vector('list', length = length(cluslist_files))
#   for(cl in seq_along(cluslist_tabs)){
#     cluslist_tabs[[cl]] <- read_cluslist(cluslist_files[cl])
#   }
#   names(cluslist_tabs) <- cluslist_files_short
#   cluslist_tabs
# }
## Get the cluster percentages from a given single CLUSLIST
# get_cluster_percentages <- function(cluslist){
#   if(!'cluster' %in% colnames(cluslist)){
#     cluslist <- cluslist %>%
#       dplyr::rename(cluster = V1)
#   }
#   ntrajectories <- nrow(cluslist)
#   nclusters <- max(cluslist$cluster)
#   # loop to get percentages
#   cluster_pc_df <- data.frame(matrix(NA,nrow = nclusters+1, ncol = 3))
#   colnames(cluster_pc_df) <- c("cluster", "percent","n")
#   it_list <- vector(mode = "list", length = nclusters)
#   for(i in seq_along(it_list)){
#     # get cluster nrows
#     ntrajectories_it <- nrow(cluslist[which(cluslist$cluster == i),])
#     cluster_pc_df[i,1] <- paste0("Cluster ",i)
#     cluster_pc_df[i,2] <- (ntrajectories_it/ntrajectories)*100
#     cluster_pc_df[i,3] <- ntrajectories_it
#   }
#   cluster_pc_df[nclusters+1,1] <- "Total"
#   cluster_pc_df[nclusters+1,2] <- sum(cluster_pc_df[1:nclusters,2])
#   cluster_pc_df[nclusters+1,3] <- ntrajectories
#   cluster_pc_df
# }
# 
# get_class_percentages <- function(cluslist,
#                                   assignments){
#   if(!'cluster' %in% colnames(cluslist)){
#     cluslist <- cluslist %>%
#       dplyr::rename(cluster = V1)
#   }
#   assignments = assignments %>%
#     dplyr::select(c(new_clust,class,class_num)) %>%
#     rename(cluster = new_clust)
#   cluslist <- cluslist %>%
#     inner_join(assignments)
#   unique_assignments <- assignments %>%
#     dplyr::select(c(class,class_num)) %>%
#     distinct() %>%
#     arrange(class_num)
#   ntrajectories <- nrow(cluslist)
#   nclass<- max(cluslist$class_num)
#   # loop to get percentages
#   class_pc_df <- data.frame(matrix(NA,nrow = nclass+1, ncol = 4))
#   colnames(class_pc_df) <- c("class","class number", "percent","n")
#   it_list <- vector(mode = "list", length = nclass)
#   for(i in seq_along(it_list)){
#     # get cluster nrows
#     ntrajectories_it <- nrow(cluslist[which(cluslist$class_num == i),])
#     class_pc_df[i,1] <- unique_assignments$class[i]
#     class_pc_df[i,2] <- unique_assignments$class_num[i]
#     class_pc_df[i,3] <- (ntrajectories_it/ntrajectories)*100
#     class_pc_df[i,4] <- ntrajectories_it
#   }
#   class_pc_df[nclass+1,1] <- "Total"
#   class_pc_df[nclass+1,2] <- NA
#   class_pc_df[nclass+1,3] <- sum(class_pc_df[1:nclass,3])
#   class_pc_df[nclass+1,4] <- ntrajectories
#   class_pc_df
# }



stmonth_sd_polygon <- function(stacked_monthmeans){
  t<- data.frame(x = c(rep(stacked_monthmeans$month), rev(rep(stacked_monthmeans$month))),
                 y = c(stacked_monthmeans$month_varmean + stacked_monthmeans$month_varsd, rev(stacked_monthmeans$month_varmean - stacked_monthmeans$month_varsd)))
  t
}


### CREATING ANALOGUE CLUSMEAN FILES
## Clusmean creator
# Takes a list of clustlists (get_clustlists output) and a set of corresponding trajectories,
# and attempts to create a set of clusmean files. The mean calculation uses geosphere::geomean, but
# produces outputs that differ slightly from HYSPLIT's trajmean program. I could not diagnose
# the differences, but they seem to be minor. Bears further investigation another time!
# create_clusmeans_analogue <- function(clustlists,
#                                       export_directory = NULL,
#                                       endpoint_folder,
#                                       traj_total_duration_hrs = 120){
#   if(is.null(export_directory)){
#     message("No export directory specified. Clusmean analogues will be collated into a nested list. This can use a lot of memory for large trajectory clustering datasets.")
#   }
#   clustlists_compiled <- vector('list',length = length(clustlists))
#   for(clist in seq_along(clustlists)){
#     ## Order doesn't really matter - just move through the iterations.
#     message("Starting clusmean creation for: ",names(clustlists)[clist])
#     clustlist_it <- clustlists[[clist]]
#     if(!'cluster' %in% colnames(clustlist_it)){
#       clustlist_it <- clustlist_it %>%
#         rename(cluster = V1)
#     }
#     # Determine number of clusters
#     nclusters <- as.data.frame(clustlist_it$cluster) %>%
#       distinct() %>% nrow() %>% as.numeric()
#     clust_itlist <- vector('list', length = nclusters)
#     for(clust in seq_along(clust_itlist)){
#       message("Starting cluster ",clust," of ", length(clust_itlist))
#       ## Iterate along clusters
#       ## Get trajectories associated with the 1st cluster of the three
#       cluster_traj_list <- as.list(clustlist_it$V8[which(clustlist_it$cluster == clust)]) %>%
#         lapply(function(file){
#           file <- read_endpoint_file(file)
#           file
#         })
#       ## HEIGHTS
#       red2hghts <- lapply(cluster_traj_list, function(f){
#         f <- f$height
#         f
#       })
#       heights <- colMeans(do.call(rbind,red2hghts))
#       ## MEAN LATS AND LONS
#       red2coords <- lapply(cluster_traj_list, function(f1){
#         f1 <- f1 %>%
#           dplyr::select(c(lon,lat))
#       })
#       endpt_list <- vector('list', length = nrow(cluster_traj_list[[1]]))
#       for(ep in seq_along(endpt_list)){
#         # For each endpoint, extract the rows at that index into a single data frame.
#         # From each list, extract the first row and combine into a single data frame.
#         endpt_list[[ep]] <- rlist::list.rbind(lapply(red2coords, function(f2){
#           f2 <- f2[ep,] %>%
#             'colnames<-'(c('x','y'))
#           f2
#         }))
#       }
#       # Means
#       endpt_means <- lapply(endpt_list, function(ept){
#         round(geosphere::geomean(xy = ept),3)
#       }) %>% rlist::list.rbind() %>% data.frame() %>% 'colnames<-'(c('lon','lat')) %>%
#         mutate(hour.inc = rev(seq(-(traj_total_duration_hrs),0,1))) %>%
#         dplyr::select(c(hour.inc,lat,lon)) %>%
#         mutate(height = heights) %>%
#         mutate(cluster = clust) %>%
#         dplyr::select(c(cluster,hour.inc,lat,lon,height))
#       clust_itlist[[clust]] <- endpt_means
#     }
#     # Compile this cluslist
#     # Export this clusmean
#     if(!is.null(export_directory)){
#       clusmean_combined_it <- rlist::list.rbind(clust_itlist)
#       write.table(clusmean_combined_it, file = paste0(export_directory,"Cmean1_",nclusters,"_anlg.tdump"))
#       message(paste0("Cmean1_",nclusters,"_anlg.tdump exported"))
#     } else if(is.null(export_directory)){
#       clustlists_compiled[[clist]] <- rlist::list.rbind(clust_itlist)
#       message(paste0("Cmean1_",nclusters," analog added to complist"))
#     }
#   }
# }

# # Read a single endpoint file.
# read_endpoint_file <- function(file, header_string = "PRESSURE"){
#   ## Column names after https://www.ready.noaa.gov/hysplitusersguide/S263.htm
#   file_lines <- readLines(file, encoding = "UTF-8", skipNul = TRUE)
#   header_line <- file_lines %>% vapply(FUN.VALUE = logical(1), USE.NAMES = FALSE, function(x) grepl(header_string, x)) %>% which()
#   tab <- read.delim(file, sep = "", skip = header_line, header = F) %>%
#     dplyr::rename(trajnum = V1,
#                   metgrid = V2,year = V3,
#                   month = V4,
#                   day = V5,
#                   hour = V6,
#                   minute = V7,
#                   fc_hour = V8,
#                   hour.inc = V9,
#                   lat = V10,
#                   lon = V11,
#                   height = V12,
#                   pressure = V13)
#   tab
# }

### PLOTTING
## Plot the mean trajectories for a given set of clusters
plot_cluster_means <- function(clustermean,
                               cluslist,
                               long_text = FALSE,
                               colourvec = 'viridis',
                               backing_colour = 'black',
                               lback_size = 0.8,
                               lfront_size = 0.5,
                               labsize = 5,
                               labels = NULL){
  ## test vars
  # clustermean = clusmean21
  #                  cluslist = cluslist21
  #                  colourvec = RA_2d12h_clus21_assignments_updated$class_colour
  #                  backing_colour = 'white'
  #                  lback_size = 2
  #                  lfront_size = 1
  #                  labels = TRUE
  #                  labsize = 7
  ##
  if(any(colourvec == 'gg_default')){
    gg_color_hue <- function(n) {
      # thank you SE https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    colourvec <- gg_color_hue(n = length(unique(clustermean$cluster)))
  } else if(any(colourvec == 'viridis')){
    colourvec <- viridis(n = length(unique(clustermean$cluster)))
  } else {
    colourvec <- colourvec
  }
  # clustermean = clusmeans$Cmean1_14
  # cluslist = cluslist$CLUSLIST_14
  termpoints <- clustermean %>%
    dplyr::filter(hour.inc == -120) %>%
    dplyr::mutate(cluslabs = paste0("C",unique(clustermean$cluster)))
  
  p <- ggplot() +
    so_50_map2_pre +
    geom_path(data = clustermean, aes(x = lon, y = lat, group = as.factor(cluster)), colour = "black", alpha = 0.9, size = lback_size) +
    geom_path(data = clustermean, aes(x = lon, y = lat, group = as.factor(cluster), colour = as.factor(cluster)), alpha = 1, size = lfront_size)
  if(isTRUE(labels)){
    p <- p +
      geom_shadowtext(data = termpoints, aes(x = lon, y = lat, group = as.factor(cluster), label = cluslabs, colour = as.factor(cluster)), bg.colour = backing_colour, size = labsize)
  }
  p <- p +
    scale_colour_manual(values = colourvec,
                        name = "Cluster") 
  if(isTRUE(long_text)){
    p <- p +
      so_50_map2_post
  } else {
    p <- p +
      so_50_map2_post_notext
  }
  p
}
## Plot the trajectories along with a cluster
# Now with proper handling for backwards trajectories!
plot_specific_cluster <- function(cluslist,
                                  clustermean,
                                  trajectories,
                                  which_cluster,
                                  bin_densities = NULL,
                                  long_text = FALSE,
                                  ntraj_1 = TRUE,
                                  direction = "backward"){
  # cluslist = new_cluslist_21
  # clustermean = new_clusmeans_analogue[[1]]
  # trajectories = trajdata_RA_full
  # which_cluster = 20
  # bin_densities = NULL
  # long_text = FALSE
  # ntraj_1 = TRUE
  # direction = "backward"
  ## Isolate the correct cluster mean.
  clusmean <- clustermean %>%
    dplyr::filter(cluster == which_cluster)
  ## Isolate the trajectory data for this specific cluster, and add an identifier
  cluster_trajectories <- extract_cluster_data(cluslist = cluslist,
                                               trajectories = trajectories,
                                               which_cluster = which_cluster) %>%
    add_traj_identifier(ntraj_1 = ntraj_1, direction = direction)
  ## Get the endpoitns
  endpts <- cluster_trajectories[which(cluster_trajectories$hour.inc == -120),]
  p <- ggplot() +
    # pre map
    so_50_map2_pre +
    # Cluster mean
    ## Cluster trajectories
    geom_path(data = cluster_trajectories, aes(x = lon, y = lat, group = trajectory), colour = "grey12", alpha = 0.4) +
    geom_point(data = endpts, aes(x = lon, y = lat),shape = 21, fill = "darkred", colour = "black") +
    geom_path(data = clusmean, aes(x = lon, y = lat), colour = "orange", alpha = 0.9, size = 1)
  if(!is.null(bin_densities)){
    p <- p +
      stat_density2d(data = endpts, aes(x = lon, y = lat), colour = "red", bins = bin_densities, geom = "contour", alpha = 0.8)
  }
  if(isTRUE(long_text)){
    p <- p +
      so_50_map2_post
  } else {
    p <- p +
      so_50_map2_post_notext
  }
  p
}

## Format a DELPCT file for plotting
# lobotomised initial bits from trajSpatial::plot_DELPCT
## bit of a mess but these are probably important
# format_obj_DELPCT <- function(DELPCT_table, threshold = 20){
#   listvars <- vector('list', length = 2)
#   newtable <- DELPCT_table
#   newtable <- newtable %>%
#     dplyr::mutate(pct_change = as.numeric((.[[3]]/lag(.[[3]]) * 100)-100))
#   # Create pct change threshold values
#   xvals_pct_change <- newtable$n_clusters[which(newtable$pct_change > threshold)] + 1
#   yvals_pct_change <- newtable$TSV_change_pct[which(newtable$pct_change > threshold)-1]
#   lines_pct_change <- data.frame(x = rep(xvals_pct_change,2),y = c(yvals_pct_change, rep(0,length(yvals_pct_change))),
#                                  np = rep(seq(from = 1,
#                                               to = as.numeric(length(xvals_pct_change)),
#                                               by = as.numeric(1)),2))
#   listvars[[1]] <- newtable
#   listvars[[2]] <- lines_pct_change
#   return(listvars)
# }

### Recoding trajectory number assignment.
## Recode a cluslist with new trajectory assignments (i.e. the old clus #s were too random)
# recode_cluslist <- function(cluslist,
#                             old_clust,
#                             new_clust){
#   if(!'cluster' %in% colnames(cluslist)){
#     cluslist <- cluslist %>%
#       dplyr::rename(cluster = V1)
#   }
#   cluslist <- cluslist %>%
#     mutate(cluster = recode(cluster, !!!setNames(new_clust, old_clust)))
#   cluslist
# }

## Recode a clusmean object with new trajectory assignments (i.e. the old clus #s were too random)
# recode_clusmean <- function(clusmean,
#                             old_clust,
#                             new_clust){
#   clusmean <- clusmean %>%
#     mutate(cluster = recode(cluster, !!!setNames(new_clust, old_clust)))
#   clusmean
# 
# }

### Functions for stacked frequency plots
## Assign trajectories to clusters
# format_trajdata_clusterfreqs <- function(trajectory_data,
#                                          cluslist,
#                                          new_assignments = NULL){
#   ## Extract date grob that can be used to match trajectories
#   cluslist_it <- cluslist
#   cluster_fnames <- cluslist_it$V8
#   ## Just do this for all clusters
#   cluster_fnames_short <- unlist(lapply(str_split(cluster_fnames,"[/]"), tail, n = 1))
#   endpt_years <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",6))
#   endpt_months <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",7))
#   endpt_days <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",8))
#   all_endpt_dates <- as.data.frame(matrix(paste(endpt_years, endpt_months,endpt_days, sep = "-"))) %>%
#     mutate(cluster = cluslist_it$cluster) %>%
#     'colnames<-'(c('just_date','cluster')) %>%
#     mutate(just_date = as_date(just_date)) %>%
#     arrange(just_date)
# 
#   ## Ok, now, given a set of trajectories:
#   trajectories <- trajectory_data
#   # 1) are they the sames size?
#   # nrow(trajectories)/121 == nrow(all_endpt_dates)
#   # 2) match the dates to the trajectories
#   trajectories <- trajectories %>%
#     mutate(just_date = as_date(just_date)) %>%
#     arrange(just_date) %>%
#     inner_join(all_endpt_dates) %>%
#     add_traj_identifier() %>%
#     mutate(start_year = year(date))
#   if(!is.null(new_assignments)){
#     trajectories <- trajectories %>%
#       inner_join(new_assignments %>% dplyr::rename(cluster = new_clust))
#   }
#   trajectories
# }

## Format a set of cluster-associated trajectories for a stacked inter-cluster frequency plot.
# Takes an output from format_trajdata_clusterfreqs
# CLUSTER FNS
cluster_stacked_frequencies_year <- function(ca_trajectories,
                                             stepfmt = F,
                                             year_range = seq(1980,2020,1)){
  ## Get the frequency of events per year
  year_event_freq <- ca_trajectories %>%
    group_by(start_year) %>%
    summarise(total_events = n()/121)
  ## Get the frequency of each cluster for a given year relative to the max number of occurences.
  yearly_clus <- ca_trajectories %>%
    group_by(start_year, cluster) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  fillyears <- data.frame(matrix(year_range)) %>%
    'colnames<-'(c('start_year'))
  nclus <- vector('list', length = length(unique(ca_trajectories$cluster)))
  for(clus in seq_along(nclus)){
    clus_it <- clus
    frame_it <- yearly_clus %>%
      filter(cluster == clus_it) %>%
      full_join(year_event_freq) %>%
      arrange(start_year) %>%
      ungroup() %>%
      mutate(cluster = clus_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) #%>%
    # mutate(freq_scaled = total_events*freq)
    if(clus == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclus[[clus]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclus[[clus]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clus_it-1),1)
      summed_freq_total <- lapply(nclus[its], function(x){
        freq <- x$freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clus_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclus[[clus]] <- frame_it
    }
    # nclus[[clus]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclus)
  allclus_list
}


cluster_stacked_frequencies_daily_movav <- function(ca_trajectories,
                                                    stepfmt = F,
                                                    year_range = seq(1980,2020,1),
                                                    winwidth = 90){
  
  # ca_trajectories = trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  # winwidth = 90
  ## Days in sequence
  tot_days = length(unique(lubridate::as_date(ca_trajectories$date.start)))
  day_sequence = seq(1,tot_days,1)
  ## Fill necessary columns where possible
  if(!'cluster' %in% colnames(ca_trajectories)){
    stop("Please add a cluster column by inner_joining with the class-cluster assignment table.")
  }
  if(!'day_seq' %in% colnames(ca_trajectories)){
    ca_trajectories <- ca_trajectories %>%
      mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1)
  }
  
  # ca_trajectories <- ca_trajectories %>%
  #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
  ## Get the frequency of events per year
  ## Add daily sequence field to trajectories
  daily_event_freq <- ca_trajectories %>%
    group_by(day_seq) %>%
    summarise(daily_events = n()/121)
  
  # year_event_freq <- ca_trajectories %>%
  #   group_by(start_year) %>% clas
  #   summarise(total_events = n()/121)
  ## Get the frequency of each class_num for a given year relative to the max number of occurences.
  daily_clus <- ca_trajectories %>%
    group_by(day_seq, cluster) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  # https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
  # daily_movfreqs <- daily_clas %>%
  #   group_by(class_num,day_seq) %>%
  #   summarise(mov_count_test = zoo::rollapply(freq, 91,function(x){sum(x)/91}, align = 'center', fill = NA))
  
  filldays <- data.frame(matrix(day_sequence)) %>%
    'colnames<-'(c('day_seq'))
  nclus <- vector('list', length = length(unique(ca_trajectories$cluster)))
  for(clus in seq_along(nclus)){
    clus_it <- clus
    frame_it <- daily_clus %>%
      filter(cluster == clus_it) %>%
      full_join(daily_event_freq) %>%
      arrange(day_seq) %>%
      ungroup() %>%
      mutate(cluster = clus_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) %>%
      mutate(mov_freq = zoo::rollapply(n_occurrences, winwidth,sum, align = 'center', fill = NA)) %>%
      mutate(mov_freq = mov_freq/winwidth)
    # mutate(freq_scaled = total_events*freq)
    if(clus == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = mov_freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclus[[clus]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclus[[clus]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clus_it-1),1)
      summed_freq_total <- lapply(nclus[its], function(x){
        mov_freq <- x$mov_freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = mov_freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclus[[clus]] <- frame_it
    }
    # nclas[[clas]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclus)
  allclus_list
}
## CLASS FNS
class_stacked_frequencies_year <- function(ca_trajectories,
                                           stepfmt = F,
                                           year_range = seq(1980,2020,1)){
  
  # ca_trajectories <- trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  
  # ca_trajectories = trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  
  if(!'class_num' %in% colnames(ca_trajectories)){
    stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
  }
  # ca_trajectories <- ca_trajectories %>%
  #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
  ## Get the frequency of events per year
  year_event_freq <- ca_trajectories %>%
    group_by(start_year) %>%
    summarise(total_events = n()/121)
  ## Get the frequency of each class_num for a given year relative to the max number of occurences.
  yearly_clas <- ca_trajectories %>%
    group_by(start_year, class_num) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  fillyears <- data.frame(matrix(year_range)) %>%
    'colnames<-'(c('start_year'))
  nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
  for(clas in seq_along(nclas)){
    clas_it <- clas
    frame_it <- yearly_clas %>%
      filter(class_num == clas_it) %>%
      full_join(year_event_freq) %>%
      arrange(start_year) %>%
      ungroup() %>%
      mutate(class_num = clas_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) #%>%
    # mutate(freq_scaled = total_events*freq)
    if(clas == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclas[[clas]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclas[[clas]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clas_it-1),1)
      summed_freq_total <- lapply(nclas[its], function(x){
        freq <- x$freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclas[[clas]] <- frame_it
    }
    # nclas[[clas]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclas)
  allclus_list
}

class_stacked_frequencies_month <- function(ca_trajectories,
                                            stepfmt = F,
                                            year_range = seq(1980,2020,1)){
  
  # ca_trajectories <- trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  
  tot_months = (max(year_range) - min(year_range)) * 12 + 12
  month_sequence = seq(1,tot_months,1)
  
  # ca_trajectories = trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  
  if(!'class_num' %in% colnames(ca_trajectories)){
    stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
  }
  if(!'month_seq' %in% colnames(ca_trajectories)){
    ca_trajectories <- ca_trajectories %>%
      mutate(month_seq = cumsum(c(0, as.numeric(diff(lubridate::month(date.start))) != 0)) + 1)
  }
  # ca_trajectories <- ca_trajectories %>%
  #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
  ## Get the frequency of events per year
  ## Add daily sequence field to trajectories
  month_event_freq <- ca_trajectories %>%
    group_by(month_seq) %>%
    summarise(monthly_events = n()/121)
  
  # year_event_freq <- ca_trajectories %>%
  #   group_by(start_year) %>%
  #   summarise(total_events = n()/121)
  ## Get the frequency of each class_num for a given year relative to the max number of occurences.
  monthly_clas <- ca_trajectories %>%
    group_by(month_seq, class_num) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  fillmonths <- data.frame(matrix(month_sequence)) %>%
    'colnames<-'(c('month_seq'))
  nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
  for(clas in seq_along(nclas)){
    clas_it <- clas
    frame_it <- monthly_clas %>%
      filter(class_num == clas_it) %>%
      full_join(month_event_freq) %>%
      arrange(month_seq) %>%
      ungroup() %>%
      mutate(class_num = clas_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) #%>%
    # mutate(freq_scaled = total_events*freq)
    if(clas == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclas[[clas]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclas[[clas]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clas_it-1),1)
      summed_freq_total <- lapply(nclas[its], function(x){
        freq <- x$freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclas[[clas]] <- frame_it
    }
    # nclas[[clas]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclas)
  allclus_list
}

class_stacked_frequencies_monthtot <- function(ca_trajectories,
                                               stepfmt = F){
  
  # ca_trajectories <- trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  #
  #   tot_months = (max(year_range) - min(year_range)) * 12 + 12
  #   month_sequence = seq(1,tot_months,1)
  
  # ca_trajectories = trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  
  if(!'class_num' %in% colnames(ca_trajectories)){
    stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
  }
  if(!'start_month' %in% colnames(ca_trajectories)){
    ca_trajectories <- ca_trajectories %>%
      mutate(start_month = month(date.start))
  }
  # ca_trajectories <- ca_trajectories %>%
  #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
  ## Get the frequency of events per year
  ## Add daily sequence field to trajectories
  month_event_freq <- ca_trajectories %>%
    group_by(start_month) %>%
    summarise(monthly_events = n()/121)
  
  # year_event_freq <- ca_trajectories %>%
  #   group_by(start_year) %>%
  #   summarise(total_events = n()/121)
  ## Get the frequency of each class_num for a given year relative to the max number of occurences.
  monthly_clas <- ca_trajectories %>%
    group_by(start_month, class_num) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  fillmonths <- data.frame(matrix(seq(1,12,1))) %>%
    'colnames<-'(c('start_month'))
  nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
  for(clas in seq_along(nclas)){
    clas_it <- clas
    frame_it <- monthly_clas %>%
      filter(class_num == clas_it) %>%
      full_join(month_event_freq) %>%
      arrange(start_month) %>%
      ungroup() %>%
      mutate(class_num = clas_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) #%>%
    # mutate(freq_scaled = total_events*freq)
    if(clas == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclas[[clas]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclas[[clas]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clas_it-1),1)
      summed_freq_total <- lapply(nclas[its], function(x){
        freq <- x$freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclas[[clas]] <- frame_it
    }
    # nclas[[clas]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclas)
  allclus_list
}

class_stacked_frequencies_daily_movav <- function(ca_trajectories,
                                                  stepfmt = F,
                                                  year_range = seq(1980,2020,1),
                                                  winwidth = 90){
  
  # ca_trajectories = trajectories
  # stepfmt = F
  # year_range = seq(1980,2020,1)
  # winwidth = 90
  ## Days in sequence
  tot_days = length(unique(lubridate::as_date(ca_trajectories$date.start)))
  day_sequence = seq(1,tot_days,1)
  ## Fill necessary columns where possible
  if(!'class_num' %in% colnames(ca_trajectories)){
    stop("Please add a class and class_num column by inner_joining with the class-cluster assignment table.")
  }
  if(!'day_seq' %in% colnames(ca_trajectories)){
    ca_trajectories <- ca_trajectories %>%
      mutate(day_seq = cumsum(c(0, as.numeric(diff(lubridate::day(date.start))) != 0)) + 1)
  }
  
  # ca_trajectories <- ca_trajectories %>%
  #   mutate(class_num = as.numeric(forcats::fct_inorder(class)))
  ## Get the frequency of events per year
  ## Add daily sequence field to trajectories
  daily_event_freq <- ca_trajectories %>%
    group_by(day_seq) %>%
    summarise(daily_events = n()/121)
  
  # year_event_freq <- ca_trajectories %>%
  #   group_by(start_year) %>%
  #   summarise(total_events = n()/121)
  ## Get the frequency of each class_num for a given year relative to the max number of occurences.
  daily_clas <- ca_trajectories %>%
    group_by(day_seq, class_num) %>%
    summarise(n = n()) %>%
    mutate(freq = n / sum(n))
  # https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
  # daily_movfreqs <- daily_clas %>%
  #   group_by(class_num,day_seq) %>%
  #   summarise(mov_count_test = zoo::rollapply(freq, 91,function(x){sum(x)/91}, align = 'center', fill = NA))
  
  filldays <- data.frame(matrix(day_sequence)) %>%
    'colnames<-'(c('day_seq'))
  nclas <- vector('list', length = length(unique(ca_trajectories$class_num)))
  for(clas in seq_along(nclas)){
    clas_it <- clas
    frame_it <- daily_clas %>%
      filter(class_num == clas_it) %>%
      full_join(daily_event_freq) %>%
      arrange(day_seq) %>%
      ungroup() %>%
      mutate(class_num = clas_it) %>%
      replace(is.na(.), 0) %>%
      mutate(n_occurrences = n/121) %>%
      mutate(mov_freq = zoo::rollapply(n_occurrences, winwidth,sum, align = 'center', fill = NA)) %>%
      mutate(mov_freq = mov_freq/winwidth)
    # mutate(freq_scaled = total_events*freq)
    if(clas == 1){
      frame_it <- frame_it %>%
        mutate(stacked_freq = mov_freq) %>%
        mutate(stacked_freq_base = 0)
      if(isTRUE(stepfmt)){
        nclas[[clas]] <- stepformat(frame_it, trail = F)
      } else if(isFALSE(stepfmt)) {
        nclas[[clas]] <- frame_it
      } else {
        stop("Please provide stepfmt as logical TRUE or FALSE.")
      }
    } else {
      # Sum the frequencies of previous groups
      its <- seq(1,(clas_it-1),1)
      summed_freq_total <- lapply(nclas[its], function(x){
        mov_freq <- x$mov_freq
      }) %>% Reduce('+',.)
      # summed_freq_base <- nclus[[clas_it-1]]$stacked_freq
      if(isTRUE(stepfmt)){
        frame_it <- stepformat(frame_it, trail = F)
      }
      # frame_it <- stepformat(frame_it, trail = F)
      frame_it <- frame_it %>%
        mutate(stacked_freq = mov_freq + summed_freq_total) %>%
        mutate(stacked_freq_base = summed_freq_total)
      nclas[[clas]] <- frame_it
    }
    # nclas[[clas]] <- frame_it
  }
  allclus_list <- rlist::list.rbind(nclas)
  allclus_list
}

## takes an output from cluster_stacked_frequencies_year. Converts it into a suitable format for filled-in steps.
plot_stacked_freq_steps <- function(cluster_stckd_freq_year = NULL,
                                    plot = T){
  ## Generate lagged stacked frequency ceilings
  testdat_ceil1 <- cluster_stckd_freq_year %>%
    # dplyr::filter(cluster == 2) %>%
    dplyr::select(c(start_year, cluster, stacked_freq)) %>%
    group_by(cluster)
  testdat_ceil2 <- bind_rows(old = testdat_ceil1,
                             new = testdat_ceil1 %>% mutate(stacked_freq = lag(stacked_freq)),
                             .id = "source") %>%
    arrange(start_year, source)
  ## Generate lagged frequency floors
  testdat_floor1 <- cluster_stckd_freq_year %>%
    # dplyr::filter(cluster == 2) %>%
    dplyr::select(c(start_year, cluster, stacked_freq_base)) %>%
    group_by(cluster)
  testdat_floor2 <- bind_rows(old = testdat_floor1,
                              new = testdat_floor1 %>% mutate(stacked_freq_base = lag(stacked_freq_base)),
                              .id = "source") %>%
    arrange(start_year, source)
  ## Combine
  testdat2 <- merge(testdat_ceil2,testdat_floor2)
  if(isTRUE(plot)){
    p <- ggplot() +
      geom_ribbon(data = testdat2, aes(x = start_year - 0.5, ymin = stacked_freq_base, ymax = stacked_freq, group = as.factor(cluster), fill = as.factor(cluster))) +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_continuous(expand = c(0,0), limits = c(2004.5,2020.5), breaks = seq(2005,2020,1)) +
      theme_cowplot(12) +
      theme(axis.text.x = element_text(colour = c("black","NA","NA","NA","NA"))) +
      labs(x = "Year", y = "Frequency",
           fill = "Cluster") +
      geom_vline(xintercept = seq(2005.5,2019.5,1), linetype = 'dashed') #+
    # geom_step(data = testdat, aes(x = start_year-0.5, y = stacked_freq, group = as.factor(cluster)), colour = 'black')
    p
  } else {
    return(testdat2)
  }
}

## Extract data from a trajectory list matching a specific cluster
extract_cluster_data <- function(cluslist,
                                 trajectories,
                                 which_cluster = 1){
  if(!'cluster' %in% colnames(cluslist)){
    cluslist <- cluslist %>%
      dplyr::rename(cluster = V1)
  }
  # First, remove the long filenames
  cluster_rows <- cluslist %>%
    dplyr::filter(cluster == which_cluster)
  cluster_fnames <- cluster_rows$V8
  cluster_fnames_short <- unlist(lapply(str_split(cluster_fnames,"[/]"), tail, n = 1))
  endpt_years <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",6))
  endpt_months <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",7))
  endpt_days <- unlist(lapply(str_split(cluster_fnames_short,"[-]"),"[[",8))
  all_endpt_dates <- as.data.frame(matrix(paste(endpt_years, endpt_months,endpt_days, sep = "-")))
  ## Date matching column
  if(!"date.start.yymmdd" %in% colnames(trajectories)){
    if(!"date.start" %in% colnames(trajectories)){
      trajectories$date.start.yymmdd <- substr(trajectories$date,3,10)
    } else {
      trajectories$date.start.yymmdd <- substr(trajectories$date.start,3,10)
    }
  }
  cluster_trajdata <- trajectories[trajectories$date.start.yymmdd %in% (all_endpt_dates[,]),]
  cluster_trajdata
}


##### Frequency grids #####

## approximate logic behind these functions:
## In my mind, this process should go as follows:
# 1) Coerce trajectory data into fully tabulated data files.
#    - this is essentially already achieved by Export_YSH and the YSH framework.
# 2) Convert tabulated endpoints into grid-mapped values.
#    - round lat/lon to halfway between grid points
#    - sum number of points for each lat/lon pair.
# 3) lat/lon frequency data as a grid in ggplot
# 4) coerce this data onto the projection used (SP Stereo).

#https://stackoverflow.com/questions/43612903/how-to-properly-plot-projected-gridded-data-in-ggplot2
generate_latlon_halfgrid <- function(min_lat,max_lat,min_lon,max_lon){
  # # test vars
  # min_lat = -90
  # max_lat = -40
  # min_lon = -180
  # max_lon = 180
  # Issue: max values are going to be rounded up.
  min_lat_int <- ifelse(sign(min_lat) == -1,
                        yes = as.numeric(as.integer(min_lat)) + 0.5,
                        no = as.numeric(as.integer(min_lat)) - 0.5)
  max_lat_int <- ifelse(sign(max_lat) == -1,
                        yes = as.numeric(as.integer(max_lat)) + 0.5,
                        no = as.numeric(as.integer(max_lat)) - 0.5)
  min_lon_int <- ifelse(sign(min_lon) == -1,
                        yes = as.numeric(as.integer(min_lon))  + 0.5,
                        no = as.numeric(as.integer(min_lon)) - 0.5)
  max_lon_int <- ifelse(sign(max_lon) == -1,
                        yes = as.numeric(as.integer(max_lon))  + 0.5,
                        no = as.numeric(as.integer(max_lon)) - 0.5)
  
  lat_seq <- seq(min_lat_int,max_lat_int,1)
  lon_seq <- seq(min_lon_int,max_lon_int,1)
  grid <- expand.grid(lon_seq,lat_seq) %>%
    'colnames<-'(c("lon","lat")) %>%
    relocate(lon,.after = lat)
}
# Polygon plotting grid generator.
# Takes a half grid (grid of 0.5 lat lon value-assignments) and creates squares for each value.
generate_latlon_gridpoly <- function(halfgrid){
  # small segment first
  # halfgrid <- grid_test[1:500,]
  it_list <- vector('list',length = nrow(halfgrid))
  new_dat <- matrix(data = NA, nrow = nrow(halfgrid)*4, ncol = 4) %>% as.data.frame() %>%
    'colnames<-'(c('lat','lon','ID','freq'))
  # if(isTRUE(cols)){
  #   new_dat <- new_dat %>%
  #     mutate(colrs = NA)
  #   colfunc <- colorRampPalette(c("black", "white"))
  #   testpal <- colfunc(length(it_list))
  # }
  for(hg in seq_along(it_list)){
    # Get row indices
    rows <- seq((4*hg)-3,4*hg,1)
    # halfpoint calcs
    halfpoint <- as.numeric(halfgrid[hg,])
    lat <- halfpoint[1]
    lon <- halfpoint[2]
    freqv <- halfpoint[3]
    # Need to invert this if the lon sign is negative.
    lonsign <- sign(lon)
    if(lonsign == 1){
      combs <- data.frame(lat = c(ceiling(lat),ceiling(lat),
                                  floor(lat),floor(lat)),
                          lon = c(ceiling(lon), floor(lon),
                                  floor(lon), ceiling(lon)),
                          ID = hg,
                          freq = freqv)
    } else {
      combs <- data.frame(lat = c(ceiling(lat),ceiling(lat),
                                  floor(lat),floor(lat)),
                          lon = c(floor(lon), ceiling(lon),
                                  ceiling(lon), floor(lon)),
                          ID = hg,
                          freq = freqv)
    }
    # if(isTRUE(cols)){
    #   combs <- combs %>% mutate(colrs = testpal[hg])
    # }
    new_dat[c(rows),] <- combs
    if(hg %% 100 == 0){
      message("Row ", hg,"/",length(it_list), " complete")
    }
  }
  new_dat %>%
    arrange(lat,lon)
  new_dat
}

get_freq_polygrid <- function(endpoints,
                              latlon_halfgrid = generate_latlon_halfgrid(-90,-40,-180,180)){
  ## EXTRACT LAT LON PAIRS
  latlon <- endpoints %>%
    dplyr::select(lat, lon)
  ## COERCE TO INTEGERS
  latlon_rnd1 <- latlon %>%
    mutate(lat = as.integer(lat)) %>%
    mutate(lon = ifelse(test = ((lon > -1) & (lon < 0)),
                        yes = (-0.5),
                        no = (as.integer(lon))))
  ## COERCE TO HALFGRID
  latlon_rnd2 <- latlon_rnd1 %>%
    mutate(lat = ifelse(test = sign(lat) == -1,
                        yes = ifelse(lat == -90, lat + 0.5, lat - 0.5),
                        no = ifelse(lat == 90, lat - 0.5, lat + 0.5))) %>%
    # Additional exception: if the sign is -1, and the value
    mutate(lon = ifelse(test = sign(lon) == -1,
                        yes = ifelse(test = lon == -180,
                                     yes = lon + 0.5,
                                     ifelse(test = lon == -0.5,
                                            yes = (lon*1),
                                            no = (lon - 0.5))),
                        no = ifelse(lon == 180, lon - 0.5, lon + 0.5)))
  ## GENERATE FREQUENCY COUNTS
  freqs <- latlon_rnd2 %>% count(lat,lon)
  ## GENERATE GENERIC HALFGRID
  # Create a grid that encompasses the values.
  grid_test <- latlon_halfgrid #%>%
  ## MERGE TO CREATE FREQUENCY HALFGRID
  match <- semi_join(freqs,grid_test, by = c("lat","lon"))
  nomatch <- anti_join(grid_test, freqs, by = c("lat","lon")) %>%
    mutate(n = 0)
  grid_freqs <- rbind(match,nomatch) %>%
    mutate(n = as.numeric(n)) %>%
    arrange(lat,lon)
  ## CREATE FREQUENCY POLYGRID
  polygrid <- generate_latlon_gridpoly(grid_freqs)
  ## RETURN
  return(polygrid)
}



