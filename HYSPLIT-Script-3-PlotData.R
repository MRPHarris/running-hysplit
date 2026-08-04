## Generic 'run hysplit' script series
## 3 - Plotting outputs

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

## PACKAGES: DISPLAY
# Optional, probably not used in this particular script, but will be later on.
pacman::p_load(raster,sf,sp,mapproj,ggplot2,mapdata,viridis)

## PACKAGES: SYNTAX
pacman::p_load(lubridate,tibble,dplyr,R.utils,chron, zoo)

## Directories
proj_dir <- paste0(here::here(),"/")
# Previous directory where collated outputs were exported
data_dir <- paste0(proj_dir,"collated_exports/")
mapdata_dir <- paste0(proj_dir,"mapping_data/")

# Data and some basic slicing and dicing
summer_months <- c(11,12,1,2,3)
winter_months <-c(4,5,6,7,8)
PHtst_1500m_24hr <- read.csv(file = paste0(data_dir,"PHtst-2015-2026-06-05_24hr1TPD1500m.csv")) %>%
  mutate(start_month = month(as.Date(date.start))) %>%
  mutate(season = ifelse(test = start_month %in% summer_months,
                         yes = 'summer',
                         no = 'winter'))

##### Plotting setup #####

## Section notes
# There are a range of ways to plot spatial data such as trajectories in R.
# Basic maps can be made using default R plotting functions, but can be a bit 
# tricky to use at high latitudes. 

# The setup here relies on ggplot2 with some WGS84 projection handling.
# All are in South Pole Stereographic (WGS 84 or EPSG 3031)
# The base mapping data are some simple global polygons trimmed to 40 or 50 degrees of latitude.

# Start by creating mapping theme objects for ggplot2.
# Two versions are created - one using <40 lat, and one using <50. 
# I also create a version that is converted to 0 to 360 longitude from -180 to 180.
# Finally, I use a 'sandwich' approach, where two objects are created to be layered 
# under and over data.

# The s50 data plots faster as it has half the points.

# Read in the map base data
s40_map_data <- readRDS(paste0(mapdata_dir,"s40_map_data.rds"))
s50_map_data <- readRDS(paste0(mapdata_dir,"s50_map_data.rds"))

# Some longitude lines
x_lines <- seq(-120,180, by = 60)

# Some thematic elements
map_theme <- list(
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'dashed', colour = "black"),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
)

# Two versions of the basemaps - one from -180 to 180 longitude, then 0 to 360. 
so_40_map <- ggplot() +
  coord_map("stereo", orientation = c(-90, 0, 0)) +   
  geom_polygon(data = s40_map_data, aes(x = long, y = lat, group = group), fill = "transparent", colour = "black", alpha = 0.8) +  # Antarctica outline polygon
  scale_y_continuous(breaks = seq(-40, -90, by = -5), labels = NULL) +
  scale_x_continuous(breaks = NULL) +    # Removes Axes and labels
  xlab("") +
  ylab("") +
  # geom_text(aes(x = 180, y = seq(-40, -80, by = -10), hjust = -0.2, label = paste0(seq(40, 80, by = 10), "S"))) + # Lat labels (optional)
  geom_text(aes(x = x_lines, y = -38, label = c("120W", "60W", "0", "60E", "120E", "180"))) + # Lon labs )optional
  geom_hline(aes(yintercept = -40), size = 1)  +  
  geom_segment(aes(y = -40, yend = -90, x = x_lines, xend = x_lines), linetype = "dashed") +   
  map_theme
so_50_map <- ggplot() +
  coord_map("stereo", orientation = c(-90, 0, 0)) +   
  geom_polygon(data = s50_map_data, aes(x = long, y = lat, group = group), fill = "transparent", colour = "black", alpha = 0.8) +  # Antarctica outline polygon
  scale_y_continuous(breaks = seq(-50, -90, by = -5), labels = NULL) +
  scale_x_continuous(breaks = NULL) +    # Removes Axes and labels
  xlab("") +
  ylab("") +
  # geom_text(aes(x = 180, y = seq(-50, -80, by = -10), hjust = -0.2, label = paste0(seq(50, 80, by = 10), "S"))) + # Lat labels (optional)
  # geom_text(aes(x = x_lines, y = -38, label = c("120W", "60W", "0", "60E", "120E", "180W"))) + # Lon labs )optional
  geom_hline(aes(yintercept = -50), size = 1)  +  
  geom_segment(aes(y = -50, yend = -90, x = x_lines, xend = x_lines), linetype = "dashed") +   
  map_theme
## 0 to 360 version of so_50_map 
s50_map_data_conv <- s50_map_data %>%
  mutate(long = long + 90)
so_50_map_conv <- ggplot() +
  coord_map("stereo", orientation = c(-90, 90, 0)) +   
  geom_polygon(data = s50_map_data_conv, aes(x = long, y = lat, group = group), fill = "transparent", colour = "black", alpha = 0.8) +  # Antarctica outline polygon
  scale_y_continuous(breaks = seq(-50, -90, by = -5), labels = NULL) +
  scale_x_continuous(breaks = NULL) +    # Removes Axes and labels
  xlab("") +
  ylab("") +
  # geom_text(aes(x = 180, y = seq(-50, -80, by = -10), hjust = -0.2, label = paste0(seq(50, 80, by = 10), "S"))) + # Lat labels (optional)
  # geom_text(aes(x = x_lines, y = -38, label = c("120W", "60W", "0", "60E", "120E", "180W"))) + # Lon labs )optional
  geom_hline(aes(yintercept = -50), size = 1)  +  
  geom_segment(aes(y = -50, yend = -90, x = x_lines + 30, xend = x_lines + 30), linetype = "dashed") +   
  map_theme

## Updated 'sandwich' system.
so_50_map2_pre <- list(
  coord_map("stereo", orientation = c(-90, 0, 0)),
  geom_polygon(data = s50_map_data, aes(x = long, y = lat, group = group), fill = 'grey', colour = NA, alpha = 1),  # Antarctica outline polygon
  geom_polygon(data = s50_map_data, aes(x = long, y = lat, group = group), fill = NA, colour = "black", alpha = 1)  # Antarctica outline polygon
)
so_50_map2_post <- list(
  geom_point(aes(x = -81.3, y = -80.3), size = 2, shape = 21, colour = "black", fill = "red"),
  scale_y_continuous(breaks = seq(-50, -90, by = -5), labels = NULL),
  scale_x_continuous(breaks = NULL),    # Removes Axes and labels
  # scale_fill_manual(values = c(pal_summer_ep)) +
  # scale_alpha_manual(values = alpha_scale_summer_ep) +
  xlab(""),
  ylab(""),
  geom_text(aes(x = x_lines, y = -46, label = c("120W", "60W", "0", "60E", "120E", "180"))),
  geom_hline(aes(yintercept = -50), size = 1),    # Adds axes
  geom_segment(aes(y = -50, yend = -90, x = x_lines, xend = x_lines), linetype = "dashed"),
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "black"),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
)
so_50_map2_post_notext <- list(
  geom_point(aes(x = -81.3, y = -80.3), size = 2, shape = 21, colour = "black", fill = "red"),
  scale_y_continuous(breaks = seq(-50, -90, by = -5), labels = NULL),
  scale_x_continuous(breaks = NULL),    # Removes Axes and labels
  # scale_fill_manual(values = c(pal_summer_ep)) +
  # scale_alpha_manual(values = alpha_scale_summer_ep) +
  xlab(""),
  ylab(""),
  # geom_text(aes(x = x_lines, y = -46, label = c("120W", "60W", "0", "60E", "120E", "180W"))),
  geom_hline(aes(yintercept = -50), size = 1),    # Adds axes
  geom_segment(aes(y = -50, yend = -90, x = x_lines, xend = x_lines), linetype = "dashed"),
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(size = 0.25, linetype = 'dashed', colour = "black"),
        axis.ticks=element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank())
)

## These objects can all be plotted directly. For example:
# The standalone 40-lat map, upon which data can be plotted directly
so_40_map

# And the 50-lat sandwich. As above, data can be added to this (see subsequent sections)
ggplot() + 
  so_50_map2_pre + 
  so_50_map2_post

##### Plotting spaghetti #####

## Section notes
# Adding some lines to maps

## The collated trajectory data has been endpoint identity tagged as a part of the 
# 'yearly site hysplit' functions by `trajSpatial::add_traj_identifier()` (see script 1 in this series).
# This adds a numeric identifier that can be used directly as a grouping variable.

# Plot spaghetti using geom_path, colour by starting month
ggplot() + 
  so_50_map2_pre + 
  geom_path(data = PHtst_1500m_24hr, aes(x = lon, y = lat, group = trajectory, colour = start_month)) +
  scale_colour_viridis(n = 12, name = "Starting month") + # slight trickiness with discrete/continuous scales etc.
  so_50_map2_post 

# Seasonal divvying, add furthest endpoints
ggplot() + 
  so_50_map2_pre + 
  geom_path(data = PHtst_1500m_24hr, aes(x = lon, y = lat, group = trajectory, colour = season)) +
  geom_point(data =  PHtst_1500m_24hr %>% filter(hour.inc == -24), aes(x = lon, y = lat, colour = season)) +
  scale_colour_manual(values = c("red","darkblue")) + # slight trickiness with discrete/continuous scales etc.
  so_50_map2_post 

##### Generating frequency polygon grids #####

## Section notes
# As your dataset grows, the information derived from individual trajectories will become more limited. 
# There are various data crunching methods that can produce useful visualisations here.

# Frequency polygrids work by calculating the frequency (number of trajectories) of endpoints points per grid cell.
# This function is experimental and unpublished! But it is also cool and seems to work.
# It is highly inefficient, iterating over grid cells
PHtst_freq_polygrid <- trajSpatial::create_freq_polygrid(endpoints_lat = PHtst_1500m_24hr$lat,
                                                         endpoints_lon = PHtst_1500m_24hr$lon)# %>%

# Trim to 50s 
# Another useful thing to do is set '0' to NA. This will mean that '0' frequency 
# cells are not plotted.
PHtst_freq_polygrid <- PHtst_freq_polygrid %>%
  filter(lat <= -50) %>%
  mutate(freq = ifelse(freq == 0, yes = NA, no = freq))


# Useful to have some knowledge of the frequency distribution
freq_summary <- PHtst_freq_polygrid %>%
  group_by(freq) %>%
  summarise(count = n()) %>%
  filter(freq > 0)
plot(x = freq_summary$freq,
     y = freq_summary$count,
     type = 'p')
# So, not many counts of values > 50.

# Colour palette object for polygons in frequency grid
pal <- viridis_pal(begin = 0.2)(length(unique(PHtst_freq_polygrid$freq)))

# This can then be plotted as a polygon object
# The underlying continent outline polygon is plotted again on top for visibility.
# For this small dataset this isn't massively informative. 
# At Patriot Hills, it just shows that the vast majority of trajectories become 
# katabatically-constrained (come from the south) before arriving at the site.
# Make sure you update the limits, breaks and labels of the colour and fill calls 
# to tweak the vis.
ggplot() + 
  so_50_map2_pre + 
  geom_polygon(data = PHtst_freq_polygrid, aes(x = lon, y = lat, group = ID, fill = freq, colour = freq)) +
  # Antarctica outline polygon
  geom_polygon(data = s50_map_data, aes(x = long, y = lat, group = group), fill = NA, colour = "white", alpha = 1) +
  scale_fill_gradientn(colours = pal, na.value = "transparent", limits = c(0,50), breaks = seq(0,50,10),
                     oob = scales::squish,
                     labels = c(seq(0,40,10),">50")) +
  scale_colour_gradientn(colours = pal, na.value = "transparent", limits = c(0,50), breaks = seq(0,50,10),
                         oob = scales::squish,
                         labels = c(seq(0,40,10),">50")) +
  so_50_map2_post_notext 

##### Density contours #####

## Section notes
# Another useful way to condense trajectories is with kernel density estimates.
# This can be a bit of a pain sometimes as the contour lines and calculations will freak out
# when they hit the 0/360 boundary. You can rotate the underlying data to fix this.

# If you are plotting density contours on top of other data, you may need to use ggnewscale to create
# new scales for colours, alpha, etc.

# Contours can be created quite easily with stat_density2d(). 

# Contour vis with trajectories is usually only informative with larger numbers of 
# longer-duration trajectories. In this example case, not much is shown.

# Some vars for setting contour visuals
# Contour size 
contsize = 0.5
# Set contour colours. This can be useful if you are extending the contour bins but only plotting every other one.
# Sub these into the manual scale values params to tweak which contours are and aren't plotted.
# contourcols = c(rep('red',5),rep(NA,25))
# contouralphas = c(rep(1,5),rep(0,16))
nbins = 25

# Plot KDE
ggplot() + 
  so_50_map2_pre + 
  stat_density2d(
    data = PHtst_1500m_24hr, aes(x = lon, y = lat, colour = as.factor(..level..), alpha = as.factor(..level..)), 
    linetype = 'solid', bins = nbins, size = contsize, geom = "contour") +
  scale_colour_manual(values = rep('red',nbins), guide = "none") +
  scale_alpha_manual(values = rep(0.8,nbins), guide = "none") +
  geom_point(aes(x = -81.3, y = -80.3), size = 2, shape = 21, colour = "black", fill = "red") +
  so_50_map2_post_notext 


