# System: Linux 5.4.0-40-generic, Ubuntu 20.04
# R: Version 3.6.3 (2020-02-29)
# RStudio: Version 1.2.5033

# For the full tutorial, please reference URL: 
# https://problemxsolutions.com/data-visualization/spatial-visualizations-in-r-part-1/

# https://www.geospatialhub.org/pages/applications
# These are the resources for Wyoming county shapefiles
# URL: https://www.geospatialhub.org/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0

# *************************************
# Preparation
# *************************************
library(tidyverse)
library(rgdal)
library(rgeos)
library(sf)
library(geosphere)

# Depricated
# wyoming <- maptools::readShapePoly(fn = "./wyo_county_shapefile/DOR_-_Counties.shp/")

# using the `sf::read_sf` function after getting a warning that the previous method had been deprecated
wyoming <- read_sf("./wyo_county_shapefile/DOR_-_Counties.shp")

# *************************************
# Data Exploration
# *************************************
head(wyoming)
# Simple feature collection with 6 features and 8 fields
# geometry type:  POLYGON
# dimension:      XY
# bbox:           xmin: -12251080 ymin: 5011878 xmax: -11583010 ymax: 5621731
# projected CRS:  WGS 84 / Pseudo-Mercator
# # A tibble: 6 x 9
# OBJECTID Shape_Leng   Shape_Area COUNTYNAME YEARCOLLEC TAXYEAR TYPENAME     RuleID                                                              geometry
# <int>      <dbl>        <dbl> <chr>           <int>   <int> <chr>        <chr>                                                          <POLYGON [m]>
# 1        1    661865. 26008733357. Natrona          2013    2014 County Boun… NA     ((-11971610 5275121, -11971607 5275669, -11971604 5276216, -11971601…
# 2        2    467223. 12347805011. Laramie          2013    2014 County Boun… NA     ((-11719727 5070782, -11719415 5070781, -11719418 5071322, -11719421…
# 3        3    569470. 13019824414. Sheridan         2013    2014 County Boun… NA     ((-12011702 5621687, -12009434 5621684, -12007827 5621676, -12007309…
# 4        4    947920. 48718545955. Sweetwater       2013    2014 County Boun… NA     ((-12251071 5197307, -12251072 5197678, -12251070 5197845, -12251062…
# 5        5    718621. 20003634370. Albany           2013    2014 County Boun… NA     ((-11835800 5030159, -11835799 5030215, -11835789 5030673, -11835789…
# 6        6    846438. 37022152214. Carbon           2013    2014 County Boun… NA     ((-12014634 5104202, -12014630 5104738, -12014627 5105273, -12014623…

# The `wyoming` object is in an easy structure that does not need to be processed as much as when we imported the shapefile via the `maptools::readShapePoly` method

str(wyoming)
# tibble [23 × 9] (S3: sf/tbl_df/tbl/data.frame)
# $ OBJECTID  : int [1:23] 1 2 3 4 5 6 7 8 9 10 ...
# $ Shape_Leng: num [1:23] 661865 467223 569470 947920 718621 ...
# $ Shape_Area: num [1:23] 2.60e+10 1.23e+10 1.30e+10 4.87e+10 2.00e+10 ...
# $ COUNTYNAME: chr [1:23] "Natrona" "Laramie" "Sheridan" "Sweetwater" ...
# $ YEARCOLLEC: int [1:23] 2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
# $ TAXYEAR   : int [1:23] 2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
# $ TYPENAME  : chr [1:23] "County Boundary" "County Boundary" "County Boundary" "County Boundary" ...
# $ RuleID    : chr [1:23] NA NA NA NA ...
# $ geometry  :sfc_POLYGON of length 23; first list element: List of 1
# ..$ : num [1:1569, 1:2] -1.2e+07 -1.2e+07 -1.2e+07 -1.2e+07 -1.2e+07 ...
# ..- attr(*, "class")= chr [1:3] "XY" "POLYGON" "sfg"
# - attr(*, "sf_column")= chr "geometry"
# - attr(*, "agr")= Factor w/ 3 levels "constant","aggregate",..: NA NA NA NA NA NA NA NA
# ..- attr(*, "names")= chr [1:8] "OBJECTID" "Shape_Leng" "Shape_Area" "COUNTYNAME" ...

names(wyoming)
# [1] "OBJECTID"   "Shape_Leng" "Shape_Area" "COUNTYNAME" "YEARCOLLEC" "TAXYEAR"    "TYPENAME"   "RuleID"     "geometry"  

# *************************************
# Data Transformation
# *************************************
# Apply the function processes to each geometry object to structure the coordinates into a table
wy_county_polys <-
lapply(1:nrow(wyoming),
       function(i){ 
         wyoming$geometry[[i]] %>% 
           st_coordinates %>% 
           data.frame %>% 
           rename('long' ='X', 'lat'='Y') %>% 
           mutate(OBJECTID = wyoming$OBJECTID[i],
                  COUNTYNAME = wyoming$COUNTYNAME[i])}) %>% 
  bind_rows() %>% 
  tibble()
  
head(wy_county_polys)
# # A tibble: 6 x 6
#         long      lat    L1    L2 OBJECTID COUNTYNAME
#        <dbl>    <dbl> <dbl> <dbl>    <int> <chr>      
# 1 -11971610. 5275121.     1     1        1 Natrona    
# 2 -11971607. 5275669.     1     1        1 Natrona    
# 3 -11971604. 5276216.     1     1        1 Natrona    
# 4 -11971601. 5276763.     1     1        1 Natrona    
# 5 -11971601. 5277270.     1     1        1 Natrona    
# 6 -11971601. 5277777.     1     1        1 Natrona    

# Quick plot to check our work
ggplot(data = wy_county_polys, 
       aes(x = long, y = lat, group = COUNTYNAME, fill =COUNTYNAME)) +
  geom_polygon() 


# Assign the plot with settings so we dont have to rewrite the code later.
wy_plot <- 
  ggplot(data = wy_county_polys, 
         aes(x = long, y = lat, group = COUNTYNAME, fill =COUNTYNAME)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_equal() +
  labs(x = 'Longitude', y = 'Latitude', 
       title = 'County Map of Wyoming') +
  theme(panel.background  = element_rect(fill = 'gray20'), 
        panel.border  = element_rect(linetype  = 'solid', fill = NA), 
        panel.spacing  = unit(0.2, 'lines'), 
        strip.text  = element_text(), 
        strip.background  = element_rect(linetype = 'solid', color = 'black'), 
        axis.text  = element_text(color = 'black'), 
        axis.ticks.length  = unit(0, "cm"))

# Calculate the centroid of each polygon then bind the two table objects into one.
cog_df <-
  sapply(1:nrow(wyoming), 
         function(i) wyoming$geometry[[i]] %>% st_centroid) %>% 
  t %>%  
  data.frame %>% 
  tibble %>% 
  rename('long_cog' ='X1', 'lat_cog'='X2') %>% 
  bind_cols(wyoming)

# If you wanted to manually adjust locations after this, you could
# edit the value and save off to a csv for future use.
cog_df %>% edit()
cog_df %>% 
  write_csv(., path = "./ProblemXSolutions.com/Wyoming/wyo_cog.csv")


# *************************************
# Data Visualization
# *************************************
wy_plot +
  guides(fill=FALSE) +
  annotate("text", 
           x = cog_df$long_cog, 
           y = cog_df$lat_cog, 
           label  = cog_df$COUNTYNAME, 
           size = 4)
