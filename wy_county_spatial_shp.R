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
# cog_df %>% edit()
# cog_df %>% 
#   write_csv(., path = "./ProblemXSolutions.com/Wyoming/wyo_cog.csv")