# System: Linux 5.4.0-40-generic, Ubuntu 20.04
# R: Version 3.6.3 (2020-02-29)
# RStudio: Version 1.2.5033

# For the full tutorial, please reference URL: 
# https://problemxsolutions.com/data-visualization/spatial-visualizations-in-r-part-1/
# https://problemxsolutions.com/data-visualization/working-with-geojson-in-r/

# https://www.geospatialhub.org/pages/applications
# These are the resources for Wyoming county shapefiles
# URL: https://www.geospatialhub.org/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0
# GeoService: https://services.wygisc.org/HostGIS/rest/services/GeoHub/DORCounties/MapServer/0/query?outFields=*&where=1%3D1
# geoJSON: https://opendata.arcgis.com/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0.geojson

# https://spatialreference.org/ref/epsg/wgs-84/

library(tidyverse)
library(rgdal)
library(rgeos)
library(geosphere)

library(geojsonR)

wyoming <- FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0.geojson")

wyoming_county_polygons <- 
  lapply(1:length(wyoming$features), 
         function(i){
           if(!is.list(wyoming$features[[i]]$geometry$coordinates)){
             tmpdata <- wyoming$features[[i]]$geometry$coordinates
           }else{
             tmpdata <- wyoming$features[[i]]$geometry$coordinates[[1]][1]} 
           
           tmpdata %>%
             data.frame() %>% 
             tibble %>%
             mutate(COUNTYNAME = wyoming$features[[i]]$properties$COUNTYNAME,
                    OBJECTID = wyoming$features[[i]]$properties$OBJECTID) %>% 
             rename("long" = 'X1', 'lat'='X2')
         }) %>%
  bind_rows()

# A modification of the process above to get the centroid of each polygon using the centroid function in the geosphere package.
cog_df <- 
  lapply(1:length(wyoming$features), 
         function(i){
           if(!is.list(wyoming$features[[i]]$geometry$coordinates)){
             tmpdata <- wyoming$features[[i]]$geometry$coordinates
           }else{
             tmpdata <- wyoming$features[[i]]$geometry$coordinates[[1]][1]
           } 
           
           tmpdata %>%
             data.frame %>% 
             geosphere::centroid(x = .) %>% 
             data.frame %>% 
             tibble %>%
             mutate(COUNTYNAME = wyoming$features[[i]]$properties$COUNTYNAME,
                    OBJECTID = wyoming$features[[i]]$properties$OBJECTID)
         }) %>% 
  bind_rows

# If you wanted to manually adjust locations after this, you could
# edit the value and save off to a csv for future use.
# cog_df %>% edit()
# cog_df %>% 
#   write_csv(., path = "./ProblemXSolutions.com/Wyoming/wyo_cog.csv")