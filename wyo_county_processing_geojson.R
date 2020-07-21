# https://problemxsolutions.com/data-visualization/spatial-visualizations-in-r-part-1/


# https://www.geospatialhub.org/pages/applications
# These are the resources for Wyoming county shapefiles
# URL: https://www.geospatialhub.org/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0
# GeoService: https://services.wygisc.org/HostGIS/rest/services/GeoHub/DORCounties/MapServer/0/query?outFields=*&where=1%3D1
# geoJSON: https://opendata.arcgis.com/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0.geojson


library(tidyverse)
library(rgdal)
library(rgeos)
library(geosphere)

library(geojsonR)

wyoming <- FROM_GeoJson(url_file_string = "https://opendata.arcgis.com/datasets/b0e0a99ec14748eeae750949c7bbb2ec_0.geojson")

# https://spatialreference.org/ref/epsg/wgs-84/

slotNames(wyoming)
# NULL

names(wyoming)
# [1] "features" "type"  

str(wyoming)
# List of 2
# $ features:List of 23
# ..$ :List of 3
# .. ..$ geometry  :List of 2
# .. .. ..$ type       : chr "Polygon"
# .. .. ..$ coordinates: num [1:1569, 1:2] -108 -108 -108 -108 -108 ...
# .. ..$ properties:List of 8
# .. .. ..$ COUNTYNAME  : chr "Natrona"
# .. .. ..$ OBJECTID    : num 1
# .. .. ..$ RuleID      : NULL
# .. .. ..$ Shape_Area  : num 2.6e+10
# .. .. ..$ Shape_Length: num 661865
# .. .. ..$ TAXYEAR     : num 2014
# .. .. ..$ TYPENAME    : chr "County Boundary"
# .. .. ..$ YEARCOLLEC  : num 2013
# .. ..$ type      : chr "Feature"
# ... MORE DATA ...


# We can see that the geoJSON structure is different than that of the shapefile, 
# but the coordinates are in the format I was looking for (decimal degrees).  This
# difference changes a few of the steps, which will be documented below.

# Lets look at the first element in the list
names(wyoming$features[[1]])
# [1] "geometry"   "properties" "type"      

summary(wyoming$features[[1]])
#            Length Class  Mode     
# geometry   2      -none- list     
# properties 8      -none- list     
# type       1      -none- character

str(wyoming$features[[1]]$geometry)
# List of 2
# $ type       : chr "Polygon"
# $ coordinates: num [1:1569, 1:2] -108 -108 -108 -108 -108 ...

wyoming$features[[1]]$geometry$coordinates
#           [,1]     [,2]
# [1,] -107.5428 42.75742
# [2,] -107.5428 42.75381
# [3,] -107.5427 42.75012
# [4,] -107.5426 42.74643
# [5,] -107.5425 42.74274

# The structure of teh `wyoming$features[[1]]$properties` gives us the information that we saw in the shapefiles spatial dataframe
# To create the same table, we could `bind_rows` using an `sapply` function to loop through each of the list elements in the `wyoming` object.
str(wyoming$features[[1]]$properties)
# List of 8
# $ COUNTYNAME  : chr "Natrona"
# $ OBJECTID    : num 1
# $ RuleID      : NULL
# $ Shape_Area  : num 2.6e+10
# $ Shape_Length: num 661865
# $ TAXYEAR     : num 2014
# $ TYPENAME    : chr "County Boundary"
# $ YEARCOLLEC  : num 2013

#****************************************************************************************
wyoming$features[[22]]$geometry$type
# [1] "Polygon"

wyoming$features[[22]]$geometry %>% length
# [1] 2

wyoming$features[[22]]$geometry$coordinates %>% head

teton_1 <- wyoming$features[[22]]$geometry$coordinates[[1]][[1]] %>%
  data.frame()
teton_2 <- wyoming$features[[22]]$geometry$coordinates[[1]][[2]] %>%
  data.frame() 
#          X1       X2
# 1 -111.0486 44.27949
# 2 -111.0486 44.27986
# 3 -111.0483 44.28786
# 4 -111.0486 44.27949

ggplot() +
  geom_polygon(data = teton_1, aes(x = X1, y = X2), color = 'black', alpha = .6) +
  geom_polygon(data = teton_2, aes(x = X1, y = X2), fill = 'red', color = 'red') +
  coord_equal()
#****************************************************************************************

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

head(wyoming_county_polygons)
# A tibble: 6 x 4
#   long   lat COUNTYNAME OBJECTID
#   <dbl> <dbl> <chr>         <dbl>
# 1 -108.  42.8 Natrona           1
# 2 -108.  42.8 Natrona           1
# 3 -108.  42.8 Natrona           1
# 4 -108.  42.7 Natrona           1
# 5 -108.  42.7 Natrona           1
# 6 -108.  42.7 Natrona           1

ggplot(data = wyoming_county_polygons, 
       aes(x = long, y = lat, 
           group = COUNTYNAME, fill =COUNTYNAME)) +
  geom_polygon()

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



# show what the quick method looks like when displaying labels
ggplot(data = wyoming_county_polygons, aes(x = long, y = lat, group = COUNTYNAME, fill =COUNTYNAME)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_equal() +
  annotate("text", 
           x = cog_df$lon, 
           y = cog_df$lat, 
           label  = cog_df$COUNTYNAME, 
           size = 4) +
  labs(x = 'Longitude', y = 'Latitude', 
       title = 'County Map of Wyoming') +
  guides(fill=FALSE) +
  theme(panel.background  = element_rect(fill = 'gray20'), 
        panel.border  = element_rect(linetype  = 'solid', fill = NA), 
        panel.spacing  = unit(0.2, 'lines'), 
        strip.text  = element_text(), 
        strip.background  = element_rect(linetype = 'solid', color = 'black'), 
        axis.text  = element_text(color = 'black'), 
        axis.ticks.length  = unit(0, "cm"))
