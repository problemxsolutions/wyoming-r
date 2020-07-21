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

wyoming <- maptools::readShapePoly(fn = "./wyo_county_shapefile/DOR_-_Counties.shp/")
 
# *************************************
# Data Exploration
# *************************************
slotNames(wyoming)
# [1] "data"        "polygons"    "plotOrder"   "bbox"        "proj4string"

wyoming@plotOrder
# [1]  4 10  6 11  1 17 23 22 16 13  5 12  9 18  3 14  2 21 20  7 15  8 19

wyoming@bbox
#         min       max
# x -12362645 -11583006
# y   5011573   5622428

wyoming@proj4string
# CRS arguments: NA 

str(wyoming)
# Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
# ..@ data       :'data.frame':	23 obs. of  8 variables:
#   .. ..$ OBJECTID  : int [1:23] 1 2 3 4 5 6 7 8 9 10 ...
# .. ..$ Shape_Leng: num [1:23] 661865 467223 569470 947920 718621 ...
# .. ..$ Shape_Area: num [1:23] 2.60e+10 1.23e+10 1.30e+10 4.87e+10 2.00e+10 ...
# .. ..$ COUNTYNAME: Factor w/ 23 levels "Albany","Big Horn",..: 13 11 17 19 1 4 8 16 2 7 ...
# .. ..$ YEARCOLLEC: int [1:23] 2013 2013 2013 2013 2013 2013 2013 2013 2013 2013 ...
# .. ..$ TAXYEAR   : int [1:23] 2014 2014 2014 2014 2014 2014 2014 2014 2014 2014 ...
# .. ..$ TYPENAME  : Factor w/ 1 level "County Boundary": 1 1 1 1 1 1 1 1 1 1 ...
# .. ..$ RuleID    : Factor w/ 0 levels: NA NA NA NA NA NA NA NA NA NA ...
# .. ..- attr(*, "data_types")= chr [1:8] "N" "N" "N" "C" ...
# ..@ polygons   :List of 23
# .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
# LONG READ OUT

names(wyoming)
# [1] "OBJECTID"   "Shape_Leng" "Shape_Area" "COUNTYNAME" "YEARCOLLEC" "TAXYEAR"    "TYPENAME"   "RuleID"


# *************************************
# Data Transformation
# *************************************

wyoming@data$id <- rownames(wyoming@data)

wyoming.points <- fortify(wyoming)
#          long     lat order  hole piece id group
# 1   -11971610 5275121     1 FALSE     1  0   0.1
# 2   -11971607 5275669     2 FALSE     1  0   0.1
# 3   -11971604 5276216     3 FALSE     1  0   0.1
# 4   -11971601 5276763     4 FALSE     1  0   0.1

wyoming.df <- left_join(x = wyoming.points, y = wyoming@data, by = "id")

head(wyoming.df)
#        long     lat order  hole piece id group OBJECTID Shape_Leng  Shape_Area COUNTYNAME YEARCOLLEC TAXYEAR        TYPENAME RuleID
# 1 -11971610 5275121     1 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>
# 2 -11971607 5275669     2 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>
# 3 -11971604 5276216     3 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>
# 4 -11971601 5276763     4 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>
# 5 -11971601 5277270     5 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>
# 6 -11971601 5277777     6 FALSE     1  0   0.1        1   661865.3 26008733357    Natrona       2013    2014 County Boundary   <NA>

# show what the quick method looks like when displaying labels
wy_plot <- 
  ggplot(data = wyoming.df, 
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

# Calculate the centroid of each polygon, method1
cog_df <- 
  wyoming.df %>% 
  group_by(id, COUNTYNAME) %>% 
  summarise(long = mean(long), 
            lat = mean(lat))
# # A tibble: 23 x 4
# # Groups:   id [23]
#   id    COUNTYNAME        long      lat
#   <chr> <fct>            <dbl>    <dbl>
# 1 0     Natrona     -11890980. 5290483.
# 2 1     Laramie     -11661355. 5067093.
# 3 10    Park        -12220736. 5487026.
# 4 11    Lincoln     -12306601. 5217259.
wy_plot +
  guides(fill=FALSE) +
  annotate("text", 
           x = cog_df$long, 
           y = cog_df$lat, 
           label  = cog_df$COUNTYNAME, 
           size = 4) 

# Exploring different ways to find an appropriate center of a polygon
sample_county <- wyoming.df %>% filter(COUNTYNAME == "Park")

# Taking a simple average of the long/lat points.
sample_cog0 <- 
  sample_county %>% 
  group_by(id, COUNTYNAME) %>% 
  summarise(long = mean(long), lat = mean(lat))

sample_cog1 <- 
  sample_county %>% 
  group_by(id, COUNTYNAME) %>% 
  summarise(long = mean(c(max(long),min(long))), 
            lat = mean(c(max(lat),min(lat))))

# Using information already available in the object structure
sample_cog2 <-
  wyoming@polygons[[11]]@labpt %>% 
  t() %>%  
  as.data.frame() %>% 
  mutate('id' = '10', 'COUNTYNAME' = 'Park') %>% 
  rename('long'='V1', 'lat'='V2') %>% 
  select(3,4,1,2)

# Using the gCentroid function to calculate the centroid of the polygon
sample_cog3 <-
  wyoming%>%
  gCentroid(.,byid=TRUE) %>% 
  as.data.frame() %>% 
  slice(11) %>% 
  mutate('id' = '10', 'COUNTYNAME' = 'Park') %>% 
  rename('long'='x', 'lat'='y') %>% 
  select(3,4,1,2)

sample_cog_df <- bind_rows(sample_cog0, 
                           sample_cog1, 
                           sample_cog2, 
                           sample_cog3)

sample_cog_df$Method <- row.names(sample_cog_df)

# Plot ot check out where the polygon centers are based on the method used
sample_plot <- 
ggplot(data = sample_county, 
       aes(x = long, y = lat, group = COUNTYNAME, fill =COUNTYNAME)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_equal() +
  guides(fill=FALSE) +
  annotate("text", 
           x = sample_cog_df$long, 
           y = sample_cog_df$lat, 
           label  = sample_cog_df$Method, 
           size = 4)

# This is a good function to extract all the labpt data from the object
county_labpt <- 
  lapply(X = 1:length(wyoming@polygons),
         FUN = function(i) {
           data.frame('id' = wyoming@polygons[[i]]@ID,
                      'long' = wyoming@polygons[[i]]@labpt[1],
                      'lat' = wyoming@polygons[[i]]@labpt[2])}) %>% 
  bind_rows() %>% as.tibble()

county_labpt %<>% 
  left_join(x = ., 
            y = wyoming@data, 
            by = 'id')

# If you wanted to manually adjust locations after this, you could
# edit the value and save off to a csv for future use.
county_labpt %>% edit()
county_labpt %>% 
  write_csv(., path = "./ProblemXSolutions.com/Wyoming/wyo_cog.csv")


# *************************************
# Data Visualization
# *************************************
wy_plot +
  guides(fill=FALSE) +
  annotate("text", 
           x = county_labpt$long, 
           y = county_labpt$lat, 
           label  = county_labpt$COUNTYNAME, 
           size = 4)
