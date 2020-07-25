# System: Linux 5.4.0-40-generic, Ubuntu 20.04
# R: Version 3.6.3 (2020-02-29)
# RStudio: Version 1.2.5033

library(tidyverse)
library(magrittr)

# Read in the spatial data to plot the Counties within 
source('~/problemxsolutions/Wyoming/wy_county_spatial_geojson.R')

file <- "./wy_covid_stats_20200724.csv"
wy_covid_df <- read_csv(file = file) 

wy_covid_df_labels <- 
  left_join(x = wy_covid_df, 
            y = cog_df, 
            by = c('County'= 'COUNTYNAME'))

wy_covid_df_labels$lat[wy_covid_df_labels$County == "Lincoln"] <- 42.15

# Using GIMP, I got the colors used in the original plot image used by the website.  
color_scheme_original_image <- c('#abddfe', # 'blue'
                                 '#abddaa', # 'green'
                                 '#ffff87', # 'yellow'
                                 '#ffcc66') # 'orange'

county_pallete <- 
c('Teton' = color_scheme_original_image[1],
  'Sweetwater' = color_scheme_original_image[1], 
  'Hot Springs' = color_scheme_original_image[1], 
  'Johnson' = color_scheme_original_image[1], 
  'Albany' = color_scheme_original_image[1], 
  'Goshen' = color_scheme_original_image[1],
  'Weston' = color_scheme_original_image[1],
  'Natrona'= color_scheme_original_image[2], 
  'Platte'= color_scheme_original_image[2],
  'Crook'= color_scheme_original_image[2], 
  'Park'= color_scheme_original_image[2],
  'Lincoln'= color_scheme_original_image[2],
  'Uinta' = color_scheme_original_image[3], 
  'Fremont' = color_scheme_original_image[3], 
  'Big Horn' = color_scheme_original_image[3], 
  'Campbell' = color_scheme_original_image[3], 
  'Niobrara' = color_scheme_original_image[3], 
  'Laramie' = color_scheme_original_image[3],
  'Sublette' = color_scheme_original_image[4],
  'Washakie' = color_scheme_original_image[4], 
  'Sheridan' = color_scheme_original_image[4], 
  'Carbon' = color_scheme_original_image[4], 
  'Converse' = color_scheme_original_image[4]
  )

base_plot <- 
  ggplot(data = wyoming_county_polygons, 
         aes(x = long, 
             y = lat, 
             group = COUNTYNAME,
             fill = COUNTYNAME)) +
  geom_polygon() +
  geom_path(color = '#5f5f57') +
  scale_fill_manual(values = county_pallete) +
  coord_map() +
  theme(panel.background  = element_rect(fill = 'gray2'), 
                      panel.border  = element_rect(linetype  = 'solid', fill = NA), 
                      panel.spacing  = unit(0.2, 'lines'), 
                      strip.text  = element_text(), 
                      strip.background  = element_rect(linetype = 'solid', color = 'NA'), 
                      axis.text  = element_text(color = 'black'), 
                      axis.ticks.length  = unit(0, "cm"),
                      plot.margin = unit(c(0,0,0,0), "pt"))


wy_covid_spatial_plot <- 
  base_plot +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = 'Longitude', y = 'Latitude', 
       title = 'Active COVID Cases by County: 2020-07-16') +
  guides(fill = FALSE) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  annotate("text", 
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = paste(wy_covid_df_labels$County, "\n", 
                          wy_covid_df_labels$`20200716`), 
           size = 3, 
           fontface = 2)


polygon_plot_data <- 
  wyoming_county_polygons %>% 
  left_join(x = ., 
            y = wy_covid_df, 
            by = c('COUNTYNAME'='County'))

# Current Active Cases Plot 
ggplot(data = polygon_plot_data, 
       aes(x = long, y = lat, 
           group = COUNTYNAME,
           fill = `20200716`)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_map() +
  scale_fill_gradient(low = 'green', 
                      high = 'red', 
                      guide = "colorbar",
                      name = "Active Cases") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  labs(x = 'Longitude', y = 'Latitude', 
       title = 'Active COVID Cases by County: 2020-07-16') +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  annotate("text", 
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = paste(wy_covid_df_labels$County, "\n", 
                          wy_covid_df_labels$`20200716`), 
           size = 3, 
           fontface = 2)

# Deaths Plot 
ggplot(data = polygon_plot_data, 
       aes(x = long, y = lat, 
           group = COUNTYNAME,
           fill = `Confirmed Cases (Overall)`)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_map() +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(low = 'green', 
                      high = 'red', 
                      guide = "colorbar", 
                      name = 'Overall Lab-Confirmed Cases') +
  ggtitle(label = "COVID Related Deaths (as of 2020-07-22)") +
  annotate("text", 
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = paste(wy_covid_df_labels$County, "\n", 
                          wy_covid_df_labels$Deaths), 
           size = 2, 
           fontface = 2)

# Mortality and Recovery Rate Plots 
polygon_plot_data %<>% 
  mutate(mortality_rate = round(Deaths / `Confirmed Cases (Overall)`, 
                                digits = 3) * 100,
         recovery_rate = round(`Confirmed Recovered (Overall)` / `Confirmed Cases (Overall)`, 
                               digits = 3) * 100)

wy_covid_df_labels %<>% 
  mutate(mortality_rate = round(Deaths / `Confirmed Cases (Overall)`, 
                                digits = 3) * 100,
         recovery_rate = round(`Confirmed Recovered (Overall)` / `Confirmed Cases (Overall)`, 
                               digits = 3) * 100)

# Mortality Plot
ggplot(data = polygon_plot_data, 
       aes(x = long, y = lat, 
           group = COUNTYNAME,
           fill = mortality_rate)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_map() +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(low = 'green', 
                      high = 'red', 
                      guide = "colorbar", 
                      name = 'Mortality Rate') +
  ggtitle(label = "COVID Mortality Rates By County (as of 2020-07-24)") +
  annotate("text", 
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = wy_covid_df_labels$County,
           size = 2, 
           fontface = 1) +
  annotate("text",
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = paste("\n\n",
                          wy_covid_df_labels$mortality_rate,
                          "%"), 
           size = 3, 
           fontface = 2)


# Recovery Rates Plot
ggplot(data = polygon_plot_data, 
       aes(x = long, y = lat, 
           group = COUNTYNAME,
           fill = recovery_rate)) +
  geom_polygon() +
  geom_path(color = 'white') +
  coord_map() +  
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(low = 'red', 
                      high = 'green', 
                      guide = "colorbar", 
                      name = 'Recovery Rate') +
  ggtitle(label = "COVID Recovery Rates By County (as of 2020-07-24)") +
  annotate("text", 
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = wy_covid_df_labels$County,
           size = 2, 
           fontface = 1) +
  annotate("text",
           x = wy_covid_df_labels$lon, 
           y = wy_covid_df_labels$lat, 
           label  = paste("\n\n",
                          wy_covid_df_labels$recovery_rate,
                          "%"), 
           size = 3, 
           fontface = 2)
