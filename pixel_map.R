# Load libraries
library(tidyverse)
library(tidygeocoder)
library(maps)

# Set working directory
setwd('~/Documents/repos/R/pixel_map')
dir <- getwd()

# Load list of places
places <- read_csv(paste0(dir,'/places.csv')) %>% 
  mutate(full = paste(city, state, country, sep = ', ')) %>% 
  geocode(full, method = 'osm', lat = latitude, long = longitude)

# Create a pixel grid
lat <- tibble(lat = seq(-90, 90, by = 1))
long <- tibble(long = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(long, all = TRUE)

dots <- dots %>% 
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  dplyr::select(-lakes)

# Create a theme for the plot
color_bk <- '#212121'
theme <- theme_void() +
  theme(panel.background = element_rect(fill=color_bk),
        plot.margin = unit(c(0, 0, 0, 0), 'cm'))



pixel_map <- ggplot() +   
  #base layer of map dots
  geom_point(data = dots, 
             aes(x=long, y = lat), 
             col = 'grey45', 
             size = 0.7) + 
  #plot all the places I've been to
  geom_point(data = places, 
             aes(x=longitude, y=latitude), 
             color='grey80', 
             size=0.8) + 
  #plot all the places I lived in, using red
  geom_point(data = places %>% 
               filter(status == 'lived'), 
             aes(x=longitude, y=latitude), 
             color='red', 
             size=0.8) +
  #an extra layer of halo around the places I lived in
  geom_point(data = places %>% 
               filter(status == 'lived'), 
             aes(x=longitude, y=latitude), 
             color='red', 
             size=3, 
             alpha = 0.4) +
  geom_point(data = places %>% 
               filter(status == 'been'), 
             aes(x=longitude, y=latitude), 
             color='yellow', 
             size=0.8) +
  #adding my theme
  theme

pixel_map
