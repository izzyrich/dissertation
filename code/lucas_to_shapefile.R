# Script to filter important aspects of LUCAS 2012 dataset for Latvia
# Latvia border, abandoned, extensive and intensive land
# Izzy Rich 12/03/19 for dissertation in EES at the University of Edinburgh 

# Load packages
library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(sf)
library(rworldmap)
library(grid)
library(rworldxtra)
library(stringr)

# Import base dataset 
lucas <- read_csv("data/2012_lucas.csv")

# LATVIA BORDER ----
latvia <- raster::getData("GADM", country = "LVA", level = 0)

# format data
map <- fortify(latvia)%>%
  dplyr::select(long, lat) 
colnames(map)[colnames(map) == "lat"] <- "LAT"
colnames(map)[colnames(map) == "long"] <- "LONG"

# set CRS and transform
coordinates(map) <- c("LONG", "LAT")
proj4string(map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
border_points <- spTransform(map, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
r_border <- raster(border_points) # border as raster

## ABANDONED LAND ----

# filter for abandoned land
U112_options <- c("10", "20") # stated that these were abandoned agricultural areas
U410_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U410 (natural terrestrial areas)

lucas_filtered <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  filter(LU1 == "U410" & class %in% U410_options |
           LU1 == "U112" & class == "D" & number %in% U112_options |
            LU1 == "U420" & class == "E" & number == "30") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>% # potentially need elevation here??
  mutate(class = "1")

# write to csv 
write.csv(lucas_filtered, file = "data/lucas_2012_filtered.csv")

## FORESTRY ----

# filter for forestry 
lucas_forestry <- lucas %>% 
  dplyr::filter(LU1 == "U120") %>% # U120 - forestry land use 
  dplyr::select(GPS_LAT, GPS_LONG) %>% # potentially need elevation here??
  mutate(class = "0")

# write to csv 
write.csv(lucas_forestry, file = "data/lucas_2012_filtered_forestry.csv")

## EXTENSIVE LAND ----

# filter for extensive land 
U113_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U113 (natural terrestrial areas)

lucas_filtered_extensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U113" & class %in% U113_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%  # potentially need elevation here??
  mutate(class = "2")

# write to csv 
write.csv(lucas_filtered_extensive, file = "data/lucas_2012_filtered_extensive.csv")


## INTENSIVE LAND ----

# filter for intensive land
U111_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U111 (natural terrestrial areas)

lucas_filtered_intensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U111" & class %in% U111_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "3")

# write to csv 
write.csv(lucas_filtered_intensive, file = "data/lucas_2012_filtered_intensive.csv")

## WATER ----

# filter for water
lucas_filtered_water <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "G") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "4")

# write to csv 
write.csv(lucas_filtered_water, file = "data/lucas_2012_filtered_water.csv")

## WETLANDS ----

# filter for wetlands
lucas_filtered_wetlands <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "H") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "5")

# write to csv 
write.csv(lucas_filtered_wetlands, file = "data/lucas_2012_filtered_wetlands.csv")

## ARTIFICIAL LAND ----

# filter for artificial land
lucas_filtered_artificial <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "A") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "6")

# write to csv 
write.csv(lucas_filtered_artificial, file = "data/lucas_2012_filtered_artificial.csv")

## JOIN DATA ----

# join datasets 
total <- rbind(lucas_filtered_intensive, lucas_filtered_extensive, lucas_forestry, 
               lucas_filtered, lucas_filtered_water, lucas_filtered_artificial, 
               lucas_filtered_wetlands)
total$class <- as.numeric(total$class)

# change column names
colnames(total)[colnames(total) == "GPS_LAT"] <- "LAT"
colnames(total)[colnames(total) == "GPS_LONG"] <- "LONG"

# write to csv 
write.csv(total, file = "data/lucas_2012_filtered_total.csv")

# set CRS and transform 
coordinates(total) <- c("LONG", "LAT")
proj4string(total) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_total <- spTransform(total, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_total@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_total, 'data/2012_lucas_total.shp', overwrite = TRUE)


