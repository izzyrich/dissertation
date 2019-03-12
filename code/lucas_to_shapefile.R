# Script to filter important aspects of LUCAS 2012 dataset for Latvia
# Latvia border, abandoned, extensive and intensive land
# Izzy Rich 12/03/19 for dissertation in EES at the University of Edinburgh 
# -------

# load packages ----
library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(sf)
library(rworldmap)
library(grid)
library(rworldxtra)
library(stringr)

# import base dataset ----
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
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# change column names
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# write to csv 
write.csv(lucas_filtered, file = "data/lucas_2012_filtered.csv")

# set CRS and transform 
coordinates(lucas_filtered) <- c("LONG", "LAT")
proj4string(lucas_filtered) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points <- spTransform(lucas_filtered, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points, 'data/2012_lucas_abandoned.shp', overwrite = TRUE)

## FORESTRY ----

# filter for forestry 
lucas_forestry <- lucas %>% 
  dplyr::filter(LU1 == "U120") %>% # U120 - forestry land use 
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# change column names
colnames(lucas_forestry)[colnames(lucas_forestry) == "GPS_LAT"] <- "LAT"
colnames(lucas_forestry)[colnames(lucas_forestry) == "GPS_LONG"] <- "LONG"

# write to csv 
write.csv(lucas_forestry, file = "data/lucas_2012_filtered_forestry.csv")

# set CRS and transform
coordinates(lucas_forestry) <- c("LONG", "LAT")
proj4string(lucas_forestry) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_forestry <- spTransform(lucas_forestry, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_forestry@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_forestry, 'data/2012_lucas_forestry.shp', overwrite = TRUE)

## EXTENSIVE LAND ----

# filter for extensive land 
U113_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U113 (natural terrestrial areas)

lucas_filtered_extensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U113" & class %in% U113_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# change column names
colnames(lucas_filtered_extensive)[colnames(lucas_filtered_extensive) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_extensive)[colnames(lucas_filtered_extensive) == "GPS_LONG"] <- "LONG"

# write to csv 
write.csv(lucas_filtered_extensive, file = "data/lucas_2012_filtered_extensive.csv")

# set CRS and transform 
coordinates(lucas_filtered_extensive) <- c("LONG", "LAT")
proj4string(lucas_filtered_extensive) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_extensive <- spTransform(lucas_filtered_extensive, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_extensive@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_extensive, 'data/2012_lucas_extensive.shp', overwrite = TRUE)

## INTENSIVE LAND ----

# filter for intensive land
U111_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U111 (natural terrestrial areas)

lucas_filtered_intensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U111" & class %in% U111_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# change column names
colnames(lucas_filtered_intensive)[colnames(lucas_filtered_intensive) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_intensive)[colnames(lucas_filtered_intensive) == "GPS_LONG"] <- "LONG"

# write to csv 
write.csv(lucas_filtered_intensive, file = "data/lucas_2012_filtered_intensive.csv")

# set CRS and transform 
coordinates(lucas_filtered_intensive) <- c("LONG", "LAT")
proj4string(lucas_filtered_intensive) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_intensive <- spTransform(lucas_filtered_intensive, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_intensive@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_intensive, 'data/2012_lucas_intensive.shp', overwrite = TRUE)

