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
library(maptools)
library(tiff)
library(dggridR)
library(modeest)

# Import base dataset 
lucas <- read_csv("data/2012_lucas.csv")

# LATVIA BORDER and POLY ----

# get data 
latvia <- raster::getData("GADM", country = "LVA", level = 0)

# change projection
latvia <- spTransform(latvia, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# create polygon
data = data.frame(f=99.9)
spdf = SpatialPolygonsDataFrame(latvia, data)
shapefile(spdf, "data/latvia_poly", overwrite = TRUE)

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
  mutate(class = "1") %>%
  mutate(name = "abandoned")

# write to csv 
write.csv(lucas_filtered, file = "data/lucas_2012_filtered.csv")

# change class
lucas_filtered$class <- as.numeric(lucas_filtered$class)

# change column names
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered) <- c("LONG", "LAT")
proj4string(lucas_filtered) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered <- spTransform(lucas_filtered, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered, 'data/2012_lucas_abandoned.shp', overwrite = TRUE)

## FORESTRY ----

# filter for forestry 
lucas_forestry <- lucas %>% 
  dplyr::filter(LU1 == "U120") %>% # U120 - forestry land use 
  dplyr::select(GPS_LAT, GPS_LONG) %>% # potentially need elevation here??
  mutate(class = "0") %>%
  mutate(name = "forestry")

# write to csv 
write.csv(lucas_forestry, file = "data/lucas_2012_filtered_forestry.csv")

# change class
lucas_forestry$class <- as.numeric(lucas_forestry$class)

# change column names
colnames(lucas_forestry)[colnames(lucas_forestry) == "GPS_LAT"] <- "LAT"
colnames(lucas_forestry)[colnames(lucas_forestry) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_forestry) <- c("LONG", "LAT")
proj4string(lucas_forestry) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_forestry <- spTransform(lucas_forestry, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_forestry@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_forestry, 'data/2012_lucas_forestry.shp', overwrite = TRUE)

## EXTENSIVE LAND ----

# filter for extensive land 
U113_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U113 (natural terrestrial areas)

lucas_filtered_extensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U113" & class %in% U113_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%  # potentially need elevation here??
  mutate(class = "2") %>%
  mutate(name = "extensive")

# write to csv 
write.csv(lucas_filtered_extensive, file = "data/lucas_2012_filtered_extensive.csv")

# change class
lucas_filtered_extensive$class <- as.numeric(lucas_filtered_extensive$class)

# change column names
colnames(lucas_filtered_extensive)[colnames(lucas_filtered_extensive) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_extensive)[colnames(lucas_filtered_extensive) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered_extensive) <- c("LONG", "LAT")
proj4string(lucas_filtered_extensive) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered_extensive <- spTransform(lucas_filtered_extensive, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered_extensive@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered_extensive, 'data/2012_lucas_extensive.shp', overwrite = TRUE)


## INTENSIVE LAND ----

# filter for intensive land
U111_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U111 (natural terrestrial areas)

lucas_filtered_intensive <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(LU1 == "U111" & class %in% U111_options) %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "3") %>%
  mutate(name = "intensive")

# write to csv 
write.csv(lucas_filtered_intensive, file = "data/lucas_2012_filtered_intensive.csv")

# change class
lucas_filtered_intensive$class <- as.numeric(lucas_filtered_intensive$class)

# change column names
colnames(lucas_filtered_intensive)[colnames(lucas_filtered_intensive) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_intensive)[colnames(lucas_filtered_intensive) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered_intensive) <- c("LONG", "LAT")
proj4string(lucas_filtered_intensive) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered_intensive <- spTransform(lucas_filtered_intensive, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered_intensive@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered_intensive, 'data/2012_lucas_intensive.shp', overwrite = TRUE)

## WATER ----

# filter for water
lucas_filtered_water <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "G") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "4") %>%
  mutate(name = "water")

# write to csv 
write.csv(lucas_filtered_water, file = "data/lucas_2012_filtered_water.csv")

# change class
lucas_filtered_water$class <- as.numeric(lucas_filtered_water$class)

# change column names
colnames(lucas_filtered_water)[colnames(lucas_filtered_water) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_water)[colnames(lucas_filtered_water) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered_water) <- c("LONG", "LAT")
proj4string(lucas_filtered_water) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered_water <- spTransform(lucas_filtered_water, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered_water@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered_water, 'data/2012_lucas_water.shp', overwrite = TRUE)

## WETLANDS ----

# filter for wetlands
lucas_filtered_wetlands <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "H") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "5") %>%
  mutate(name = "wetlands")

# write to csv 
write.csv(lucas_filtered_wetlands, file = "data/lucas_2012_filtered_wetlands.csv")

# change class
lucas_filtered_wetlands$class <- as.numeric(lucas_filtered_wetlands$class)

# change column names
colnames(lucas_filtered_wetlands)[colnames(lucas_filtered_wetlands) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_wetlands)[colnames(lucas_filtered_wetlands) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered_wetlands) <- c("LONG", "LAT")
proj4string(lucas_filtered_wetlands) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered_wetlands <- spTransform(lucas_filtered_wetlands, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered_wetlands@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered_wetlands, 'data/2012_lucas_wetlands.shp', overwrite = TRUE)

## ARTIFICIAL LAND ----

# filter for artificial land
lucas_filtered_artificial <- lucas %>% 
  separate(LC1, into = c('class', 'number'), sep = 1) %>%
  dplyr::filter(class == "A") %>%
  dplyr::select(GPS_LAT, GPS_LONG) %>%
  mutate(class = "6") %>%
  mutate(name = "artificial")

# write to csv 
write.csv(lucas_filtered_artificial, file = "data/lucas_2012_filtered_artificial.csv")

# change class
lucas_filtered_artificial$class <- as.numeric(lucas_filtered_artificial$class)

# change column names
colnames(lucas_filtered_artificial)[colnames(lucas_filtered_artificial) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered_artificial)[colnames(lucas_filtered_artificial) == "GPS_LONG"] <- "LONG"

# set CRS and transform 
coordinates(lucas_filtered_artificial) <- c("LONG", "LAT")
proj4string(lucas_filtered_artificial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points_lucas_filtered_artificial <- spTransform(lucas_filtered_artificial, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# set extent
points_lucas_filtered_artificial@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points_lucas_filtered_artificial, 'data/2012_lucas_artificial.shp', overwrite = TRUE)

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