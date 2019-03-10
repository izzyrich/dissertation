# Script to filter important aspects of LUCAS 2012 dataset for Latvia
# to only include forest land (other in classification) 
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

# import dataset ----
lucas <- read_csv("data/2012_lucas.csv")

# filter for forestry ----
lucas_filtered <- lucas %>% 
  dplyr::filter(LU1 == "U120") %>% # U120 - forestry land use 
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# change column names
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# write to csv ----
write.csv(lucas_filtered, file = "data/lucas_2012_filtered_forestry.csv")

# set CRS and transform ----
coordinates(lucas_filtered) <- c("LONG", "LAT")
proj4string(lucas_filtered) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points <- spTransform(lucas_filtered, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")

# add extent (boundary of Latvia) ----
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

# save border of Latvia as a shapefile 
shapefile(map, 'data/latvia_border.shp')

# set extent ----
points@bbox <- as.matrix(extent(r_border))

# write to shapefile
shapefile(points, 'data/2012_lucas_forestry.shp')
