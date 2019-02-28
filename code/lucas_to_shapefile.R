# Script to filter important aspects of LUCAS 2012 dataset for Latvia
# to only include abandoned land 
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

# filter for abandoned land ----
U112_options <- c("D10", "D20") # stated that these were abandoned agricultural areas
U410_options <- c("B", "C", "D", "E", "F") # classes that could be agriculture in U410 (natural terrestrial areas)

lucas_filtered <- lucas %>% 
  dplyr::filter(LU1 == "U112" & "LC1" %in% U112_options | 
                  LU1 == "U410" & "LC1" %in% U410_options |
                 LU1 == "U420" & LC1 == "E30") %>%
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# may need to change column names to change to shapefile
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# write to csv ----
write.csv(lucas_filtered, file = "data/lucas_2012_filtered.csv")

# most successful attempt - I thought this worked but the CRS didn't seem to work
coordinates(lucas_filtered) <- c("LONG", "LAT")
this_crs <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
sp_file <- st_as_sf(lucas_filtered, coords = c("LAT", "LONG"), crs = this_crs)

# write to shapefile 
st_write(sp_file,
         "data/lucas_2012.shp", driver = "ESRI Shapefile")

# add extent (boundary of Latvia) ----
latvia <- raster::getData("GADM", country = "LVA", level = 0)

# format data
map <- fortify(latvia)%>%
  dplyr::select(long, lat) 
colnames(map)[colnames(map) == "lat"] <- "LAT"
colnames(map)[colnames(map) == "long"] <- "LONG"

# set coordinates 
coordinates(map) <- c("LONG", "LAT")

# spatial object (may not be necessary)
sp_border_file <- st_as_sf(map, coords = c("LAT", "LONG"), crs = this_crs)

# check if it worked - it did!
ggplot() +
  geom_sf(data = sp_file) +
  ggtitle("Map of Plot Locations")

ggplot() +
  geom_sf(data = sp_file) +
  geom_sf(data = sp_border_file) +
  ggtitle("Boundary plot")

# but that doesn't fully set the extent --> change to raster?
proj4string(lucas_filtered) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
points <- spTransform(lucas_filtered, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
r <- raster(points) # points as raster

proj4string(map) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
border_points <- spTransform(map, "+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:3857")
r_border <- raster(border_points) # border as raster

# set extent 
setExtent(r, r_border, keepres = FALSE, snap = FALSE)
shapefile(r, 'file.shp') # doesn't work?

# convert raster back to spatial points dataframe - but still doesn't write to shapefile 
new_r <- rasterToPoints(r, fun = NULL, spatial = TRUE)
str(new_r)
class(new_r)
writeOGR(new_r, "data/lucas_2012_extent.shp", driver = "ESRI Shapefile")
st_write(new_r,
         "data/lucas_2012_extent.shp", driver = "ESRI Shapefile")
