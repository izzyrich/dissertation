# Script to filter important aspects of LUCAS 2009 dataset for Latvia
# to only include abandoned land 
# -------

# load packages ----
library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(sf)

# import dataset ----
lucas <- read_csv("data/2009_lucas.csv")

# filter for abandoned land = U112 "Fallow land" ----
lucas_filtered <- lucas %>% 
  dplyr::filter(LU1 == "U112") %>%
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# may need to change column names to change to shapefile
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# write to csv ----
write.csv(lucas_filtered, file = "data/lucas_2009_filtered")

# write to shp ----

# most successful attempt - I thought this worked but the CRS didn't seem to work
# and it doesn't work when plotting
coordinates(lucas_filtered) <- c("LONG", "LAT")
coords_man <- SpatialPoints(lucas_filtered, 
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
lucas_2009_shapefile <- spTransform(coords_man, CRS("+proj=longlat"))
st_crs(lucas_2009_shapefile)

# check if it worked - it didn't
ggplot() +
  geom_sf(data = lucas_2009_shapefile) +
  ggtitle("Map of Plot Locations")

# other trials
proj4string(lucas_filtered) <- CRS("+proj=longlat +datum=WGS84")
LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
class(lucas_filtered)
writeOGR(lucas_filtered, "lucas_2009_shapefile", driver = "ESRI Shapefile")
locations <- st_as_sf(lucas_filtered, coords = c("GPS_LAT", "GPS_LONG", crs = WGS84))
st_crs(locations)

df.SP <- st_as_sf(lucas_filtered, coords = c("LONG", "LAT"), crs = 4326)
st_crs(df.SP)
df.SP<-st_transform(x = df.SP, crs = 4326)
df.SP$LONG<-st_coordinates(df.SP)[,1]
df.SP$LAT<-st_coordinates(df.SP)[,2]
df.SP<-st_transform(x = df.SP, crs = 4326)
df.SP$LONG<-st_coordinates(df.SP)[,1] 
df.SP$LAT<-st_coordinates(df.SP)[,2] 
writeOGR(df.SP, "lucas_2009_shapefile", driver = "ESRI Shapefile")
