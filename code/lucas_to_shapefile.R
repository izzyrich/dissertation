# Script to filter important aspects of LUCAS 2012 dataset for Latvia
# to only include abandoned land 
# -------

# load packages ----
library(tidyverse)
library(rgdal)
library(sp)
library(raster)
library(sf)

# import dataset ----
lucas <- read_csv("data/2012_lucas.csv")

# filter for abandoned land ----
U112_options <- c("D10", "D20") # stated that these were abandoned agricultural areas
list <- c("B", "D", "E") # classes that could be agriculture in U410 (Cropland, shrubland, grassland)
U410_options <-  paste0("^(", paste(list, collapse = "|"), ")")

lucas_filtered <- lucas %>% 
  dplyr::filter(LU1 == "U112" & "LC1" %in% U112_options | 
                  LU1 == "U410" & str_detect(LC1, U410_options)) %>%
  dplyr::select(GPS_LAT, GPS_LONG) # potentially need elevation here??

# may need to change column names to change to shapefile
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LAT"] <- "LAT"
colnames(lucas_filtered)[colnames(lucas_filtered) == "GPS_LONG"] <- "LONG"

# write to csv ----
write.csv(lucas_filtered, file = "data/lucas_2012_filtered.csv")

# write to shp ----

# most successful attempt - I thought this worked but the CRS didn't seem to work
coordinates(lucas_filtered) <- c("LONG", "LAT")
this_crs <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
sp_file <- st_as_sf(lucas_filtered, coords = c("LAT", "LONG"), crs = this_crs)
st_crs(sp_file) # no reference system still -- will this be an issue??

# check if it worked - it did!
ggplot() +
  geom_sf(data = sp_file) +
  ggtitle("Map of Plot Locations")

# write to shapefile 
st_write(sp_file,
         "data/lucas_2012.shp", driver = "ESRI Shapefile")

# other trials
#coordinates(lucas_filtered) <- c("LONG", "LAT")
#coords_man <- SpatialPoints(lucas_filtered, 
                            proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#lucas_2009_shapefile <- spTransform(coords_man, CRS("+proj=longlat"))
#st_crs(lucas_2009_shapefile
#proj4string(lucas_filtered) <- CRS("+proj=longlat +datum=WGS84")
#LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
#class(lucas_filtered)
#writeOGR(lucas_filtered, "lucas_2009_shapefile", driver = "ESRI Shapefile")
#locations <- st_as_sf(lucas_filtered, coords = c("GPS_LAT", "GPS_LONG", crs = WGS84))
#st_crs(locations)
#df.SP <- st_as_sf(lucas_filtered, coords = c("LONG", "LAT"), crs = 4326)
#st_crs(df.SP)
#df.SP<-st_transform(x = df.SP, crs = 4326)
#df.SP$LONG<-st_coordinates(df.SP)[,1]
#df.SP$LAT<-st_coordinates(df.SP)[,2]
#df.SP<-st_transform(x = df.SP, crs = 4326)
#df.SP$LONG<-st_coordinates(df.SP)[,1] 
#df.SP$LAT<-st_coordinates(df.SP)[,2] 
#writeOGR(df.SP, "lucas_2009_shapefile", driver = "ESRI Shapefile")



