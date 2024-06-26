## Buffering a GeoJSON file in R

# read geojson file and use tmap package to visualize it
library(geojsonio)
library(tmap)
library(tmaptools)
library(sf)

# Specify the path to your GeoJSON file
geojson_file <- "/Users/sousekilyu/Documents/GitHub/PekingCollege/data/北京普通高等学校（分校区）.geojson"

# Read the GeoJSON file and transfer its prj to 4326
data <- st_read(geojson_file)
data <- st_transform(data, crs = 4326)

# Set tmap options to check and fix geometry
tmap_options(check.and.fix = TRUE)

# Visualize the data
qtm(data)

# Install and load the lwgeom package
if (!require(lwgeom)) {
  install.packages("lwgeom")
}
library(lwgeom)

# Make the data valid
data <- st_make_valid(data)

# Now try to create the buffer again
data_buffer <- st_buffer(data, dist = 1000)

# Visualize the data_buffer
qtm(data_buffer)

## Read data of Beijing reaction facilities

# read Beijing poi csv data and transfer it to sf data
beijing_poi <- read.csv("/Users/sousekilyu/Documents/Meta Data/高德poi/2022高德poi/北京POI数据/北京市POI数据.csv")
# rename col names
colnames(beijing_poi) <- c("name", 
                           "type1", 
                           "type2", 
                           "longitude", 
                           "latitude", 
                           "province", 
                           "city", 
                           "district")
beijing_poi_sf <- st_as_sf(beijing_poi, coords = c("longitude", 
                                                   "latitude"), crs = 4326)

