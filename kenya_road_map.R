## Kenya road map

library(rgdal)
if (!require(geojsonio)) {
  install.packages("geojsonio")
  library(geojsonio)
}
library(sp)
library(maps)
library(ggmap)
library(maptools)