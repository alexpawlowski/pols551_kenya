## Kenya road map
source('theme_fig.R')
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(geojson)
library(geojsonio)
library(sf)


# raster ----
pop_density <- raster('pop_density/KEN15adjv6.tif')

pop_density@crs

#http://spatialreference.org/ref/sr-org/7643/proj4/
sr <- "+proj=utm +zone=37 +ellps=WGS84 +units=m +no_defs"
pop_density_pr <- projectRaster(pop_density, crs = sr)

### Raster Pixel Values

hist(pop_density, main="Distribution of population density values", 
     col= "purple", 
     maxpixels=100000)

# # add a color map with 5 colors
# col=terrain.colors(5)
# breaks = c(100, 200, 300, 400, 600, 800)
# 
# plot(pop_density_pr, #col=col, breaks=breaks,
#      main="2015 estimated population density") 

map.p <- rasterToPoints(pop_density)

colnames(map.p) <- c('lat', 'long', 'population_density')
df <- as_data_frame(map.p)

pretty_breaks <- c(10, 100, 200, 300, 600, 1000)

# find the extremes
minVal <- min(df$population_density, na.rm = T)  
maxVal <- max(df$population_density, na.rm = T)  
# compute labels
labels <- c()  
brks <- c(minVal, pretty_breaks, maxVal)  
# round the labels (actually, only the extremes)
for(idx in 1:length(brks)){  
  labels <- c(labels,round(brks[idx + 1], 2))
}

labels <- labels[1:length(labels)-1]  
# define a new variable on the data with the breaks
df$brks <- cut(df$population_density,  
                           breaks = brks, 
                           include.lowest = TRUE, 
                           labels = labels)

brks_scale <- levels(df$brks)  
labels_scale <- rev(brks_scale)

## outline_geometry ----
kenya_country_outline <- st_read("kenya_shapes/admin_level_2.geojson", quiet = TRUE,
                                 stringsAsFactors = FALSE)
kenya_outline <- st_read("kenya_shapes/admin_level_4.geojson", quiet = TRUE,
                         stringsAsFactors = FALSE)
  #geojsonio::geojson_read('kenya_shapes/admin_level_4.geojson', what = 'sp')
ke_out_correct_missing <- data_frame(name = c(
  'Kisii', 'Kisumu', 'Bungoma', 'Turkana', 'Murang`a'
),
COUNTY_COD = as.character(c(45, 42, 39, 23 ,21))
)

kenya_outline$COUNTY_COD[is.na(kenya_outline$COUNTY_COD)] <- ke_out_correct_missing$COUNTY_COD[ke_out_correct_missing$name %in% kenya_outline$name]
  

plot(kenya_outline)

## roads ----
roads <- st_read('kenya_shapes/ke_major-roads.shp') %>%
  st_transform(., 4326)
water_bodies <- st_read('kenya_shapes/ke_waterbodies.shp') 
st_crs(water_bodies) <- "+proj=longlat +datum=WGS84"

urban <- st_read('kenya_shapes/ke_urban.shp')
protected_areas <- st_read('kenya_shapes/ke_protected-areas.shp')%>%
  st_transform(., 4326)

## area data -----
url = 'https://en.wikipedia.org/wiki/Counties_of_Kenya'
xpath = '//*[@id="mw-content-text"]/div/table[3]'
kenya_area <- url %>%
  read_html() %>%
  html_nodes(xpath = xpath) %>%
  html_table(header = TRUE) %>%
  first() %>%
  mutate(County_Code = paste0('KE',sprintf("%02d", Code)))

kenya_area <- kenya_area %>%
  select(County_Code, County, `Area (km2)`) %>%
  rename(Area = `Area (km2)`) %>%
  mutate(Area = as.numeric(gsub(",","",Area)))

kenya_area <- kenya_area[-48,] #remove total
  
#read_csv('pop_density/2010_pop_density.csv')
kenya_pop  <- read_csv('pop_density/popl_distr_2009_2018_UNICEF.csv')
kenya_pop <- kenya_pop %>%
  gather('Year', 'Population', `2009`:`2019`, convert = TRUE)

kenya_pop_density <- kenya_pop %>%
  filter(Year == 2017) %>%
  select(Name, County_code, Year, Population) %>%
  left_join(., kenya_area, by = c("County_code" = "County_Code")) %>%
  mutate(Pop_Density = Population / Area) %>%
  select(-County)


kenya_counties <- kenya_outline %>%
  mutate(COUNTY_COD = paste0('KE',sprintf("%02d", as.numeric(COUNTY_COD)))) %>%
  left_join(., kenya_pop_density, by = c("COUNTY_COD" = "County_code"))



pop_density_plot <- kenya_counties %>%
  ggplot() +
  geom_sf(aes(color = Pop_Density ,fill = Pop_Density)) +#,
         #color = '#58595b') +
  geom_sf(data = water_bodies, fill = 'white', color = NA) +
  geom_sf(data = protected_areas, fill = '#d9f0d3', color = NA) +
  geom_sf(data = roads, color = '#1b7837') +
  geom_sf(data = urban, color = NA, fill = '#58595b', alpha = 0.8) +
  geom_sf(data = kenya_country_outline, fill = NA, color = 'black') +
  #coord_sf(crs = "+proj=aea +lat_1=2.5 +lat_2=-2.55 +lat_0=0 +lon_0=38 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +no_defs") + #+units=m
  scale_fill_distiller(name = 'Population Density (ppl / km^2)',
                       palette = 'BuPu',
                       direction = 1,
                       trans = 'log10') +
  scale_color_distiller(name = 'Population Density (ppl / km^2)',
                       palette = 'BuPu',
                       direction = 1,
                       trans = 'log10') +
  theme_fig() +
  theme(
    legend.key.size = unit(1,'in')
  )

pop_density_plot

ggsave(pop_density_plot, filename = 'kenya_pop_density_roads.svg',
       width = 9, height = 10.5, units = 'in')
