## Day 5: Open Street Map data.
# Attempting to read in GPS traces. 
# This mostly failed (only got a few traces per query), so I plotted paths in Shenandoah by elevation.

library(ggplot2)
library(dplyr)
library(sf)

## attempt at downloading gps traces
# u <- "https://api.openstreetmap.org/api/0.6/trackpoints?bbox=-0.15,51.45,-0.12,51.51&page=0"
# download.file(url = u, destfile = "trace.gpx")
# #st_layers("trace.gpx")
# 
# r = st_read("trace.gpx", layer = "tracks")
# #st_geometry_type(r)
# 
# #plot(r$geometry)
# 
# ldn_base <- read_sf("data/statistical-gis-boundaries-london/ESRI/LSOA_2004_London_Low_Resolution.shp") %>% 
#   select(geometry) # only geometry
# 
# #ldn <- read_sf("data/statistical-gis-boundaries-london/ESRI/LSOA_2004_London_Low_Resolution.shp")  
# 
# #ldn_roads <- read_sf("data/OS VectorMap District (ESRI Shape File) TQ/data/TQ_Road.shp")
# 
# ggplot() +
#   geom_sf(data = ldn_base$geometry, color = 'darkgrey', size = .25) +
#   geom_sf(data = r$geometry, color = 'black', size= .5)+
#   theme_minimal() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# let's try downloading shenandoah
library(osmdata)
library(ggmap)
library(elevatr)

q <- opq(bbox = c(-78.6035, 38.3309, -78.1329, 38.9380)) %>%
  add_osm_feature(key = 'highway', value = 'footway') %>%
  #add_osm_feature(key = 'highway', value = 'path') %>%
  osmdata_sf ()

geom <- as.data.frame(q$osm_points$geometry)
# split into two columns
library(tidyr)
geom <- geom %>%
  separate(geometry, into = c('lat', 'lon'), sep=",")

# get lat & long, project elevation data
geom$lat <- sub('.', '', geom$lat)
geom$lat <- sub('.', '', geom$lat)
geom$lon <- gsub('^.|.$', '', geom$lon)

geom$lat <- as.numeric(geom$lat)
geom$lon <- as.numeric(geom$lon)

prj_dd <- "EPSG:4326"
df_elev_epqs <- get_elev_point(geom, prj = prj_dd, src = "epqs")
elev_data <- data.frame(df_elev_epqs)

myLocation <- c(-78.6035, 38.3309, -78.1329, 38.9380)

mad_map <- get_map(location=myLocation, source='stamen', maptype = 'terrain-background', color='bw', force=T)

# plot
ggmap(mad_map,darken = c(0.2, "lightgrey"))+
  geom_point(data = elev_data,
             aes(x = coords.x1, y = coords.x2, color=elevation),
             alpha = .8,
             size = 1.2 )+
  scale_color_gradientn(colors=c("#72efdd", "#7400b8"),name="")+
  labs(x = "", y = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.25, 'cm'))

ggsave('day5_attempt.png', width = 3, height = 5, dpi = 300)