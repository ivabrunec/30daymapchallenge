## Day 2: Lines
# A map with lines. 
# Philadelphia High Injury Network downloaded from: https://www.opendataphilly.org/dataset/high-injury-network <br>
# Philadelphia bike network downloaded from: https://www.opendataphilly.org/dataset/bike-network 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(ggplot2)
library(dplyr)

phl_base <- read_sf("data/PhiladelphiaStreetCenterlines2020.shp") %>% 
  select(geometry) # only geometry

phl_hin <- st_read("data/high_injury_network_2020.shp")

phl_bike <- st_read("data/Bike_Network.shp")

# plot, overlay
ggplot() +
  geom_sf(data = phl_base$geometry, color = 'darkgrey', size = .25, alpha = .3) +
  geom_sf(data = phl_hin, size=.5, color='darkorange2', alpha = .5) +
  geom_sf(data = phl_bike, color = 'cyan4', size = .5, alpha = .5) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave('day2_phl_bike_network.png', height = 5, width = 4)

