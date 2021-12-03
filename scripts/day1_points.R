## Day 1: Points
# A map with points. 
# Philadelphia base map downloaded from: https://www.pasda.psu.edu/uci/DataSummary.aspx?dataset=7102 <br>
# Philadelphia stree tree inventory downloaded from: https://www.opendataphilly.org/dataset/philadelphia-street-tree-inventory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(sf)
library(ggplot2)
library(dplyr)

phl_base <- read_sf("data/PhiladelphiaStreetCenterlines2020.shp") %>% 
  select(geometry) # only geometry

phl_trees <- st_read("data/69b39f50-460b-4b7b-b5ad-d8d9c491e4392020328-1-1vb911p.sd1c.shp")

# plot 
ggplot() +
  geom_sf(data = phl_base$geometry, color = 'darkgrey', size = .25) +
  geom_sf(data = phl_trees, size=.05, color='#235347') +
  coord_sf() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave('day1_phl_trees.png', width = 4, height = 6, dpi = 300)
