## Day 4: hexagons
# A map with hexagons. 
# Cardinal sightings Oct-Dec 2020. Data from: https://www.gbif.org/occurrence/download/0042864-210914110416597 

library(ggplot2)
library(grid)
library(maps)
library(mapdata)
library(ggmap)
library(mapproj)
library(dplyr)

# dataset too big for github
cardinals <- read.table("data/cardinals_2020.csv", fill=T, sep="\t", quote="", header=T)
cardinals_filtered <- filter(cardinals, decimalLongitude > -125 & decimalLongitude < -50)
usa <- map_data('usa')

ggplot() +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group ), fill= 'grey80') + 
  geom_hex(data = cardinals_filtered, aes(x = decimalLongitude, y = decimalLatitude, fill= stat(log(count))), binwidth = c(.6,.5)) +
  coord_fixed(1.3) +
  scale_fill_gradientn(colors=c("paleturquoise4", "#D5BB9F", "#C42E2A"))+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="")

ggsave('day4_cardinals.png', width = 6, height = 4, dpi = 300)
