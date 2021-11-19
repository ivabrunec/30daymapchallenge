## Day 17: Land.

# plotting distance to nearest coast
# data: https://oceancolor.gsfc.nasa.gov/docs/distfromcoast/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(sf)
library(ggplot2)
library(raster)

library(rnaturalearth)

#Mapping for coastlines
coast <- ne_coastline(scale = "medium", returnclass = "sf")


dist_data <- read.table("data/dist2coast.signed.txt")
colnames(dist_data) <- c('lon','lat','dist') 

# convert to raster so we can make it lower res
dist_raster <- rasterFromXYZ(dist_data)          
#plot(dist_raster)

# aggregate output
dist_agg <- aggregate(dist_raster, fact = 10) 

dist_matrix <- as.data.frame(rasterToPoints(dist_agg))

# plot
ggplot() +
  geom_raster(data=dist_matrix, aes(x = x, y = y, fill = dist)) +
  scale_fill_gradientn(colors=c('#F35843','#FE8D76','#F5F5DC', '#017875','#015957')) +
  geom_sf(data = coast, fill=NA, color= 'grey30', size=.2, alpha = .6) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position = c(0.5, 0.04),
        legend.direction="horizontal",
        legend.key.height = unit(.2, 'cm'), 
        legend.key.width = unit(.8, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=5, color='white'),
        legend.background=element_blank(),
        plot.margin = unit(c(t=0,r=0,b=0.1,l=0), "cm"),
        plot.background=element_rect(fill = '#FE8D76', color=NA),
        panel.background = element_rect(fill = '#FE8D76', color=NA)) 

ggsave('day17.png', width = 7, height = 4)
