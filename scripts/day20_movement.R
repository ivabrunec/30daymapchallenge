## Day 20: Movement.
# Where do Slovenians move in the world?

# data from SURS
# definitely some data missing/collapsed (e.g., no separate UK category)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(mapdata)
#library(raster)

world <- map_data("world")
slovenia <- filter(world, region == "Slovenia")

emi_data <- read.table("data/slovenians_emigration.csv", header=T, sep=",")
emi_data <- emi_data %>%
  mutate(tert = ntile(number, 3)) 

ggplot() +
  geom_polygon(data = world, aes(x =long, y = lat, group=group), fill='grey20',color=NA)+
  geom_curve(data = emi_data, aes(x = long1, y = lat1, xend = long2, yend = lat2, color = tert),  arrow = arrow(length = unit(0.01, "npc"))) +
  scale_color_gradientn(colors=c('#118ab2','#ef476f','#fcbf49')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.15, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background=element_blank(),
        legend.text=element_text(color='white',size=8),
        #plot.margin = unit(c(t=4,r=4,b=4,l=4), "cm"),
        plot.background=element_rect(fill = 'black', color=NA),
        panel.background = element_rect(fill = 'black', color=NA))+
  guides(size=FALSE)
