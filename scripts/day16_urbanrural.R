## Day 16: Urban/Rural.

# plotting urban centers in Iceland
# from https://www.oecd.org/regional/regional-statistics/functional-urban-areas.html
# population data: https://data.humdata.org/dataset/iceland-high-resolution-population-density-maps-demographic-estimates
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(sf)
library(ggplot2)
library(raster)
library(LaCroixColoR)
library(mapdata)

iceland_urb <- read_sf("data/ISL_core.shp")
world <- map_data('world')
iceland_base <- filter(world, region == 'Iceland')

pop_density <- raster("data/population_isl_2019-07-01.tif")
pop_up <- aggregate(pop_density, fact = 10) # aggregate output
pop_up2 <- aggregate(pop_up, fact = 10) # aggregate output
pop_density_mat <- as.data.frame(rasterToPoints(pop_up2))

# wanted to also plot elevation but it makes things entirely too laggy
#elevation <- raster("data/IslandsDEMv1.0_50x50m_isn2016_shade.tif")
## DOWNSAMPLE

#el_up <- aggregate(elevation, fact = 10) # aggregate output
#el_up_reproject <- projectRaster(el_up, crs = crs(iceland_urb))
#elmat <- as.data.frame(rasterToPoints(el_up_reproject)) 

## 2d tile plot

isl_plot <- ggplot() +
  geom_polygon(data = iceland_base, aes(x = long, y = lat), fill="#132157") +
  geom_tile(data = pop_density_mat,
            aes(x = x, y = y, fill = population_isl_2019.07.01)) +
  geom_sf(data = iceland_urb$geometry, color = 'white', fill=NA, alpha = 1, size=.7) +
  scale_x_continuous("X") +
  scale_y_continuous("Y") +
  scale_fill_gradientn("Z", colors=rev(lacroix_palette(name = "Apricot", n = 256))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.key.height = unit(.2, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=36),# this looks absurd in the plot viewer but fine when saved!
        legend.background=element_blank(),
        plot.margin = unit(c(t=.6,r=.8,b=.6,l=0.8), "cm"),
        plot.background=element_rect(fill = '#f7cfd1', color=NA),
        panel.background = element_rect(fill = '#f7cfd1', color=NA)) 

isl_plot

ggsave('day16.png', height = 5, width = 7, dpi= 500)



## if we want to 3d plot:  ####
# have to omit the 'fill' argument for polygon.

# specify new color palette that reverses another one
flipped_palette <- rev(lacroix_palette('PeachPear', n = 256))

isl_plot <- ggplot() +
  geom_polygon(data = iceland_base, aes(x = long, y = lat), fill=NA, color = 'grey', size = 1) +
  geom_sf(data = iceland_urb$geometry, color = 'white', alpha = 1, size=1.3) +
  geom_tile(data = pop_density_mat,
              aes(x = x, y = y, fill = population_isl_2019.07.01)) +
  scale_x_continuous("X") +
  scale_y_continuous("Y") +
  scale_fill_gradientn("Z", colors=flipped_palette) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="",
        plot.background=element_rect(fill = "#132157", color=NA),
        panel.background = element_rect(fill = "#132157", color=NA)) 

isl_plot


library(rayshader)

plot_gg(isl_plot, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
