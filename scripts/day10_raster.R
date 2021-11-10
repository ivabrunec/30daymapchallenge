## Day 10: Elevation.
## map of Olympic peninsula (WA, USA)

## map of phytoplankton.

library(ggplot2)
library(sf)
library(elevatr)
library(raster)
library(dplyr)
library(maps)
library(mapdata)
library(viridis)
#library(rayshader)

## plot phytoplankton
# data from NASA Earth
chlor_raster <- raster("data/RenderData_chlor.tiff")
chlor_matrix <- as.data.frame(rasterToPoints(chlor_raster)) %>%
  naniar::replace_with_na(replace = list(RenderData_chlor = 255))

ggplot() +
  geom_raster(data = chlor_matrix,
              aes(x = x, y = y, fill = RenderData_chlor)) + 
  #scale_fill_gradientn(colors = viridis::inferno(n=50),na.value='black') +
  scale_fill_gradientn(colors=c("#2a0042","#7400b8","#5e60ce","#4ea8de","#56cfe1","#72efdd","#80ffbb"),na.value='grey20')+
  coord_sf(ylim = c(-70, 70), crs = 4326) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        plot.background=element_rect(fill = 'grey20', color=NA),
        panel.background = element_rect(fill = 'grey20', color=NA)) 

ggsave('day10_raster.png',width=7, height=4, dpi=300)

## plot olympic peninsula

wa <- map_data('state') %>%
  filter(region == 'washington')

olympic <- data.frame(x = -124.05823278202054, y = 48.00413778235446) %>%
  st_as_sf(coords = c('x','y'), crs = 4326)

elev <- get_elev_raster(olympic, z = 8)
elev_matrix <- as.data.frame(rasterToPoints(elev))
colnames(elev_matrix) <- c('x','y','z')

elevplot <- ggplot() +
  geom_raster(data = elev_matrix,
              aes(x = x, y = y, fill = z)) + 
  scale_fill_viridis(option = 'viridis') +
  scale_alpha(range =  c(0.15, 0.89), guide = "none") + 
  geom_polygon(data = wa, aes(x = long, y = lat, group=group), fill = NA, color = 'lightgrey', size=.3) +
  coord_sf(xlim = c(-125, -122), ylim = c(47, 49), crs = 4326) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        legend.key.height = unit(.15, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        plot.background=element_rect(fill = 'white', color=NA),
        panel.background = element_rect(fill = 'white', color=NA)) 


# if you want to render it in rayshader
#plot_gg(elevplot,multicore = TRUE,width=5,height=5,scale=150,windowsize=c(1400,866),
#       zoom = 0.55, phi = 30)


