## Day 29: NULL.

# Plotting locations on land with elevation closest to 0

# topography: https://neo.gsfc.nasa.gov/view.php?datasetId=SRTM_RAMP2_TOPO

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(sf)
library(raster)

topo_raster <- raster("data/SRTM_RAMP2_TOPO_2000-02-11_rgb_3600x1800.tiff")
topo_matrix <- as.data.frame(rasterToPoints(topo_raster)) %>%
  naniar::replace_with_na(replace = list(SRTM_RAMP2_TOPO_2000.02.11_rgb_3600x1800 = 255))
colnames(topo_matrix) <- c('x','y','topo') 
topo_matrix <- topo_matrix %>%
  mutate(percentile = ntile(topo, 100))
topo_filter <- filter(topo_matrix, percentile == 1)


ggplot() +
  geom_tile(data = topo_filter,  aes(x = x, y = y, fill = topo)) + 
  scale_fill_gradientn(colors=c("#400C2C","#530F39","#563765","#595E91","#6a80ad","#7CA7C8","#75C2E1","#8CE3F4","#9BF7DB"),na.value='grey20')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        plot.background=element_rect(fill = 'grey20', color=NA),
        panel.background = element_rect(fill = 'grey20', color=NA)) 
  

ggsave('day29_null.png', width = 7, height = 4, dpi = 400)
