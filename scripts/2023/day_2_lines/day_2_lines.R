## Day 2: Lines. TTC.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(rayshader)
library(rayrender)

ttc <- st_read('ttc-subway-shapefile-wgs84/TTC_SUBWAY_LINES_WGS84.shp')

toronto <- data.frame(x = c(-79.63285790573701, -79.1574169150847), 
                       y = c(43.54887320613665, 43.95695330258044))

elev <- elevatr::get_elev_raster(toronto, z = 10, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")

elmat <- raster_to_matrix(elev) 

elmat %>%
  sphere_shade(texture = 'imhof3',
               sunangle = 270) %>%
  plot_3d(elmat, zscale = 5, theta = 0, phi = 30, zoom = .6, 
          windowsize = c(1200, 800), background = '#f3c2b9', solid = F, shadow = T,
          solidcolor = 'grey30', alpha = .2)

colors <- c('#FFCB0C', '#16A753', '#1F99D5', '#B32078')
for (i in 1:nrow(ttc)){
  render_path(ttc$geometry[i], extent = raster::extent(toronto), heightmap = elmat, color=colors[i],
              linewidth = 2, zscale=100, offset = 4500, clear_previous = F)
}

render_highquality(
                   filename = 'day_2_toronto_ttc_light_5.png',
                   clamp_value=10,
                   samples = 1200,
                   sample_method = 'sobol_blue',
                   ambient_light = T,
                   backgroundhigh = "#231f66", backgroundlow = "#4b45a1",
                   #backgroundhigh = '#231f66', backgroundlow = "#c95a0a",
                   #light = T,
                   #lightintensity = 20,
                   width = 1200, height = 800,
                   path_material = light)

