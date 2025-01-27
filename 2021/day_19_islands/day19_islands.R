## Day 19: Islands. 

## plotting the Lofoten islands in Norway

library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(dplyr)


# coordinates
lofoten <- data.frame(x = c(12.74086,13.00367), y = c(67.81806, 67.97274)) 

elev <- get_elev_raster(lofoten, z = 13, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


elmat <- raster_to_matrix(elev) 

elmat %>%
  sphere_shade(texture = create_texture('#F27A7A', '#000000', '#7A472E', '#1A6408', '#aed9e0')) %>%
  #add_shadow(ray_shade(elmat, sunaltitude=20, sunangle = 90)) %>%
  #add_shadow(ambient_shade(elmat)) %>%
  #add_water(detect_water(elmat),color="imhof4") %>% # this works but i didn't like how it looked
  plot_3d(elmat, zscale = 2, theta = 0, phi=60, zoom = .56, windowsize = c(1200, 800), background = '#aed9e0', solid = F, shadow = F)

render_snapshot('day19_islands2', clear = F)
