## Day 19: Islands. 

## plotting the Lofoten islands in Norway

library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(dplyr)


# coordinates
lofoten <- data.frame(x = c(12.74086,13.00367), y = c(67.81806, 67.97274)) #%>%
  #st_as_sf(coords = c('x','y'), crs = 4326)

elev <- get_elev_raster(lofoten, z = 13, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


elmat <- raster_to_matrix(elev) 

#aed9e0
#b2d6dc
elmat %>%
  sphere_shade(texture = create_texture('#F27A7A', '#000000', '#7A472E', '#1A6408', '#B9D4E5')) %>%
  #add_shadow(ray_shade(elmat, sunaltitude=20, sunangle = 90)) %>%
  #add_shadow(ambient_shade(elmat)) %>%
  #add_water(detect_water(elmat),color="imhof4") %>% # this works but i didn't like how it looked
  plot_3d(elmat, zscale = 2, theta = 0, phi=60, zoom = .56, windowsize = c(1200, 800), background = '#B9D4E5', solid = F, shadow = F)

render_snapshot('day19_islands', clear = F)
