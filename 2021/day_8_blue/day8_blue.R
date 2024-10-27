## Day 8: Blue.
# A map of the Alps.

library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(dplyr)
library(magick)

# select specific subsection of the Slovenian Alps
alps <- data.frame(x = 14.599516710974358, y = 46.36015759606476) %>%
  st_as_sf(coords = c('x','y'), crs = 4326)

elev <- get_elev_raster(alps, z = 13)

elmat <- raster_to_matrix(elev) 

filename_map = tempfile()

elmat %>%
  sphere_shade(texture = create_texture("#F2F2F2","#01426F","#61a5c2","#2a6f97","#89c2d9")) %>%
  add_shadow(ray_shade(elmat, sunaltitude=20, sunangle = 90)) %>%
  plot_3d(elmat, zscale=6, fov = 0, theta = 0, zoom = .58, phi = 40, windowsize = c(1200, 800), background = '#DBDBDB', solid = F, shadow = F)

Sys.sleep(0.2)
render_snapshot('day8_alps', clear = F)
