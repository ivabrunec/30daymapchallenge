## Day 26: Islands.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(elevatr)
library(raster)

galapagos_bb <- data.frame(x = c(-92.0235313, -88.8825399),
                           y = c(-1.581648, 0.738314355))

galapagos_elev <- get_elev_raster(galapagos_bb, z = 8, clip = 'bbox',
                                  prj = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

library(rayshader)
galapagos_mat <- raster_to_matrix(galapagos_elev)
galapagos_mat[galapagos_mat < 0] = 0

galapagos_mat |>
  sphere_shade(texture = create_texture('#b28fad', '#000000', '#3d3a05', '#7d462b', '#8cbebd')) |>
  plot_3d(galapagos_mat, zscale = 18, theta = 0, phi = 90, zoom = .6,
          background = 'grey90', solid = T, shadow = T, 
          solidcolor = '#afbdc1', solidlinecolor = '#afbdc1',
          windowsize = c(800, 600))

render_camera(zoom = .50, phi = 90)
render_highquality(filename = 'day26_islands.png', lightdirection = 300, samples = 400)


