## Day 28: The Earth is not flat.

# plotting Shenandoah (?)

library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(dplyr)

# coordinates
shenandoah <- data.frame(x = c(-78.6823, -78.1622), y = c(38.3148, 38.8761)) 

elev <- get_elev_raster(shenandoah, z = 11, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


elmat <- raster_to_matrix(elev) 

elmat %>%
  sphere_shade(texture = create_texture('#E87620', '#000000', '#103D05','#7D462B',  '#8CA9BE')) %>%
  plot_3d(elmat, zscale = 6, theta = 0, phi=45, zoom = .56, windowsize = c(1200, 800), background = '#ffffff', solid = T)

render_snapshot('day28_flatearth', clear = F)
