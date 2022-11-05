## Day 5: Ukraine.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(raster)
library(sf)
library(dplyr)
library(rayshader)
library(giscoR)
library(elevatr)

ukr_raster <- gisco_get_countries(country = 'Ukraine',epsg = "3035") |>
  get_elev_raster(z = 5, clip = 'location')

ukr_mat <- raster_to_matrix(ukr_raster)  

# light, shadow, left, right, center (flat areas) 

ukr_mat |>
  ##sphere_shade(texture = create_texture('#723d46', '#472d30', '#e26d5c','#ffe1a8',  '#c9cba3'), sunangle = 90) |>
  sphere_shade(texture = create_texture('#723d46', '#695958', '#6D7789','#cfffe5',  '#A2C6AD'), sunangle = 90) |>
  #add_shadow(ray_shade(ukr_mat,sunaltitude=30, sunangle = 0),max_darken=0.65) |>
  plot_3d(ukr_mat, zscale = 25, windowsize = (c(1000,800)),
          theta = 12, phi = 60, zoom = .8)

render_highquality(clear = TRUE, lightintensity = 600, samples = 360, filename='data/day5_ukraine.png')

# read in image we just created to add annotations with magick
library(magick)
library(showtext)

font_add_google(name = 'Montserrat', family = 'Montserrat')
showtext_auto()

ukr_img <- image_read('data/day5_ukraine.png')

# this works on mac but not windows 
ukr_img2 <- image_annotate(ukr_img, "Україна", font = 'Montserrat', size = 80, location = "+850+950", color = 'darkgrey', strokecolor = '#723d46')

# ended up not using this
image_write(ukr_img2, 'data/day5_ukraine.png')

