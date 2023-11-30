## Day 23: 3D.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(dplyr)
library(elevatr)
library(ggplot2)
library(showtext)

# great barrier reef data from: https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/115066
# this is panel A
# aggregated in QGIS, original file is ~1gb

gbr <- raster('GreatBarrierReef_resampled.tif')

gbr_df <- rasterToPoints(gbr) |>
  as.data.frame()
colnames(gbr_df) <- c('x','y','z')

aus_geom <-
  giscoR::gisco_get_countries(resolution = "20", country = "Australia")

# plot in rayshader
library(rayshader)
elmat <- raster_to_matrix(gbr) 


elmat %>%
  sphere_shade(texture='bw') %>%
  plot_3d(elmat, zscale = 20, theta = 0, phi = 30, zoom = .6, 
          windowsize = c(1200,600), 
          background = '#Eaeae0', solid = T, shadow = T,
          solidcolor = '#F4f4e6', solidlinecolor = "grey30",
          raytrace=T)

water_init = 0
for(i in 1:360) {
  render_water(elmat,zscale=20, 
               waterdepth = water_init - (i*10),
               watercolor = '#59af95',
               wateralpha = .4
  )
  render_camera(theta=135-i,phi=30,zoom=0.7, fov=40)
  #render_highquality(ground_material = rayrender::metal(color = '#6094e0'),
  #                   width=800,height=800, sample_method="sobol_blue",
  #                   filename=sprintf("temp%i.png",i),verbose=T)
  render_snapshot(filename = sprintf("gbr_%i.png",i))
  
}

av::av_encode_video(glue::glue("gbr_{1:360}.png"), framerate=30, output = "gbr_drain.mp4")
file.remove(glue::glue("gbr_{1:360}.png"))


render_highquality(ground_material = rayrender::metal(color = '#6094e0'),
                   width=800,height=800, sample_method="sobol_blue",
                   filename=sprintf("temp.png",i),verbose=T)
