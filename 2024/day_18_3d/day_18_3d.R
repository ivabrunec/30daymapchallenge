# Day 18: 3D
# Milford Sound, New Zealand
# data: https://www.linz.govt.nz/products-services/maps/new-zealand-topographic-maps/topo50-map-chooser/topo50-map-ca08-milford-sound-piopiotahi
# following same approach as day 7 - vintage; 
# following steps from https://michaelpaulschramm.com/posts/2020-10-08-rayshading_maps/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(elevatr)
library(rayshader)

map_tif <- raster::brick('data/CA08_GeoTifv1-02.tif')
# dataset too large for github
topo_map <- raster::stack(map_tif)
plot(topo_map)

map_extent <- extent(topo_map)
bbox <- as(map_extent, "SpatialPolygons")
crs(bbox) <- crs(topo_map)

elevation <- get_elev_raster(bbox, z = 12)
elevation <- raster::crop(elevation, extent(topo_map))
plot(elevation)

# now convert the raw file to a 3 channel rgb array
# this takes a second, but it runs
names(topo_map) <- c("r", "g", "b")
topo_r <- rayshader::raster_to_matrix(topo_map$r)
topo_g <- rayshader::raster_to_matrix(topo_map$g)
topo_b <- rayshader::raster_to_matrix(topo_map$b)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))

topo_rgb_array[,,1] <- topo_r/255
topo_rgb_array[,,2] <- topo_g/255
topo_rgb_array[,,3] <- topo_b/255

# the array needs to be transposed
topo_rgb_array <- aperm(topo_rgb_array, c(2,1,3))

elev_mat <- raster_to_matrix(elevation)

elev_mat %>%
  sphere_shade() |>
  add_overlay(topo_rgb_array) |>
  plot_3d(elev_mat, zscale = 10, windowsize = c(1000, 1200))

render_highquality('day_18_3d_2.png', samples = 365,
                   clamp_value = 5, lightcolor = 'beige')