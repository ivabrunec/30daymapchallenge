## Day 7: Vintage style
# Historic map with rayshader:
# https://ngmdb.usgs.gov/topoview/viewer/#9/42.6808/-76.9707
# seriously incredible resource
# basically following this super helpful tutorial: https://michaelpaulschramm.com/posts/2020-10-08-rayshading_maps/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(raster)
library(elevatr)
library(rayshader)

# downloaded geotiff directly from website
map_tif <- raster::brick('data/NY_Elmira_138223_1965_250000_geo_tif/NY_Elmira_138223_1965_250000_geo.tif')
topo_map <- raster::stack(map_tif)

map_extent <- extent(topo_map)
bbox <- as(map_extent, "SpatialPolygons")
crs(bbox) <- crs(topo_map)

elevation <- get_elev_raster(bbox, z = 9)
elevation <- raster::crop(elevation, extent(topo_map))
plot(elevation)

# create a baseline elevation (the part of the file outside the map)
base_raster <- elevation * 0 + 20

# specify coordinates you want to crop the map to
# these are the bounds actually printed on the map
x <- c(-78.000, -76.000, -76.000, -78.000) 
y <- c(42.000, 43.000, 42.000, 43.000)
neatline_coords <- cbind(x, y)

# transform to the crs of the map itself
# for some reason it has to be done manually like this instead of just assigning the crs of the map
neatline_points <- SpatialPoints(neatline_coords, proj4string = CRS("+proj=longlat +datum=NAD27"))
projected_neatline_points <- spTransform(neatline_points, CRS("+proj=tmerc +lat_0=42 +lon_0=-77 +k=1 +x_0=0 +y_0=0 +datum=NAD27 +units=m"))
neatline_extent <- extent(projected_neatline_points)

# crop the elevation
interior_elevation <- crop(elevation, neatline_extent)
plot(interior_elevation)

# merge with the outside of the map
elevation <- merge(interior_elevation, base_raster)
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
# i was worried about the extents being wildly different from the elevation matrix but it's fine

elev_mat <- raster_to_matrix(elevation)

elev_mat %>%
  sphere_shade() |>
  add_overlay(topo_rgb_array) |>
  plot_3d(elev_mat, zscale = 10, windowsize = c(1200, 800))

render_highquality('day_7_vintage.png', samples = 365)
