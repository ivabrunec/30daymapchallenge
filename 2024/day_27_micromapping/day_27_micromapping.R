## Day 27: Micromapping.
# some section of Toronto (if I can get this to work)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(terra)
library(sf)
library(ggplot2)
library(rayshader)

lidar_files <- list.files("data/GTA-2015-DSM-03/", pattern = "\\.img$", full.names = TRUE)

lidar_list <- lapply(lidar_files, rast)

crs_list <- lapply(lidar_list, crs)
print(unique(crs_list)) 

mosaic_raster <- do.call(mosaic, c(lidar_list, list(fun = "mean")))  # Use "mean" to handle overlaps
plot(mosaic_raster)

# clip just the section we want to map
bbox_wgs84 <- st_bbox(c(xmin = -79.48137308428446, ymin = 43.63012559968974,
                        xmax = -79.46437287212731, ymax = 43.64849822337122),
                      crs = st_crs("EPSG:4326"))

# even smaller?
bbox_wgs84 <- st_bbox(c(xmin = -79.4806806553399, ymin = 43.64332769368048,
                        xmax = -79.46351451820885, ymax = 43.65500350010827),
                      crs = st_crs("EPSG:4326"))

# reproject the bounding box
# idk why simpler approaches didn't work
bbox_matrix <- matrix(
  c(bbox_wgs84["xmin"], bbox_wgs84["ymin"],
    bbox_wgs84["xmax"], bbox_wgs84["ymin"],
    bbox_wgs84["xmax"], bbox_wgs84["ymax"],
    bbox_wgs84["xmin"], bbox_wgs84["ymax"],
    bbox_wgs84["xmin"], bbox_wgs84["ymin"]), 
  ncol = 2, byrow = TRUE
)

bbox_poly <- vect(bbox_matrix, type = "polygons", crs = "EPSG:4326")
bbox_utm <- project(bbox_poly, crs(mosaic_raster))

cropped_raster <- crop(mosaic_raster, bbox_utm)

plot(cropped_raster)
writeRaster(cropped_raster, "cropped_raster.tif", overwrite = TRUE)

## rayshader:
cropped_raster_raster <- raster::raster(cropped_raster)

elmat <- raster_to_matrix(cropped_raster_raster)

elmat |>
  sphere_shade() |>
  plot_3d(elmat, windowsize = c(800, 800))

render_highquality('test_file.png')
