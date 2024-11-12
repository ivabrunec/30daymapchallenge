# Day 12: Time & space.
# shrinking Aral sea

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(terra)
library(sf)
library(rayshader)

tif_files <- list.files("data", pattern = "\\.tif$", full.names = TRUE)
rasters <- lapply(tif_files, rast)

rast_combin <- do.call(mosaic, rasters)
aral_extent <- ext(56.5, 62.5, 42.5, 47)

aral_crop <- crop(rast_combin, aral_extent)
plot(aral_crop)

# this is janky as hell but I think the logic is reasonable
aral_crop[aral_crop == 0] <- NA

# Define breaks for deciles
breaks <- seq(0, 100, by = 10)
reverse_values <- seq(100, 10, by = -10)
rcl <- cbind(breaks[-length(breaks)], breaks[-1], reverse_values)

# Apply the classification
aral_deciles_reversed <- classify(aral_crop, rcl = rcl)

# Plot to verify the reversed classification
plot(aral_deciles_reversed)

# Classify into deciles, ignoring the NA areas
#aral_deciles <- classify(aral_crop, rcl = cbind(breaks[-length(breaks)], breaks[-1]))

# Plot the result
#plot(aral_deciles)

aral_deciles_filled <- ifel(is.na(aral_deciles_reversed), 110, aral_deciles_reversed)
plot(aral_deciles_filled)

aral_raster <- raster::raster(aral_deciles_filled)
plot(aral_raster)
aral_agg <- aggregate(aral_raster, fact = 10)
plot(aral_agg)

raster_mat <- raster_to_matrix(aral_agg)

write.csv(raster_mat, 'raster_mat.csv')

col_pal <- scico::scico(10, palette = 'oslo')

raster_mat |>
  height_shade(texture = col_pal) |>
  plot_3d(raster_mat, zscale = 1, windowsize = c(1000, 800), solid = F)

render_camera(theta = 0, phi = 90)

render_highquality(filename = 'day_12_time_space.png',
                   samples = 365, 
                   ground_material = rayrender::diffuse(color = 'white'))