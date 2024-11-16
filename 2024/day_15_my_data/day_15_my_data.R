## Day 15: My data.
# Hikes in 2024.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(XML)
library(dplyr)
library(stringr)
library(geosphere)
library(rayshader)

gpx_files <- list.files('data', pattern = "\\.gpx$", full.names = TRUE)

hike_list <- list()
for (i in 1:length(gpx_files)){
  gpx_parsed <- htmlTreeParse(file = gpx_files[i], useInternalNodes = TRUE)
  
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
  
  hike_df <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ]),
    elevation = as.numeric(elevation)
  )
  hike_df$index <- as.numeric(row.names(hike_df))
  hike_df$hike_name <- str_replace(gpx_files[i], ".gpx", "")
  hike_df$hike_name <- str_replace(hike_df$hike_name, "data/", "")
  
  # get distance instead of index
  hike_df <- hike_df |>
    mutate(
      dist = c(0, distHaversine(cbind(lon, lat)[-n(), ], cbind(lon, lat)[-1, ])),
      cumulative_dist = cumsum(dist)
    )
  
  hike_df$hike_id <- i
  hike_df$y = i * 10 # used for offsetting hikes in 3d plot
  
  hike_list[[i]] <- hike_df
}
hike_df_all <- do.call(rbind, hike_list)

# create base matrix
nrows <- 1000
ncols <- 1000
base_matrix <- matrix(0, nrow = nrows, ncol = ncols)

# define extent
extent_x <- max(hike_df_all$cumulative_dist)
extent_y <- max(hike_df_all$y)
extent <- c(0, extent_x, 0, extent_y)

# plot flat base
base_matrix |>
  plot_3d(base_matrix, zscale = 1, solid = F, shadow = T, windowsize = c(800, 800),
          background = 'black')

# iterate over hikes in list & plot
for (path_data in hike_list){
  render_path(
    extent = extent,
    lat = path_data$y,
    long = path_data$cumulative_dist,
    altitude = path_data$elevation - min(path_data$elevation),
    color = "#F18805",
    linewidth = 3,
    clear_previous = FALSE
  )
}

render_highquality(filename = 'day_15_light.png',
                   samples = 280,
                   point_material = rayrender::light)

render_movie(filename = 'day_15_orbit.mp4')
