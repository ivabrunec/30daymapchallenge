# Day 13: new tool
# deck gl in R (rdeck + overture buildings)
# tutorial: https://walker-data.com/posts/overture-buildings/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(arrow)
#install_arrow()
library(sf)
library(dplyr)
library(tigris)
library(rdeck) # pak::pak("rdeck")
options(tigris_use_cache = TRUE)

# updated to most (?) recent release
# here: https://docs.overturemaps.org/release/2024-09-18.0/
buildings <- open_dataset('s3://overturemaps-us-west-2/release/2024-09-18.0/theme=buildings?region=us-west-2')
nrow(buildings)

ny_bbox <- counties(state = "NY", cb = TRUE, resolution = "20m") |> 
  filter(NAME == "New York") |> 
  st_bbox() |> 
  as.vector()

Sys.setenv(MAPBOX_ACCESS_TOKEN = "your_mapbox_token_here")

ny_buildings <- buildings |>
  filter(bbox$xmin > ny_bbox[1],
         bbox$ymin > ny_bbox[2],
         bbox$xmax < ny_bbox[3],
         bbox$ymax < ny_bbox[4]) |>
  select(id, geometry, height) |> 
  collect() |>
  st_as_sf(crs = 4326) |> 
  mutate(height = ifelse(is.na(height), 8, height))

# define color
col_pal <- scico::scico(100, palette = 'hawaii')

nyc_map <- rdeck(map_style = mapbox_dark(), 
      initial_view_state = view_state(
        center = c(-74.006, 40.7128), 
        zoom = 11.3,
        pitch = 30
      )) |> 
  add_polygon_layer(
    data = ny_buildings, 
    name = "NYC",
    get_polygon = geometry, 
    get_elevation = height, 
    get_fill_color = scale_color_linear(
      col = height,
      palette = col_pal
    ),
    extruded = TRUE, 
    opacity = 0.5)

htmlwidgets::saveWidget(nyc_map, "NYC_Building_Map.html", selfcontained = TRUE)
