# Day 29: overture 
# tutorial: https://walker-data.com/posts/overture-buildings/
# adjusted to point data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(arrow)
install_arrow()
library(sf)
library(dplyr)
library(tigris)
library(rdeck) # pak::pak("rdeck")
options(tigris_use_cache = TRUE)

london_bbox <- c(-0.5103751, 51.2867602, 0.3340155, 51.6918741)

s3_path <- "s3://overturemaps-us-west-2/release/2024-11-13.0/theme=places/type=place/"
places_dataset <- open_dataset(s3_path, format = "parquet")

ldn_pubs <- places_dataset |>
  filter(bbox$xmin > london_bbox[1],
         bbox$ymin > london_bbox[2],
         bbox$xmax < london_bbox[3],
         bbox$ymax < london_bbox[4]) |>
  select(id, geometry, categories, names) |> 
  filter(categories$primary == 'pub') |>
  collect() |>
  st_as_sf(crs = 4326)

ldn_pubs$Name <- ldn_pubs$names$primary

Sys.setenv(MAPBOX_ACCESS_TOKEN = "your_mapbox_token_here")

ldn_map <- rdeck(map_style = mapbox_dark(), 
                 initial_view_state = view_state(
                   center = c(-0.09, 51.508),
                   zoom = 15,
                   pitch = 30
                 )) |> 
  add_scatterplot_layer(
    name = "London",
    data = ldn_pubs, 
    get_position = geometry, 
    filled = T,
    get_fill_color = '#CC2936',
    radius_scale = 10,
    pickable = T,
    tooltip = Name
  )

ldn_map
