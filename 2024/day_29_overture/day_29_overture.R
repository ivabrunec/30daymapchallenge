# Day 29: overture buildings
# repurposing basically all of day 13 
# tutorial: https://walker-data.com/posts/overture-buildings/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(arrow)
install_arrow()
library(sf)
library(dplyr)
library(tigris)
library(rdeck) # pak::pak("rdeck")
options(tigris_use_cache = TRUE)

# updated to most (?) recent release
# here: https://docs.overturemaps.org/release/2024-09-18.0/
buildings <- open_dataset('s3://overturemaps-us-west-2/release/2024-09-18.0/theme=buildings?region=us-west-2')
nrow(buildings)


ldn_lat <- c(-0.1029258906155917, -0.0794941134020941)
ldn_long <- c(51.50335642533778, 51.515856610622706)

Sys.setenv(MAPBOX_ACCESS_TOKEN = "your_token_here")

ldn_buildings <- buildings |>
  filter(bbox$xmin > ldn_lat[1],
         bbox$ymin > ldn_long[1],
         bbox$xmax < ldn_lat[2],
         bbox$ymax < ldn_long[2]) |>
  select(id, geometry, height) |> 
  collect() |>
  st_as_sf(crs = 4326) |> 
  mutate(height = ifelse(is.na(height), 8, height))

# define color
col_pal <- scico::scico(100, palette = 'batlow')

ldn_map <- rdeck(map_style = mapbox_light(), 
                 initial_view_state = view_state(
                   center = c(-0.09, 51.508), 
                   zoom = 15,
                   pitch = 30
                 )) |> 
  add_polygon_layer(
    name = "London",
    data = ldn_buildings, 
    get_polygon = geometry, 
    get_elevation = height, 
    get_fill_color = scale_color_linear(
      col = height,
      palette = col_pal
    ),
    extruded = TRUE, 
    opacity = 0.4)

ldn_map
htmlwidgets::saveWidget(nyc_map, "NYC_Building_Map.html", selfcontained = TRUE)
