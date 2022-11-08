## Day 8: OpenStreetMap

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

library(sf)
library(ggplot2)
library(osmdata)
library(showtext)
library(dplyr)

font_add_google(name = 'Barlow', family = 'Barlow')
showtext_auto()

bb = c(14.4927, 46.0421,
       14.5219, 46.0586)

roads_main <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway',
                  value = c('primary',
                            'secondary',
                            'motorway',
                            'trunk',
                            'primary_link',
                            'secondary_link',
                            'trunk_link',
                            'motorway_link'
                            )) |>
  osmdata_sf()

roads_all <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway') |>
  osmdata_sf()

buildings <- opq(bb) |> 
  add_osm_feature(key = "building") %>%
  osmdata_sf()

landuse <- opq(bb) |>
  add_osm_feature(key = 'landuse', value = c('grass','forest')) |>
  osmdata_sf()

leisure <- opq(bb) |>
  add_osm_feature(key = 'leisure', value = c('garden','park')) |>
  osmdata_sf()

natural <- opq(bbox = bb) |>
  add_osm_feature(key = 'natural') |>
  osmdata_sf()

# circle
# code from: https://github.com/AbdoulMa/30DayMapChallenge/blob/main/Day8/day8_2022.R
long = 14.5077
lat = 46.0494

center_proj <-
  tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_main$osm_lines))

dist <-  1000
circle <- tibble(long, lat) %>%
  st_as_sf(coords = c("long", "lat"), crs = crs(roads_main$osm_lines)) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = crs(roads_main$osm_lines))


roads_lines <- st_intersection(circle, roads_main$osm_lines)
roads_all_lines <- st_intersection(circle, roads_all$osm_lines)
river_lines <- st_intersection(circle, river$osm_multipolygons)
buildings_lines <- st_intersection(circle, buildings$osm_polygons)
landuse_lines <- st_intersection(circle, landuse$osm_polygons) 
leisure_lines <- st_intersection(circle, leisure$osm_polygons)
natural_lines <- st_intersection(circle, natural$osm_polygons)

ggplot() +
  geom_sf(data = river_lines, color = '#0f3b5f', fill = '#0f3b5f') +
  geom_sf(data = landuse_lines, color = '#3c6f57', fill = '#3c6f57') +
  geom_sf(data = leisure_lines, color = '#5f7c45', fill = '#5f7c45') +
  geom_sf(data = natural_lines, color = '#3c6f57', fill = '#3c6f57')+
  geom_sf(data = buildings_lines, color = NA, fill = '#f3b49f') +
  geom_sf(data = roads_all_lines, color = 'grey60', fill = 'grey60', size = .3) +
  theme_void() +
  theme(plot.caption = element_text(hjust = .5, color = '#fee3eb', 
                                    family = 'Barlow', size = 60), 
        plot.background = element_rect(fill = "#3a3b3c", color = NA)) +
  labs(caption = 'Ljubljana, Slovenia')
  
ggsave('day8_OSM.png', width = 6, height = 6.5, dpi = 300)  
