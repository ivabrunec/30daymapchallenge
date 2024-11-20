## Day 20: OSM data
# map of Chicago centered on the Bean

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

library(sf)
library(ggplot2)
library(osmdata)
library(showtext)
library(dplyr)

font_add_google(name = 'DM Sans', family = 'dm', bold.wt = 800)
showtext_auto()

long = -87.62337579034902
lat = 41.88286887865635

point <- st_sf(
  geometry = st_sfc(st_point(c(long, lat)), crs = 4326)
)

point_proj <- st_transform(point, crs = 4326)

# circular buffer with a 1 km radius
circle <- st_buffer(point_proj, dist = 2000) 
circle <- st_transform(circle, crs = 4326)

# generate bounding box based on the circle
circle_bb <- st_bbox(circle)

buildings <- opq(circle_bb) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

buildings_commercial <- opq(circle_bb) |>
  add_osm_feature(key = "building", value = "commercial") |>
  osmdata_sf()

leisure <- opq(circle_bb) |>
  add_osm_feature(key = "leisure", value = c("garden", "park")) |>
  osmdata_sf()

water_natural <- opq(circle_bb) |>
  add_osm_feature(key = "natural", value = c("water", "wetland")) |>
  osmdata_sf()

# filter so that only elements within circle are kept
buildings_circle <- st_intersection(circle, buildings$osm_polygons)
buildings_commercial_circle <- st_intersection(circle, buildings_commercial$osm_polygons)
leisure_circle <- st_intersection(circle, leisure$osm_polygons)
water_circle <- st_intersection(circle, water_natural$osm_multipolygons)

ggplot() +
  geom_sf(data = leisure_circle, color = '#CAD6C0', fill = '#CAD6C0') +
  geom_sf(data = buildings_circle, color = '#A27E8E', fill = '#A77464',
          linewidth = .2) +
  geom_sf(data = buildings_commercial_circle, color = '#88292F', fill = '#88292F',
          linewidth = .2) +
  geom_sf(data = water_circle, color = '#577590', fill = '#577590') +
  ylim(c(41.86,41.9)) +
  annotate("text",
           label = "Chicago, IL",
           x = -87.625, y = 41.862,
           size = 160, family = 'dm', fontface = 'bold', color = '#CAD6C0') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#1C3144', color = NA),
        plot.margin = unit(c(2, 2, 2, 2), "cm")) 


ggsave('day_20_osm.png', height = 12, width = 12, dpi = 400)
