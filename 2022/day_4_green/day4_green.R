## Day 4: Green.
# greenspaces in the city of New York

# borough outlines from:
# https://www1.nyc.gov/site/planning/data-maps/open-data/districts-download-metadata.page

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

library(sf)
library(ggplot2)
library(osmdata)
library(showtext)

font_add_google(name = 'Alfa Slab One', family = 'Alfa Slab One')
font_add_google(name = 'PT Mono', family = 'PT Mono')
showtext_auto()

# dataset too big for github
nyc_bound <- st_read('data/nybb_22b/nybb.shp')

bb <- getbb ('new york city')

# leisure
leisure <- opq(bbox = bb) |>
  add_osm_feature(key = 'leisure', 
                  value = c(
                    'park',
                    'pitch', 
                    'garden')) |>
  osmdata_sf () 

leisure$osm_polygons <- leisure$osm_polygons |> 
  st_transform(st_crs(nyc_bound)) |>
  st_set_crs(st_crs(nyc_bound))

# landuse values
landuse <- opq(bbox = bb) |>
  add_osm_feature(key = 'landuse', 
                  value = c(
                    'forest',
                    'meadow',
                    'recreation_ground',
                    'cemetery')) |>
  osmdata_sf () 

landuse$osm_polygons <- landuse$osm_polygons |> 
  st_transform(st_crs(nyc_bound)) |>
  st_set_crs(st_crs(nyc_bound))

# natural values
natural <- opq(bbox = bb) |>
  add_osm_feature(key = 'natural', 
                  value = c('wood')) |>
  osmdata_sf () 

natural$osm_polygons <- natural$osm_polygons |> 
  st_transform(st_crs(nyc_bound)) |>
  st_set_crs(st_crs(nyc_bound))

# only keep parks within the polygons
within_poly1 <- st_intersection(nyc_bound, leisure$osm_polygons)
within_poly2 <- st_intersection(nyc_bound, landuse$osm_polygons)
within_poly3 <- st_intersection(nyc_bound, natural$osm_polygons)

ggplot()+
  geom_sf(data = nyc_bound, fill = 'grey90', color = 'grey60', size = .2) +
  geom_sf(data = within_poly1, fill = '#2f9a62', color = NA) +
  geom_sf(data = within_poly2, fill = '#2f9a62', color = NA) +
  geom_sf(data = within_poly3, fill = '#2f9a62', color = NA) +
  theme_minimal() +
  theme(#panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#f4d8da', color = NA),
        plot.background = element_rect(fill = '#f4d8da', color = NA),
        axis.text = element_blank(),
        plot.title = element_text(color = '#2f9a62', size = 92,
                                  family = 'Alfa Slab One',
                                  vjust = 0, hjust = .5),
        plot.caption = element_text(color = 'grey20', size = 20,
                                    family = 'PT Mono',
                                    vjust = 10, hjust = 1)
  ) +
  labs(title = 'NYC green spaces',
       caption = 'Data: NYC Open Data | Map: {osmdata}') 

ggsave('day5_green.png', width = 5, height = 5.5, dpi = 300)
