## Day 22: 2 colors
# plotting built environment: Ljubljana

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(sf)
library(osmdata)
library(showtext)

font_add_google(name = 'Space Grotesk', family = 'space')
showtext_auto()

long = 14.509079254797951
lat = 46.048614504263405

point <- st_sf(
  geometry = st_sfc(st_point(c(long, lat)), crs = 4326)
)

point_proj <- st_transform(point, crs = 4326)

# circular buffer with a 1 km radius
circle <- st_buffer(point_proj, dist = 1500) 
circle <- st_transform(circle, crs = 4326)

# generate bounding box based on the circle
circle_bb <- st_bbox(circle)

buildings <- opq(circle_bb) |>
  add_osm_feature(key = "building") |>
  osmdata_sf()

# recode a couple things
buildings$osm_polygons <- buildings$osm_polygons |>
  mutate(building = case_when(
    building %in% c("residential", "apartments", "semidetached_house", "detached") ~ "residential",
    !is.na(building) ~ "non-residential", 
    TRUE ~ NA_character_ 
  ))

ggplot() +
  geom_sf(data = buildings$osm_polygons,
          aes(fill = building), color = NA) +
  scale_fill_manual(values = c('#52AA8A','#E98A15'), na.value = 'grey20') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.key.width = unit(0.1, "cm"),
        legend.text = element_text(color = 'white', size = 40, family = 'space'),
        plot.title = element_text(color = '#E98A15', size = 160, family = 'space', hjust = 0.5),
        plot.background = element_rect(fill = 'black', color = NA)) +
  labs(title = 'Ljubljana')

ggsave('day_22_two_colors.png', height = 10, width = 8, dpi = 300)
