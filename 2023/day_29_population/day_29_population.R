## Day 29: Population

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(showtext)

font_add_google(name = 'Yeseva One', family = 'Yeseva')
font_add_google(name = 'Antic Slab', family = 'Antic')
showtext_auto()

# data from: https://data.humdata.org/dataset/kontur-population-canada
world_pop <- read_sf('kontur_population_CA_20231101.gpkg') |>
  st_transform(crs = 4326)

# area around Lake Ontario
# thanks to Ryan Hart for this code:
# https://github.com/curatedmess/30DayMapChallenge/blob/main/2023/11292023/border_population.R

lat = 43.56116676296167
long = -79.43229632958112

center <- st_sfc(st_point(c(-79.43229632958112, 43.56116676296167)), crs = 4326)
buffer_radius <- 40 * 1609.34  # 1 mile = 1609.34 meters
bounding_box <- st_buffer(center, dist = buffer_radius)

# bounding box
bounding_box_polygon <- st_geometry(bounding_box)

# bounding box coordinates
bbox_coords <- st_bbox(bounding_box)

# convert the polygon to a data frame
bounding_box_df <- st_sf(geometry = bounding_box_polygon)

# crop the pop data using the bounding box
df_cropped <- st_intersection(world_pop, bounding_box_df)

cols <- c(
  "#182223", 
  "#73e6cd", 
  "#b7f6c5", 
  "#d4eecb", 
  "#e8bfd3"
)

ggplot() +
  geom_sf(data = df_cropped, aes(fill = population), color=NA) +
  annotate("text", x = long-.05, y = lat-.2, label = "'the golden horseshoe'", 
           color = "#e8bfd3", family = 'Yeseva', size = 25, hjust = 0.5) +
  annotate("text", x = long, y = lat-.7, label = "The Greater Toronto & Hamilton Area", 
           color = "#e8bfd3", family = 'Antic', size = 10, hjust = 0.5) +
  scale_fill_gradientn(colors = cols) +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#212121', color = NA))

ggsave('day_29_population.png', height = 5, width = 4, dpi = 300)
