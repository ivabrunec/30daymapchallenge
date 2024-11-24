# Day 24: Circular shapes only
# data from: https://hub.arcgis.com/datasets/edu::soils-of-canada/explore

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(showtext)

font_add_google(name = 'Archivo', family = 'archivo', bold.wt = 800)
showtext_auto()

soil_dat <- read_sf('data/Soils_of_Canada/Soils_of_Canada.shp')

ggplot() +
  geom_sf(data = soil_dat,
          aes(fill = ORDER_),
          color = NA)
plot(soil_dat)

# borrowing approach from here:
# https://github.com/gkaramanis/30DayMapChallenge/blob/main/2024/24-circles-euroleague/24-circles-euroleague-instagram.R
bbox <- st_bbox(soil_dat)

circle_spacing <- 100000
grid <- expand.grid(
  x = seq(bbox["xmin"], bbox["xmax"], by = circle_spacing),
  y = seq(bbox["ymin"], bbox["ymax"], by = circle_spacing)
)

grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = st_crs(soil_dat))

soil_dat_points <- st_intersection(grid_sf, soil_dat)
col_pal <- c("#696969", "#FF6347", "#7B68EE", "#ADD8E6", "#ACBFA4", "#228B22", "#FFD700", "#FF7F11")

ggplot() +
  geom_sf(data = soil_dat_points,
          aes(color = SurfMat)) +
  scale_color_manual(values = col_pal) +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 60, color = 'white', family = 'archivo'),
        plot.background = element_rect(fill = '#333333', color = NA))


ggsave('day_24_circles.png', width = 10, height = 14, dpi = 300)

