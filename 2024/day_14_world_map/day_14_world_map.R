## Day 14: a world map
# volcano risk as extruded points
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(sf)
library(rayshader)

world_polygons <- giscoR::gisco_get_countries()

volcanoes <- read_sf('volcano.json') 
volcanoes <- volcanoes |>
  dplyr::filter(H_active == 1)

volcanoes$risk[volcanoes$risk == "NULL"] <- 0
volcanoes$risk <- as.numeric(volcanoes$risk)

temp <- ggplot() +
  geom_sf(data = world_polygons,
          fill = 'grey', color = NA) +
  geom_point(data = volcanoes,
             aes(x = Longitude, y = Latitude,
                 color = risk),
             size = 0.2) +
  scale_color_gradientn(colors = c('#091E05', "#004F2D", "#FFDA22", "#D87CAC")) +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(color = NA, fill = 'grey92'))

plot_gg(temp,
        width = 10,
        height = 6,
        windowsize = c(1200, 800),
        solid = F,
        raytrace = F)

render_highquality(filename = 'day_14_world_map.png',
                   lightintensity = 800,
                   lightdirection = 200,
                   samples = 365)
