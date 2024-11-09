## Day 8: Humanitarian Data Exchange
# Volcanoes of Japan.
# volcano data: https://data.humdata.org/dataset/volcano-population-exposure-index-gvm
# bathymetry data: https://download.gebco.net/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(rnaturalearth)
library(showtext)

font_add_google(name = 'Rokkitt', family = 'dm')
font_add_google(name = 'Cutive Mono', family = 'dm')
showtext_auto()

volcanoes <- read_sf('data/volcano.json') |>
  filter(Country == "Japan")

japan_polygon <- ne_countries(scale = "medium", returnclass = "sf") |>
  filter(name == "Japan")

# get elevation raster
japan_elev <- japan_polygon |>
  elevatr::get_elev_raster(z = 5, clip = 'location')
plot(japan_elev)
japan_points <- rasterToPoints(japan_elev)
colnames(japan_points) <- c('x','y','z')

# get bounding box of japan polygon
# crop bathymetry to just this polygon
bathy <- raster('data/GEBCO_Japan_bathy/GEBCO_08_Nov_2024_2e693f6b9431/gebco_2024_n46.0_s20.0_w124.0_e146.0.tif')
bathy_agg <- aggregate(bathy, fact = 10)
plot(bathy_agg)

bathy_points <- rasterToPoints(bathy_agg)
colnames(bathy_points) <- c('x','y','z')

# combine the two datasets
bathy_elev <- rbind(japan_points, bathy_points)
# this isn't very precise, but it's intentional - it creates a raster-y effect

label_text <- data.frame(x = 145.3, y = 20, label = "volcanoes of Japan")
label_shadow <- data.frame(x = 145.3, y = 20.1, label = "volcanoes of Japan")

ggplot() +
  geom_point(data = bathy_elev,
             aes(x = x, y = y, color = z)) +
  geom_sf(data = japan_polygon,
          color = '#942911', fill = NA,
          size = 1) +
  geom_sf(data = volcanoes,
          color = '#FE7C44') +
  scico::scale_color_scico(palette = 'fes',
                          midpoint = 0) +
  geom_text(data = label_shadow,
            aes(x = x, y = y, label = label),
            hjust = 0,
            color = 'darkgreen', 
            angle = 90,
            size = 60, 
            family = "dm") +
  geom_text(data = label_text,
            aes(x = x, y = y, label = label),
            hjust = 0,
            color = '#FE7C44', 
            angle = 90,
            size = 60, 
            family = "dm") +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#E6E1C5', color = NA))

ggsave('day_8_hdx.png', height = 12, width = 7)
  