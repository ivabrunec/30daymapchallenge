## Day 11: Arctic: multiyear sea ice
# ice extent: https://nsidc.org/data/g10033/versions/1#anchor-data-access-tools

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(ggplot2)
library(raster)
library(showtext)

font_add_google(name = 'Cambay', family = 'cambay', bold.wt = 700)
showtext_auto()

# read in data, keep 'sea ice' data only
ice_shp <- read_sf('data/nh_20241108/nh_20241108.shp') |>
  filter(POLY_TYPE == "I") |>
  arrange(myi)

crs_string <- "+proj=ortho +lon_0=-60 +lat_0=70 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"
ice_shp <- ice_shp |>
  st_transform(crs = crs_string)

world <- giscoR::gisco_countries
world <- world |>
  st_transform(crs = crs_string)

ocean <- st_point(x = c(-60, 70)) |>
  st_buffer(dist = 6370000) |>
  st_sfc(crs = crs_string)

col_pal<-c(  
  "#4f517e", 
  "#858de5", 
  "#be8dc3", 
  "#db7b7a", 
  "#e7764c", 
  "#e69b81"
)

ggplot() +
  geom_sf(data = ocean, fill = '#333333', color= 'NA') +
  geom_sf(data = world, color = 'grey80', fill = 'grey80') +
  geom_sf(data = ice_shp,
          aes(fill = myi),
          color = NA) +
  scale_fill_gradientn(colors = rev(col_pal)) +
  coord_sf(xlim = c(-6000000, 6000000), ylim = c(-5000000, 5000000), expand = FALSE) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.key.height = unit(0.2, 'cm'),
        legend.position = 'top',
        legend.text = element_text(color = 'white', size = 20),
        plot.background = element_rect(fill = 'black', color= NA),
        plot.title = element_text(color = 'grey80', family = 'cambay', size = 80, face = 'bold'),
        plot.subtitle = element_text(color = 'grey80', family = 'cambay', size = 30),
        ) +
  labs(subtitle = "current concentration of resilient multiyear sea ice across the Arctic",
       title = "where ice stays:")

ggsave('day_11_arctic.png', height = 5, width = 5, dpi = 400)

