# Day 25: heat.
# global temperature anomaly
# data: https://neo.gsfc.nasa.gov/view.php?datasetId=GISS_TA_Y&date=2023-12-01

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(scico)
library(showtext)

font_add_google(name = 'Cutive Mono', family = 'cutive', bold.wt = 800)
showtext_auto()


temp_dat <- rast('data/GISS_TA_Y_2023-01-01_rgb_720x360.FLOAT.TIFF')
plot(temp_dat)
# looks like NAs are coded as 99999
temp_dat[temp_dat == 99999] <- NA
plot(temp_dat)

mollweide_crs <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m"
dat_mollweide <- project(temp_dat, mollweide_crs)
plot(dat_mollweide)

# world outline
world <- giscoR::gisco_get_countries(resolution = "20")
world_outline <- st_union(world)
world_mollweide <- st_transform(world_outline, crs = mollweide_crs)

col_pal <- rev(scico(30, palette = 'glasgow'))

ggplot() +
  geom_spatraster(data = dat_mollweide) +
  geom_sf(data = world_mollweide, fill = NA, color = "black", size = 0.3) +
  scale_fill_gradientn(colors = col_pal, na.value = 'transparent') +
  labs(title = "Global temperature anomaly, 2023",
       fill = 'Degrees C') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.key.height = unit(0.2, 'cm'),
        legend.title = element_text(size = 30, family = 'cutive'),
        legend.text = element_text(size = 30, family = 'cutive'),
        plot.title = element_text(hjust = 0.5, size = 60, family = 'cutive'),
        plot.background = element_rect(fill = '#EAFDF8', color = NA))

ggsave('day_25_heat.png', width = 10, height = 5, dpi = 300)
