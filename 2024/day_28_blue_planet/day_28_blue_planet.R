## Day 28: The blue planet
# pacific ocean bathymetry

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(ggplot2)
library(terra)
library(tidyterra)
library(geomtextpath)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'roboto', bold.wt = 800)
showtext_auto()

bathy_raster <- rast('data/GEBCO_BATHY_2002-01-01_rgb_3600x1800.FLOAT.TIFF')
plot(bathy_raster)
bathy_raster[bathy_raster == 99999] <- NA

crs_string <- "+proj=ortho +lon_0=-160 +lat_0=0 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"
bathy_raster_ortho <- terra::project(bathy_raster, crs_string)

col_pal <- scico::scico(palette = 'tokyo', 50)
ggplot() +
  geom_spatraster(data = bathy_raster_ortho) +
  scico::scale_fill_scico(palette = 'devon', na.value = '#333745') + 
  geom_textcurve(data = data.frame(x = 5000000, xend = 4000000, y = -3000000, yend = -4200000), 
                 aes(x, y, xend = xend, yend = yend), hjust = 0.5, 
                label = "the blue planet", curvature = -0.2, family = 'roboto',
                color = '#333745', size = 12) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#333745', color = NA),
        legend.position = 'none') 

ggsave('day_28_blue.png', height = 8, width = 8, dpi = 300)
