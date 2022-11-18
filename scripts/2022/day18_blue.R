## Day 18: Blue.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(dplyr)
library(elevatr)
library(ggplot2)
library(showtext)

# great barrier reef data from: https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/115066
# this is panel A
# aggregated in QGIS, original file is ~1gb

gbr <- raster('data/GreatBarrierReef_resampled.tif')

gbr_df <- rasterToPoints(gbr) |>
  as.data.frame()
colnames(gbr_df) <- c('x','y','z')

aus_geom <-
  giscoR::gisco_get_countries(resolution = "20", country = "Australia")

# plot in rayshader
library(rayshader)
elmat <- raster_to_matrix(gbr) 

watermap <- elmat
watermap[watermap < 0] = 0


elmat %>%
  sphere_shade(texture='imhof3') %>%
  plot_3d(elmat, zscale = 20, theta = 0, phi = 30, zoom = .6, 
          background = 'grey90', solid = T, shadow = T,
          solidcolor = '#Afbdc1', solidlinecolor = "#Afbdc1", 
          windowsize=c(1800, 800))

render_water(elmat,zscale=20, 
             waterdepth = -50,
             watercolor = '#599baf',
             wateralpha = .4
             )

render_camera(zoom = .7)

render_highquality('day18_rayshader.png', 
                   lightcolor = '#Cdd8e2',
                   lightintensity = 600,
                   samples = 500)

# plot inset map
extent(gbr)
rect <- data.frame(
  x = c(142.0001, 142.0001, 147.0001, 147.0001),
  y = c(-17.00013, -10.00014, -10.00014, -17.00013)
)

col_pal = colorspace::diverging_hcl(100, palette = "Tofino")

ggplot() +
  geom_sf(data = aus_geom, color = 'grey60', fill = 'grey80') +
  geom_raster(data = gbr_df, aes(x = x, y = y, fill = z)) +
  scale_fill_gradientn(colours = col_pal) +
  geom_polygon(data = rect, aes(x, y, group = 1), color = 'grey40', fill = NA) +
  theme_void() +
  theme(legend.position = '')

ggsave('day18_inset.png', width = 3, height = 2, dpi = 300)

# combine with magick
library(magick)

aus_temp <- image_read('day18_inset.png')
ray_temp <- image_read('day18_rayshader.png')

test <- image_composite(ray_temp, image_scale(aus_temp, 'x200'), offset='+40+40')

image_write(test, 'day18_test.png')
