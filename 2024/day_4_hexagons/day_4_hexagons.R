## Day 4: Hexagons
# Elevation of Vancouver Island
# shapefile from: https://cartographyvectors.com/map/1235-vancouver-island

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(sf)
library(raster)
library(elevatr)
library(dplyr)
library(rayshader)

# read in vancouver island shapefile + extract bounding box
van_shape <- read_sf('data/vancouver-island_1235.geojson')
bbox <- st_bbox(van_shape)

bbox_sf <- st_as_sfc(bbox)

# get data for entirety of canada
rgn_shp <- giscoR::gisco_get_countries(country = "Canada", resolution = "01")
# filter canada data to the bounding box
bbox_sf <- st_transform(bbox_sf, st_crs(rgn_shp))
rgn_within_bbox <- st_intersection(rgn_shp, bbox_sf)

# get elevation, limit to boundaries
elev_raster <- get_elev_raster(van_shape, z = 8, clip = 'location')
raster::plot(elev_raster)

# ggplot then convert to 3d
van_df <- as.data.frame(rasterToPoints(elev_raster))
colnames(van_df) <- c('x', 'y', 'z')

col_pal <- c("#d0e1f2", "#8fbccc", "#5a9cae", "#2e7d88", "#196b60", "#14594b")

pp <- ggplot() + 
  geom_sf(data = rgn_within_bbox, 
          fill = 'grey92',
          color = NA) +
  stat_summary_hex(data = van_df, aes(x, y, z = z),
                   bins = 100,
                   color = NA) +
  scale_fill_gradientn(colors = col_pal) +
  theme_void() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#ebf2f9', color = NA))

plot_gg(pp, width = 5, height = 4, scale = 200, 
        raytrace = FALSE, windowsize = c(1000, 960),
        max_error = 0.01, verbose = TRUE,
        solid = FALSE)

render_highquality(filename = 'day_4_hexagons_vancouver_island.png',
                   samples = 360,
                   light = TRUE,
                   lightdirection = 270,
                   lightintensity = 300,
                   lightaltitude = 35,
                   lightcolor = '#F3FAE1',
                   ground_material = rayrender::diffuse(color = '#ebf2f9'),
                   )


  