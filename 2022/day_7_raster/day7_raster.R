setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(ggplot2)

# data downloaded from https://storage.googleapis.com/earthenginepartners-hansen/GFC-2021-v1.9/download.html
# dataset too big for github
data1 <- raster('data/Hansen_GFC-2021-v1.9_treecover2000_30N_090E.tif')
ra1 <- aggregate(data1, fact = 50)

data2 <- raster('data/Hansen_GFC-2021-v1.9_treecover2000_30N_080E.tif')
ra2 <- aggregate(data2, fact = 50)

data_merged <- merge(ra1, ra2)

# get bhutan outline
bhutan_geom <-
  giscoR::gisco_get_countries(resolution = "10", country = "Bhutan")

data_select <- mask(data_merged, bhutan_geom)

# intentionally reduce resolution to get a blocky effect
data_select_lowres <- aggregate(data_select, fact = 3)
ra_df_lowres <- rasterToPoints(data_select_lowres)
colnames(ra_df_lowres) <- c('x','y','forest')
ra_df_lowres <- as.data.frame(ra_df_lowres)

# full size
ra_df <- rasterToPoints(data_select)
colnames(ra_df) <- c('x','y','forest')
ra_df <- as.data.frame(ra_df)

g1 <- ggplot()+
  geom_raster(data = ra_df_lowres, aes(x = x, y = y, fill = forest)) +
  colorspace::scale_fill_continuous_sequential(palette = 'PuBuGn') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#595959', color = NA),
        panel.background = element_rect(fill = '#595959', color = NA),
        legend.position = '') +
  coord_fixed(1.3)

library(rayshader)
plot_gg(g1, width = 6, height = 6, scale = 150, raytrace = T, sunangle = 0,
        windowsize = c(1000, 1000))
render_camera(zoom = .60, phi = 90, theta = 0)

render_snapshot('bhutan1_big.png')
render_highquality('bhutan_highqual.png')
