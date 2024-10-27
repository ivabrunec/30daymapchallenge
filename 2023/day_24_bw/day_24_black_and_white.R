## Day 24: Black and white
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(ggplot2)
library(dplyr)

aus <- giscoR::gisco_get_countries(country = "Australia") |>
  elevatr::get_elev_raster(z = 5, clip = 'location')

plot(aus)

aus1 <- aggregate(aus, 10)
plot(aus1)

dat_df <- as.data.frame(rasterToPoints(aus))
colnames(dat_df) <- c('x','y','elev')

dat_df <- dat_df |>
  mutate(elev_level = ntile(elev, 20))

col_pal = colorRampPalette(colors = c(
    "#030503", 
    "#64532b", 
    "#cfa94d", 
    "#c6c57c", 
    "#b4e0aa"
))
cur_pal = col_pal(100)

bg_col = '#ccc1d9'


ggplot() +
  geom_contour(data = dat_df, aes(x = x, y = y, z = elev, color = ..level..), 
               linewidth=.25, bins = 10) +
  scale_color_gradientn(colors=c("grey20",'grey80')) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'black', color = NA),
        legend.position = 'bottom',
        legend.text = element_text(color = 'white')
        #plot.caption = element_text(color = "#A4C7AE", hjust = 0.05,
        #                            family = 'Squada One', size = 60)
        ) +
  labs(caption = 'Australia') +
  coord_sf()

ggsave('temp2.png', width = 8, height = 8, dpi = 300)
