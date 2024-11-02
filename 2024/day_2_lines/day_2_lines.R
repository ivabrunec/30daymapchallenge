## Day 2: Map of Australia with contour lines
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(showtext)

font_add_google(name = 'Ultra', family = 'Ultra')
showtext_auto()

aus <- giscoR::gisco_get_countries(country = "Australia") |>
  elevatr::get_elev_raster(z = 5, clip = 'location')
plot(aus)

dat_df <- as.data.frame(rasterToPoints(aus))
colnames(dat_df) <- c('x','y','elev')

col_pal <- scico::scico(10, palette = 'tokyo')

# borrowing from my own 2023 code - Toronto points
x_coord <- c(158)
y_coord <- c(-32)
label_text <- c('Australia')

label_df <- data.frame(x_coord, y_coord, label_text)

x_coord <- c(158.1)
y_coord <- c(-32.1)
label_text <- c('Australia')

label_df_shadow <- data.frame(x_coord, y_coord, label_text)

ggplot() +
  geom_contour(data = dat_df, aes(x = x, y = y, z = elev, color = ..level..), 
               linewidth=.25, bins = 20) +
  scale_color_gradientn(colors=col_pal) +
  theme_void() +
  geom_text(data = label_df_shadow, 
            aes(x = x_coord, y = y_coord, label = label_text),
            size = 35, angle = 90, hjust = 0, family = 'Ultra', color = '#a2d988') +
  geom_text(data = label_df, 
            aes(x = x_coord, y = y_coord, label = label_text),
            size = 35, angle = 90, hjust = 0, family = 'Ultra', color = '#572948') +
  theme(plot.background = element_rect(fill = '#0e1111', color = NA),
        legend.position = 'right',
        legend.direction = 'vertical',
        legend.key.width = unit(0.2, 'cm'),
        legend.box.margin = unit(c(0,0,0,0), 'cm'),
        legend.text = element_text(color = 'grey80', size = 30),
        plot.margin = margin(5, 20, 5, 5)
        ) +
  coord_sf()

ggsave('day_2_lines_contours.png', width = 8, height = 8, dpi = 300)
