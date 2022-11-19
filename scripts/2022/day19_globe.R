## Day 19: Globe.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Space Grotesk', family = 'Space Grotesk')
showtext_auto()


gisco_countries <- giscoR::gisco_countries

# code for this from https://github.com/gkaramanis/30DayMapChallenge/blob/main/2022/16-minimal-world/16-minimal-world.R
crs_string <- "+proj=ortho +lat_0=-90 +lon_0=-95 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"

world <- gisco_countries |>
  st_transform(crs = crs_string)

world$fill <- ifelse(world$NAME_ENGL == 'Antarctica', '595,000', '0') |>
  as.factor()

ocean <- st_point(x = c(0, 0)) |>
  st_buffer(dist = 6371000) |>
  st_sfc(crs = crs_string)

ggplot() +
  geom_sf(data = ocean, fill = '#84aaa3', color= NA) +
  geom_sf(data = world, aes(color = fill, fill = fill), size = .3, show.legend = 'point') +
  scale_color_manual(values = c('#3c3f51', '#3c3f51')) +
  scale_fill_manual(values = c('#84aaa3','#3c3f51')) +
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 5, color = NA))) + 
  theme_void() +
  theme(
    plot.background = element_rect(fill = 'grey97', color = NA),
    legend.position = 'bottom',
    legend.title = element_blank(),
    legend.text = element_text(family = 'Space Grotesk', color = 'grey40', size = 20),
    plot.title = element_text(family = 'Space Grotesk', color = 'grey40', hjust = .5, size = 60)
  ) +
  labs(title = 'Emperor Penguins In The Wild')

ggsave('day19_globe.png', height = 4, width = 4, dpi =300)
  