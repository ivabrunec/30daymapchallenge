## Day 25: Two colours.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(showtext)
library(sf)

remotes::install_github('yixuan/showtext')

font_add_google(name = 'BhuTuka Expanded One', family = 'BhuTuka Expanded One')
showtext_auto()

rivers_eur <- st_read('data/ne_10m_rivers_europe/ne_10m_rivers_europe.shp')
rivers <- st_read('data/ne_10m_rivers_lake_centerlines_scale_rank/ne_10m_rivers_lake_centerlines_scale_rank.shp')

box1 = extent(rivers_eur)

rivers_cropped <- st_crop(rivers, box1)

ggplot() +
  geom_sf(data = rivers_cropped, aes(size = scalerank), fill = 'black', color = '#26f99a') +
  scale_size(range = c(.1,.6)) +
  geom_sf(data = rivers_eur, size = .3, color = '#388e65') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#1d1354', color = NA),
        legend.position = '',
        plot.title=element_text(margin=margin(t=410, b=-410), 
                                color = '#26f99a', size = 65, family = 'BhuTuka Expanded One')) +
  coord_sf(xlim=c(-9.0, 45.35), ylim = c(31.05, 69.0)) +
  labs(title = 'Rivers of Europe')

ggsave('day25_2colors.png', width = 5.5, height = 6, dpi = 300)
