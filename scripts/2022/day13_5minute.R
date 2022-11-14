library(sf)
library(osmdata)
library(ggplot2)
library(showtext)

font_add_google(name = 'Work Sans', family = 'Work Sans')
showtext_auto()

bb = c(-117.18396934005607, 32.6947745108055,
       -117.12619318942403, 32.744044109077066)

roads_main <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway') |>
  osmdata_sf()

ggplot() +
  geom_sf(data = roads_main$osm_lines, color = '#f9c80e', size = .3) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey96', color = NA),
        plot.caption = element_text(color = '#f9c80e', size = 90, 
                                    hjust = .5, family = 'Work Sans')) +
  labs(caption = 'San Diego, CA')

ggsave('day13_5min.png', width = 5, height = 6, dpi = 300)
