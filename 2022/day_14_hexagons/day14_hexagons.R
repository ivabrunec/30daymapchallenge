## Day 14: Hexagons.

library(dplyr)
library(ggplot2)
library(sf)
library(showtext)

font_add_google(name = 'Space Mono', family = 'Space Mono')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# dataset too big for github
# emissions data from https://data.london.gov.uk/dataset/london-atmospheric-emissions-inventory--laei--2019
no2_data <- read.csv('data/laei_LAEI2019v3_CorNOx15_NO2.csv')

col_pal = colorspace::sequential_hcl(100, palette = "Batlow")

ggplot() +
  stat_summary_hex(data = no2_data,
                   aes(x = x, y = y, z = conc), 
                   fun = function(x) mean(x), bins = c(150,100),
                   color = 'grey30', size = .15) +
  scale_fill_gradientn(colors=col_pal)+
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey20', color = NA),
        legend.position = 'bottom',
        legend.key.height = unit(dev.size()[2] / 20, 'inches'),
        legend.title = element_blank(),
        legend.text = element_text(color = 'grey96', 
                                   family = 'Space Mono', size = 25),
        plot.caption = element_text(color = 'grey96', hjust = .5, 
                                    family = 'Space Mono', size = 28),
        plot.title = element_text(color = '#ffbbe0', hjust = .1, 
                                  family = 'Space Mono', size = 70)) +
  labs(caption = 'Annual NO2 concentration in µg/m3',
       title = 'London air pollution')

ggsave('day14_hexagons.png', width = 6, height = 5, dpi = 300)
