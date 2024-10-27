## Day 21: Kontur dataset.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(showtext)
library(showtext)

font_add_google(name = 'Inconsolata', family = 'Inconsolata')
showtext_auto()

svn_geom <- giscoR::gisco_get_countries(country = 'slovenia',
                                         resolution  = 3) 

# population density of slovenia
# dataset too big for github
svn_data <- read_sf('data/kontur_population_SI_20220630.gpkg')

col_pal1 = rev(colorspace::sequential_hcl(12, palette = "Turku"))

ggplot() +
  geom_sf(data = svn_geom, fill = 'black', color = NA) +
  geom_sf(data = svn_data, aes(fill = population), color = NA) +
  scale_fill_gradientn(colors = col_pal1) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey20', color = NA),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(color = '#ffe5e4', 
                                   family = 'Inconsolata', size = 16),
        legend.key.height = unit(dev.size()[2] / 30, 'inches'),
        plot.title = element_text(color = '#ffe5e4', family = 'Inconsolata',
                                  size = 40)) +
  labs(title = 'Population density: Slovenia')

ggsave('day21_kontur.png', width = 6, height = 4, dpi = 300)

