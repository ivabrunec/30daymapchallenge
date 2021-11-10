## Day 9: Monochrome.

library(tidyverse)
library(sf)
library(osmdata)
library(extrafont)

## only run line 9 the first time to import all fonts
#font_import()
loadfonts(device = "win")

#bbx <- getbb('London, U.K.') ## too big
# custom bounding box
bbx <- c(-.20, 51.47, -.07, 51.53)

# all streets
streets <- opq(bbox = bbx) %>%
  add_osm_feature(key = "highway") %>%
  osmdata_sf()

## this works but results in gaps
#streets <- opq(bbox = bbx) %>%
#  add_osm_feature(key = "highway", value = c("motorway","motorway_link","primary","secondary","tertiary","primary_link","secondary_link","tertiary_link")) %>%
#  osmdata_sf()

# load only footways
footways <- opq(bbox = bbx) %>%
  add_osm_feature(key = "highway", value = c("footway","residential","living_street","pedestrian")) %>%
  osmdata_sf()

# read in the thames
water_osm <- opq(bbox=bbx) %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf()

# plot
ggplot() +
  geom_sf(data = streets$osm_lines,  col = "grey40", size = .3, alpha = .65) +
  #geom_sf(data = footways$osm_lines, col = "grey50", size = .3, alpha = .65) +
  geom_sf(data = water_osm$osm_multipolygons, fill="grey72", size=NA) +
  coord_sf(xlim = c(-.20, -.07), ylim = c(51.47, 51.53), expand = FALSE) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(c(t=.6,r=1,b=.6,l=1), "cm"),
        plot.caption = element_text(color = "grey58", size = 10, 
                                    hjust = .95, vjust = .5, family = "Franklin Gothic Medium"),
        plot.background=element_rect(fill = 'grey92', color=NA),
        panel.background = element_rect(fill = 'grey92', color=NA)) +
  labs(caption = "London, UK")

ggsave('day9_london.png', width = 6, height = 4, dpi = 300)