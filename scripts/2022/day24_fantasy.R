## Day 24: Fantasy.

library(dplyr)
library(sf)
library(ggplot2)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# otherwise extrafont import doesnt work for me:
#remotes::install_version("Rttf2pt1", version = "1.3.8")
library(extrafont)
font_import()
loadfonts(device = 'win')
fonts()

## tfl ####
tfl <- st_read('data/tfl_lines.json')

# filter out everything that's NOT the underground
tfl <- tfl %>%
  filter(!str_detect(lines, 'London Overground')) %>%
  filter(!str_detect(lines, 'DLR')) %>%
  filter(!str_detect(lines, 'TfL Rail')) %>%
  filter(!str_detect(lines, 'Tramlink')) %>%
  filter(!str_detect(lines, 'Crossrail')) %>%
  filter(!str_detect(lines, 'East London')) %>%
  filter(!str_detect(lines, 'Thameslink'))

# good god this is hacky
df <- as.data.frame(str_split_fixed(tfl$lines, ':', 2))
df <- as.data.frame(str_split_fixed(df$V2, ',', 2))
df <- as.data.frame(gsub('[\"]', '', df$V1))
colnames(df) <- c('tfl_lines')

# center map at 0,0
# get geometry and center of this map
tfl_geom <- st_geometry(tfl)
tfl_box <- st_as_sfc(st_bbox(tfl_geom))
box_ctrd <- st_centroid(tfl_box)
p_geom <- box_ctrd

# now move to new york coordinates

# get the long/lat scaling right
# get bounding box: prints out min/max
tfl_box
# instead of 0,0, we want the center to be at... x & y
#xnew = 73.7734
xnew = 73.85
ynew = 51.402 - 40.6293

p <- st_point(c(xnew, ynew))
p_geom2 <- st_geometry(p)

tfl_geom2 = (tfl_geom - p_geom2) 
tfl_geom2
 
# nyc data ####
nyc <- st_read('data/Subway Lines/geo_export_be18b715-615c-4bcb-85aa-dc60e8d3723d.shp')
nyc_geom <- st_geometry(nyc)
st_crs(tfl_geom2) <- st_crs(nyc)


## put it all together

# first, get underlying high res data
library(mapdata)
library(raster)
us <- getData("GADM", country = "USA", level = 1) %>%
  st_as_sf(us)

states <- c('New York','New Jersey')
us.states <- us[us$NAME_1 %in% states, ]

# for some reason this does not include the Hudson
# get it from osm
library(osmdata)

bbx = c(-74.05520136615102, 40.7112695404427,
        -73.90141119290442, 40.90919884757917)
rivers <- opq(bbox = bbx) %>%
  add_osm_feature(key = "water", value = "river") %>% 
  osmdata_sf()

# define palette for tfl lines:
tfl_palette <- c("#b56307", "#e32018", "#fed303",
                 "#017929", '#D71A21', '#f3a9bb',
                 '#a0a4a7', '#9c0157', '#010101',
                 '#013785', '#0199d5', '#97ccba')

# now plot everything: 
ggplot() +
  geom_sf(data = us$geometry, fill='grey97', color='grey40', size = .2) +
  geom_sf(data = rivers$osm_multipolygons, fill = 'grey85', color = 'grey40', size = .2) +
  geom_sf(data = nyc_geom, fill = 'grey97', color = 'grey50', alpha = .3, size = .2) +
  geom_sf(data = tfl_geom2, aes(color = df$tfl_lines), size = .75) +
  scale_color_manual(values=tfl_palette) +
  coord_sf(xlim=c(-74.5, -73.6), ylim = c(40.56, 40.92)) +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        plot.background = element_rect(fill = 'grey85', color='grey85'),
        panel.background = element_rect(fill = 'grey85', color = 'grey85'),
        plot.title = element_text(size = 35, color = 'black', 
                                  hjust = .5, family = 'London Tube'),
        legend.text = element_text(family = 'London Tube')) +
  labs(title = 'Transport for New York')

ggsave('day24_fantasy.png', width = 6, height = 5, dpi = 400)
#

