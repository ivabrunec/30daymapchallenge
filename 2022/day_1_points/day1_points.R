 ## Day 1: Points

# Toronto street trees
# data from: https://open.toronto.ca/ (centreline & street tree data)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(osmdata) 
library(geojsonio)
library(MetBrewer)
library(showtext) # fonts


font_add_google(name = "Abril Fatface", family = "abril")
font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

# read in tree inventory
# dataset too big for github
trees <- geojson_read("data/Street Tree Data.geojson",  what = "sp")
trees_df <- as.data.frame(trees)

# add underlying osm map
bbx = c(-79.50, 43.56,
        -79.25, 43.72)

# filter out corresponding tree coordinates as well
trees_df <- filter(trees_df, coords.x1 > -79.5 & coords.x1 < -79.25 &
                       coords.x2 > 43.46 & coords.x2 < 43.72)

# get just the genus (?) not species
trees_df$species_parent <- sub("\\,.*", "", trees_df$COMMON_NAME)

# summarize by species
trees_sum <- trees_df |>
  group_by(species_parent) |>
  summarise(n_trees = n()) |>
  slice_max(n_trees, n = 10)

trees_df <- merge(trees_df, trees_sum, by=c('species_parent'))

# all roads for dataviz
roads_all <- bbx %>%
  opq() %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

ggplot() +
  geom_sf(data = roads_all$osm_lines, col = 'grey70', size = .2) +
  geom_point(data = trees_df, aes(x = coords.x1, y = coords.x2, color = species_parent), 
             size = .3, alpha = .5) +
  theme_void() +
  scale_color_manual(values=met.brewer('Hokusai1', 10)) +
  labs(title = 'Trees of Toronto',
       subtitle = '10 most prevalent species in the city',
       caption = 'Data: open.toronto.ca | Palette: {MetBrewer}') +
  theme(plot.background=element_rect(fill = 'grey20', color=NA),
        panel.background = element_rect(fill = 'grey20', color=NA),
        plot.margin = unit(c(t=0,r=.2,b=0,l=.7), 'cm'),
        legend.position = 'right',
        legend.title = element_blank(),
        legend.text = element_text(family = 'roboto', color = 'grey80',
                                   size = 25),
        plot.caption = element_text(color = 'grey80', size = 25, 
                                    hjust = 0.5, vjust = .6, 
                                    family = 'roboto'),
        plot.title = element_text(color = '#DF7E66', size = 70,
                                  hjust = 0.14, vjust = .6, 
                                  family = 'roboto'),
        plot.subtitle = element_text(color = '#DF7E66', size = 35,
                                     hjust = 0.14, vjust = .6, 
                                     family = 'roboto')) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1))) 

ggsave('day1_points.png', height = 6.5, width = 8)
