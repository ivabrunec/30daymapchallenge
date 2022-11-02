## Day 2: Lines

# joyplot of Canadian elevation + population
# following this tutorial: https://www.helenmakesmaps.com/post/how-to-joy-plot

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(ggridges)
library(dplyr)
library(raster)
library(showtext)

font_add_google(name = 'DM Serif Display', family = 'DM Serif Display')
showtext_auto()

elev <- giscoR::gisco_get_countries(country = 'Canada') |>
  elevatr::get_elev_raster(z = 3, clip = 'location')

elev <- aggregate(elev, fact=3)


# convert from raster to matrix
elmat <- rasterToPoints(elev)
colnames(elmat) <- c('x','y','z')
elmat <- as.data.frame(elmat)

# plot elevation as heatmap, double check it looks okay
ggplot() +
  geom_raster(data = elmat , aes(x = x, y = y, fill = z)) + 
  coord_quickmap()

canada_geom <-
  giscoR::gisco_get_countries(resolution = "20", country = "Canada")

# joyplot
ggplot() +
  geom_density_ridges(data = elmat, aes(x = x, y = y, group = y, height = z,
                                        fill = y),
                      stat = 'identity', color = 'grey20', 
                      size = .2, scale = 20, rel_min_height = .04) +
  scale_fill_gradient(low = "#9497a0", high = "#e17e5f") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'grey80', color = NA),
        plot.background = element_rect(fill = 'grey80', color = NA),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = unit(c(t=1,r=0,b=0,l=0), 'cm'),
        plot.title = element_text(color = 'grey20', size = 60,
                                  hjust = 0.5, family = 'DM Serif Display'),
        #plot.subtitle = element_text(color = 'grey20', size = 10,
        #                             hjust = 0.5, family = 'DM Serif Display'),
        legend.position = ''
  ) +
  labs(title = "41°40'53'' N - 83°6'41'' N")+
  coord_fixed(1.4) 

ggsave('day2_lines.png', width = 7, height = 5, dpi = 300)  
