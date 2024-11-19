## Day 19: Typography
# subway/metro stations w/ typography

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)
library(cowplot)
library(showtext)
library(magick)

library(extrafont)
font_import()
loadfonts(device = 'win')

# honestly this was a smorgasbord of me using system fonts and importing fonts

# specify list of city names + fonts
cities_df <- data.frame(
  city = c("London",
           "New York City",
           "Montréal",
           "Paris",
           "서울",
           "TORONTO"),
  font = c("London Tube",
           "Helvetica Neue",
           "Univers",
           "Parisine",
           "SeoulNamsan",
           "Toronto Subway"))

plots <- list()
for (i in 1:nrow(cities_df)){
  city_string <- cities_df$city[i]
  city_font <- cities_df$font[i]
  print(i)
  print(city_string)
  
  bbox <- getbb(city_string) 
  osm_data <- (opq(bbox = bbox) |>
             add_osm_feature(key = "route", value = "subway") |>
             add_osm_feature(key = "ref") |>
             osmdata_sf())$osm_lines 
  
  p <- ggplot() +
    geom_sf(data = osm_data,
      aes(color = mode),
      alpha = .6,
      color = '#E3655B') +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.position = 'none',
          axis.text = element_blank(),
          plot.background = element_rect(fill = '#F6F7EB', color = NA),
          plot.title = element_text(color = '#393E41', 
                                    family = city_font,
                                    size = 60,
                                    hjust = 0.1)) +
    labs(title = city_string)
  
  plots[[paste0("plot_", i)]] <- p
}

final_plot <- plot_grid(plotlist = plots, ncol = 2, align = 'v') +
  theme(plot.background = element_rect(fill = "#F6F7EB", colour = NA))

ggsave('day_19_typography.png', height = 10, width = 8)
