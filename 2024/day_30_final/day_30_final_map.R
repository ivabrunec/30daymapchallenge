# Day 30: The final map.
# connecting the termini of the london underground
# data from: https://github.com/oobrien/vis/blob/master/tubecreature/data/tfl_stations.json

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(osmdata)
library(sf)
library(ggplot2)
library(tidyr)
library(extrafont)
font_import()
loadfonts(device = 'win')
fonts()

stn_data <- jsonlite::fromJSON('data/tfl_stations.json', flatten = T)

features <- stn_data$features |> 
  as_tibble()

stn_sf <- features |>
  mutate(
    geometry = st_sfc(lapply(geometry.coordinates, function(coord) st_point(coord))),
    .keep = "unused"
  ) |>
  st_as_sf(crs = 4326)

# i assume there's a non-manual way to do this but i couldn't find a good dataset
stn_list <- list(
  "Bakerloo" = c("Harrow & Wealdstone", "Elephant & Castle"),
  "Central" = c("Epping", "Ealing Broadway", "West Ruislip"),
  "Circle" = c("Hammersmith", "Edgware Road"),
  "District" = c("Ealing Broadway", "Richmond", "Kensington Olympia", "Wimbledon", "Edgware Road", "Upminster"),
  "Hammersmith & City" = c("Hammersmith", "Barking"),
  "Jubilee" = c("Stanmore", "Stratford"),
  "Metropolitan" = c("Amersham", "Chesham", "Uxbridge", "Aldgate"),
  "Northern" = c("Edgware", "High Barnet", "Mill Hill East", "Battersea Power Station", "Morden"),
  "Piccadilly" = c("Heathrow Terminal 5", "Uxbridge", "Cockfosters"),
  "Victoria" = c("Brixton", "Walthamstow Central"),
  "Waterloo & City" = c("Waterloo", "Bank")
)

# get combos of terminal stations within each line
stn_combinations <- lapply(names(stn_list), function(line) {
  expand.grid(station1 = stn_list[[line]], station2 = stn_list[[line]]) |>
    filter(station1 != station2) |>
    mutate(line = line)
})

termini <- bind_rows(stn_combinations)

line_geometry_list <- list()
for (i in 1:nrow(termini)){
  station1 <- termini$station1[i]
  station2 <- termini$station2[i]
  
  geometry_station1 = stn_sf$geometry[stn_sf$properties.name == station1]
  geometry_station2 = stn_sf$geometry[stn_sf$properties.name == station2]
  line_geometry = st_sfc(st_linestring(rbind(st_coordinates(geometry_station1), st_coordinates(geometry_station2))))
  line_geometry_list[[i]] <- line_geometry
}

# combine into a single sfc object
lines_sfc <- do.call(c, line_geometry_list) 

# convert the combined sfc object into an sf object
lines_sf <- st_sf(geometry = lines_sfc, termini)

# okay now we need the colors
tfl_palette <- c("#b56307", "#e32018", "#fed303",
                 "#017929", '#f3a9bb',
                 '#a0a4a7', '#9c0157', '#010101',
                 '#013785', '#0199d5', '#97ccba')

# Assume we have a list of lines corresponding to these colors
line_names <- c("Bakerloo", "Central", "Circle",
                "District", "Hammersmith & City", "Jubilee")

# Create a named vector that links each line to its color
line_colors <- setNames(tfl_palette, line_names)

ggplot() +
  geom_sf(data = lines_sf, aes(color = line), size = .75) +
  scale_color_manual(values=tfl_palette) +
  theme_void() +
  theme(legend.title = element_blank(),
        plot.background = element_rect(fill = '#EFF7F6', color = NA),
        legend.text = element_text(family = 'London Tube'))

ggsave('day_30_final.png', width = 8, height = 6, dpi = 300)
  