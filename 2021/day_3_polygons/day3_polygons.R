## Day 3: Polygons
# A map with polygons. <br>
# Data from: https://www.statista.com/statistics/444589/european-beer-consumption-per-capita-by-country/ 
# and https://en.wikipedia.org/wiki/List_of_countries_by_beer_consumption_per_capita. 
# For wine: https://ourworldindata.org/grapher/wine-consumption-per-person but this contains data from 2014.

library(ggplot2)
library(grid)
library(maps)
library(mapdata)
library(ggmap)
library(mapproj)
library(cowplot)
library(dplyr)

world <- map_data("world")

# make a list of European countries
europe_list <- c("Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Cyprus",
                 "Czech Republic","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia", "Kosovo",
                 "Lithuania","Luxembourg","Malta","Moldova","Montenegro", "Netherlands",
                 "Macedonia","Norway","Poland",
                 "Portugal","Romania","Russia","Serbia", "Slovakia","Slovenia","Spain",
                 "Sweden","Switzerland", "Turkey", "UK","Ukraine")

# filter out european countries
europe <- filter(world, region %in% europe_list) 

# read in beer data
beer_data <- read.table("data/europe_beer.csv", header=T, sep=",")

europe$beer_per_capita <- beer_data$beer_per_capita[match(europe$region,beer_data$region)]
europe$wine_per_capita <- beer_data$wine_per_capita[match(europe$region,beer_data$region)]
europe$spirits_per_capita <- beer_data$spirits_per_capita[match(europe$region,beer_data$region)]
europe$group <- as.factor(europe$group)
europe$region <- as.factor(europe$region)

beerplot <- ggplot(data = europe) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=beer_per_capita), color = 'white', size = .5) + 
  coord_fixed(1.3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.25, 'cm')) +
  coord_map(xlim = c(-17, 38),  ylim = c(32, 71)) +
  scale_fill_gradient(name = "", low = "#ee9b00", high = "#691e06", na.value = "grey50", limits = c(0,7)) 

wineplot <- ggplot(data = europe) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=wine_per_capita), color = 'white', size = .5) + 
  coord_fixed(1.3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.25, 'cm')) +
  coord_map(xlim = c(-17, 38),  ylim = c(32, 71)) +
  scale_fill_gradient(name = "", low = "#d62828", high = "#3f220f", na.value = "grey50", limits = c(0,7)) 

spiritplot <- ggplot(data = europe) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=spirits_per_capita), color = 'white', size = .5) + 
  coord_fixed(1.3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.25, 'cm')) +
  coord_map(xlim = c(-17, 38),  ylim = c(32, 71)) +
  scale_fill_gradient(name = "", low = "#63DDDD", high = "#001845", na.value = "grey50", limits = c(0,7)) 

# combine plots
plot_grid(wineplot, beerplot, spiritplot, nrow = 1)

ggsave('day3_europe', width = 7, height = 3, dpi = 300)