## Day 12: Population. 

# population of lynx, bear, wolves in Europe
# data from: https://datadryad.org/stash/dataset/doi:10.5061/dryad.pc866t1p3 

library(tidyverse)
library(sf)
library(ggplot2)
library(mapdata)

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

lynx <- read_sf("data/2020_03_Lynx_IUCN_RedList_SK_new.shp")
bear <- read_sf("data/2018_06_06_Bear_IUCN_RedList.shp")
wolf <- read_sf('data/2018_06_06_Wolf_IUCN_RedList.shp')

ggplot() +
  #geom_polygon(data = europe,aes(x=long, y=lat, group = group), fill= 'lightgrey', color='lightgrey') +
  geom_sf(data = lynx, color='darkorange', fill='darkorange', alpha=.2) +
  geom_sf(data = bear, color = 'firebrick3', fill='firebrick3', alpha=.2) +
  geom_sf(data = wolf, color='goldenrod', fill='goldenrod', alpha=.2)+
  coord_sf(crs = st_crs(4236), xlim = c(-17, 38),  ylim = c(32, 71)) 
  
  