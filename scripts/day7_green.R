## Day 7: Green.
# A map with green colour or a map about something green. 
# Forest coverage as & of land area. Data from https://ourworldindata.org/forest-area 
# Loss of forest per year, in ha (I calculated it as a % of total area for each country): 
# https://www.globalforestwatch.org/
  
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(mapproj)
library(dplyr)
library(cowplot)

world <- map_data("world")

forest_cover <- read.table("data/forest-area-as-share-of-land-area.csv", header = T, sep=",")
forest_cover_filtered <- filter(forest_cover, Year < 2021 & Year > 2009)
forest_cover_filtered <- forest_cover_filtered %>%
  dplyr::rename(region = Entity)

## rename countries to match across dataset & map
levels(forest_cover_filtered$region) <- c(levels(forest_cover_filtered$region), "USA")
forest_cover_filtered$region[forest_cover_filtered$region == 'United States'] <- 'USA'

levels(forest_cover_filtered$region) <- c(levels(forest_cover_filtered$region), "UK")
forest_cover_filtered$region[forest_cover_filtered$region == 'United Kingdom'] <- 'UK'

levels(forest_cover_filtered$region) <- c(levels(forest_cover_filtered$region), "Czech Republic")
forest_cover_filtered$region[forest_cover_filtered$region == 'Czechia'] <- 'Czech Republic'

levels(forest_cover_filtered$region) <- c(levels(forest_cover_filtered$region), "Democratic Republic of the Congo")
forest_cover_filtered$region[forest_cover_filtered$region == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'


## now read in year-by-year loss
tree_cover_loss <-read.table("data/treecover_loss_by_region__ha.csv", header=T, sep=",")
tree_cover_loss$region <- as.character(tree_cover_loss$region)
tree_cover_loss_filtered <- filter(tree_cover_loss, umd_tree_cover_loss__year > 2009 & umd_tree_cover_loss__year < 2021)

world2 <- map_data("world")

## rename countries to match
levels(tree_cover_loss_filtered$region) <- c(levels(tree_cover_loss_filtered$region), "USA")
tree_cover_loss_filtered$region[tree_cover_loss_filtered$region == 'United States'] <- 'USA'

levels(tree_cover_loss_filtered$region) <- c(levels(tree_cover_loss_filtered$region), "UK")
tree_cover_loss_filtered$region[tree_cover_loss_filtered$region == 'United Kingdom'] <- 'UK'

levels(tree_cover_loss_filtered$region) <- c(levels(tree_cover_loss_filtered$region), "Czech Republic")
tree_cover_loss_filtered$region[tree_cover_loss_filtered$region == 'Czechia'] <- 'Czech Republic'

levels(tree_cover_loss_filtered$region) <- c(levels(tree_cover_loss_filtered$region), "Democratic Republic of the Congo")
tree_cover_loss_filtered$region[tree_cover_loss_filtered$region == 'Democratic Republic of Congo'] <- 'Democratic Republic of the Congo'

## plot current year
year_current = 2020

forest_cover_current <- filter(forest_cover_filtered, Year == year_current)

world$forest_cover <- forest_cover_current$Forest.cover[match(world$region,forest_cover_current$region)]

ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=forest_cover), color = 'white', size = .2) + 
  coord_fixed(1.3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.15, 'cm'),
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        plot.title = element_text(size=12),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  scale_fill_gradientn(colors=c("#d8f3dc", "#99e2b4", "#248277", "#025353"),na.value='lightgrey', name='') +
  ggtitle(paste0('% forest cover ',year_current))

plot_title <- paste0(year_current,"_forest_coverage.png")
ggsave(plot_title,width=6,height=5)


## plot year by year
# this generates year-by-year images but can be combined into gif

for (year_current in c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)){
  
  tree_cover_loss_current <- filter(tree_cover_loss_filtered, umd_tree_cover_loss__year == year_current)
  
  world2$percentage_adjusted <- tree_cover_loss_current$percentage_adjusted[match(world2$region,tree_cover_loss_current$region)]
  
  
  ggplot(data = world2) + 
    geom_polygon(aes(x = long, y = lat, group = group, fill=percentage_adjusted), color = 'white', size = .2) + 
    coord_fixed(1.3) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), legend.position="bottom",
          legend.key.height = unit(.15, 'cm'), 
          legend.key.width = unit(.4, 'cm'),
          legend.title=element_text(size=8),
          legend.text=element_text(size=8),
          plot.title = element_text(size=10)) +
    #  scale_fill_gradientn(colors=c("#f2e9e4", "#9a8c98", "#4a4e69", "#22223b"),na.value='lightgrey', name='', limits = c(0,3.2)) +
    scale_fill_gradientn(colors=c("#d8f3dc", "#248277", "#025353"),na.value='lightgrey', name='', limits=c(0,3.2)) +
    ggtitle(paste0('% tree cover loss ', year_current))
  
  plot_title <- paste0(year_current,".png")
  ggsave(plot_title,width=8,height=4,dpi=300)
  
}