## Day 22: Boundaries.

# plotting the boundaries of all european countries and US states
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(maps)
library(mapdata)
library(mapproj)
library(cowplot)
library(dplyr)
library(sf)

world <- map_data("world")

# make a list of European countries
europe_list <- c("Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina","Bulgaria","Croatia","Cyprus",
                 "Czech Republic","Denmark","Estonia","Finland","France",
                 "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia", "Kosovo",
                 "Lithuania","Luxembourg","Malta","Moldova","Montenegro", "Netherlands",
                 "Macedonia","Norway","Poland",
                 "Portugal","Romania","Serbia", "Slovakia","Slovenia","Spain",
                 "Sweden","Switzerland", "Turkey", "UK","Ukraine",
                 "Andorra", "Liechtenstein", "San Marino", "Monaco")

# not including russia because it's so huge everything else looks tiny

# filter out european countries
europe <- filter(world, region %in% europe_list) 
europe <- europe %>%
  group_by(group) %>%
  mutate(mean_lat = mean(lat), mean_long = mean(long)) %>%
  mutate(centered_lat = lat - mean_lat, centered_long = long - mean_long)

europe_sizes <- read.table("data/europe_size_rank.csv", header=T, sep=",")
europe_sizes$region <- as.character(europe_sizes$region)

europe$size_rank <- europe_sizes$rank[match(europe$region,europe_sizes$region)]

# now, plot outlines
europe_plot <- ggplot() + 
  geom_polygon(data = europe, aes(x = centered_long, y = centered_lat, group = group, color = size_rank), fill='grey40', alpha=.3, size = .7) + 
  scale_color_gradientn(colors =rev(c("#5FA2CF", "#81C1BC", "#F19D69", "#EB99B4"))) +
  coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        #legend.key.height = unit(.25, 'cm'),
        plot.margin = unit(c(t=1.5,r=0,b=1.5,l=0), "cm"),
        panel.background = element_rect(fill = 'grey30', color = 'grey30'),
        plot.background = element_rect(fill = 'grey30', color = 'grey30')) 

europe_plot

## plot individual countries in order of size
# sort by size
europe_sorted <- europe_sizes[order(-europe_sizes$rank),]

# initialize plot counter
plot_counter <- 1

for (i in 1:nrow(europe_sorted)){
  region_temp <- europe_sorted$region[i]
  poly_temp <- filter(europe, region == region_temp)
  
  plot_temp <- ggplot()+
    geom_polygon(data= poly_temp, aes(x = centered_long, y = centered_lat, group=group), color='lightgrey') +
    coord_fixed(1) +
    xlim(-10, 10) + ylim(-10, 10) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(), legend.position="",
          panel.background = element_rect(fill = 'grey30', color = 'grey30'),
          plot.background = element_rect(fill = 'grey30', color = 'grey30')) 
  
  nam <- paste("plot", plot_counter, sep = "_")
  assign(nam, plot_temp)  
  
  plot_counter = plot_counter+1
}

temp_plots <- plot_grid(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6, plot_7, plot_8, plot_9, plot_10,
          plot_11, plot_12, plot_13, plot_14, plot_15, plot_16, plot_17, plot_18, plot_19,
          plot_20, plot_21, plot_22, plot_23, plot_24, plot_25, plot_26, plot_27, plot_28,
          plot_29, plot_30, plot_31, plot_32, plot_33, plot_34, plot_35, plot_36, plot_37,
          plot_38, plot_39, plot_40, plot_41, plot_42, plot_43, plot_44, plot_45, plot_46,
          nrow = 2)
# these are too tiny
# i like them but it's not worth the hassle
temp_plots 

## us map
usa <- map_data("state")
al_hi <- c("Alaska", "Hawaii")
usa_plus <- map_data('world') %>%
  filter(., subregion %in% al_hi)
#usa <- rbind(usa, usa_plus)
# this works but alaska is so huge everything else looks tiny

usa <- usa %>%
  group_by(group) %>%
  mutate(mean_lat = mean(lat), mean_long = mean(long)) %>%
  mutate(centered_lat = lat - mean_lat, centered_long = long - mean_long)

usa_sizes <- read.table("data/us_states_size.csv", header=T, sep=",")
usa_sizes$region <- as.character(usa_sizes$region)

usa$size_rank <- usa_sizes$rank[match(usa$region,usa_sizes$region)]

# now, plot outlines
usa_plot <- ggplot() + 
  geom_polygon(data = usa, aes(x = centered_long, y = centered_lat, group = group, color = size_rank), fill='grey40', alpha=.3, size = .7) + 
  scale_color_gradientn(colors =rev(c("#5FA2CF", "#81C1BC", "#F19D69", "#EB99B4"))) +
  coord_fixed(1.3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        legend.key.height = unit(.25, 'cm'),
        panel.background = element_rect(fill = 'grey30', color = 'grey30'),
        plot.background = element_rect(fill = 'grey30', color = 'grey30')) 

usa_plot


cowplot::plot_grid(europe_plot, usa_plot, 
                   ncol = 2,
                   align = 'v')

ggsave('day22_boundaries.png', width = 10, height = 5, dpi = 400)
