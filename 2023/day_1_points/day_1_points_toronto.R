# Day 1: Points.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(bertin)
library(showtext)

font_add_google(name = 'Ultra', family = 'Ultra')
font_add_google(name = 'Raleway', family = 'Raleway')
showtext_auto()

# from: https://open.canada.ca/data/en/dataset/a883eb14-0c0e-45c4-b8c4-b54c4a819edb
canada_base <- read_sf('data/lfsa000b16a_e.shp')
st_crs(canada_base)
canada_base$area_sqkm <- st_area(canada_base) / 1000000

areas <- cbind(canada_base$area_sqkm, canada_base$CFSAUID)
colnames(areas) <- c("area_sqkm","Geographic.code")
areas <- as.data.frame(areas)

# read in toronto postcodes and filter base file
toronto_postcodes <- read.table("data/;'/postcodes_toronto.csv",header=T,sep=",")

# append only toronto sq km areas
toronto_postcodes <- merge(toronto_postcodes, areas, by=c('Geographic.code'))
# calculate population / area
toronto_postcodes$area_sqkm <- as.numeric(as.character(toronto_postcodes$area_sqkm))
toronto_postcodes$pop_by_area <- toronto_postcodes$Population..2016 / toronto_postcodes$area_sqkm
toronto_postcodes$pop_by_area_log <- log(toronto_postcodes$pop_by_area)

# filter the base file by toronto postcodes only
toronto_base <- canada_base %>%
  filter(CFSAUID %in% toronto_postcodes$Geographic.code)

# now append the population normalized by area
toronto_base$population <- toronto_postcodes$pop_by_area[match(toronto_base$CFSAUID,toronto_postcodes$Geographic.code)]
toronto_base$population_log <- toronto_postcodes$pop_by_area_log[match(toronto_base$CFSAUID,toronto_postcodes$Geographic.code)]

# create point density diagram
regions_valued <- make_points(
  polygon=toronto_base,
  n=65, 
  square=TRUE 
)

x_coord <- c(-79.65)
y_coord <- c(43.77)
label_text <- c('Toronto')

label_df <- data.frame(x_coord, y_coord, label_text)

x_coord <- c(-79.646)
y_coord <- c(43.772)
label_text <- c('Toronto')

label_df_shadow <- data.frame(x_coord, y_coord, label_text)


ggplot()+
  geom_sf(toronto_base,
    mapping=aes(geometry=geometry),
    inherit.aes=FALSE, color = '#F49D37', linewidth = .4, fill = NA)+
  geom_sf(data = regions_valued,
          aes(size=population_log), color = '#140F2D')+
  scale_size(range=c(1,4))+
  coord_sf(crs='EPSG:4326') +
  geom_text(data = label_df_shadow, aes(x = x_coord, y = y_coord, label = label_text),
            size = 100, angle = 17, hjust=0, family = 'Ultra', color = '#F49D37') +
  geom_text(data = label_df, aes(x = x_coord, y = y_coord, label = label_text),
            size = 100, angle = 17, hjust=0, family = 'Ultra', color = '#F22B29') +
  labs(caption = 'Population density by postcode. | Data: StatCan.') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#dbd4d3', color = NA),
        legend.position = 'none',
        plot.caption = element_text(color = '#140F2D', size = 36, 
                                    family = 'Raleway',
                                    hjust = .5, vjust = 0))


ggsave('day_1_points_toronto.png', width = 10, height = 8)

