## Day 6: Red.
# A map with red colour or a map about something red. 
# Wildfires in the US. Data from https://nifc.maps.arcgis.com/home/index.html 

# wildfire data
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)
library(mapproj)

wildfires <- st_read("data/AgencyHistoricFirePerimeters_2010_2019.shp")
wildfires_2020 <- st_read("data/AgecnyHistoricFirePerimeters_2020.shp")

wildfires <- rbind(wildfires, wildfires_2020); rm(wildfires_2020)

### my laptop can't handle gganimate with geom data so we'll make separate tiff files and combine them later
# combined into gif separately - this generates individual files
fire_list <- split(wildfires, f = wildfires$FIRE_YEAR)

usa <- map_data('state')

year=2010

for (i in fire_list){
  ggplot() +
    geom_polygon(data = usa, aes(x=long, y = lat, group = group ), color='white', fill= 'grey80') + 
    geom_sf(data = i$geometry, color='firebrick3', alpha = .4) +
    coord_sf(xlim = c(-130,-70), ylim = c(25, 50)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle(paste0(year))
  
  filename <- paste0(year, ".tiff")
  ggsave(filename, width=6, height=4)
  
  year=year+1
  
}

# calculate acres per year
acre_data <- wildfires[c('GIS_ACRES','FIRE_YEAR')]
acre_data <- st_set_geometry(acre_data, NULL) # remove geometry, coerce to data.frame

acre_sum <- acre_data %>%
  group_by(FIRE_YEAR) %>%
  summarise(total_acres = sum(GIS_ACRES, na.rm=T))

acre_sum <- na.omit(acre_sum)
ggplot(data = acre_sum, aes(x=FIRE_YEAR, y=total_acres)) +
  geom_bar(stat='identity')
