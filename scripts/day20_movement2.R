## Day 20: Movement.
# Blue & fin whale tracking data: https://animalbiotelemetry.biomedcentral.com/articles/10.1186/s40317-020-00207-x
# https://www.datarepository.movebank.org/handle/10255/move.1065

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(mapdata)
library(raster)

whales <- read.table("data/Blue and fin whales Southern California 2014-2015 - Argos data.csv", header=T, sep=",")

blue_whales <- filter(whales, individual.taxon.canonical.name == "Balaenoptera musculus")

blue_whales <- filter(blue_whales, location.long > -125 & location.long < -116 & location.lat < 41)
# filter out specific outlier
blue_whales <- filter(blue_whales, location.lat != 35.51010)

# pick out the whales with the longest trajectories
blue_whales_viz <- filter(blue_whales, 
                          #tag.local.identifier == "2015CA-MK10-00838" |
                            tag.local.identifier == "2015CA-MK10-04177" )
                           # tag.local.identifier == "2015CA-MK10-05650")

states <- map_data("state")
california <- subset(states, region %in% c("california",'nevada'))
mexico <- map_data("world") %>%
  filter(., region == "Mexico")

# combine the country polygons
countries_subset <- rbind(california, mexico)

# bathymetry
# https://neo.gsfc.nasa.gov/view.php?datasetId=GEBCO_BATHY
bath_raster <- raster("data/GEBCO_BATHY_2002-01-01_rgb_3600x1800.tiff")
bath_matrix <- as.data.frame(rasterToPoints(bath_raster))
colnames(bath_matrix) <- c("x","y","bath")

# subset the bathymetry matrix
bath_matrix_small <- filter(bath_matrix, (x > -127 & x < - 115) & (y > 30 & y < 40))

ggplot() +
  geom_raster(data = bath_matrix_small, aes(x=x, y=y, fill=bath)) +
  geom_polygon(data = countries_subset, aes(x=long, y = lat, group = group ), fill= 'grey80', color='grey80') + 
  #geom_point(data = blue_whales, aes(x = argos.lon2, y = argos.lat2, group = tag.local.identifier, color = tag.local.identifier), size=1) +
  geom_path(data = blue_whales_viz[order(blue_whales_viz$timestamp),], aes(x = argos.lon2, y = argos.lat2, group=tag.local.identifier, color = event.id), size = 2)+
  coord_fixed(1.3) +
  coord_sf(xlim = c(-125, -115),  ylim = c(30, 40)) +
  scale_fill_gradientn(colors=c("navyblue", "paleturquoise4"))+
  scale_color_gradientn(colors=c('goldenrod','#F27A7A')) +
  theme_minimal() 
#theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#      axis.title.x=element_blank(),
#       axis.text.x=element_blank(),
#       axis.ticks.x=element_blank(),
#       axis.title.y=element_blank(),
#       axis.text.y=element_blank(),
#       axis.ticks.y=element_blank(), legend.position="")
