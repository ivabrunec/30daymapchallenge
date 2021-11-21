## Day 21: Elevation.

# Plotting 9 most memorable trails of 2021.
# GPS data from AllTrails.com

# Get elevation raster for each bounding box, plot in temporal order starting with most recent

library(sf)
library(elevatr)
library(raster)
library(dplyr)
library(ggplot2)
library(cowplot)
library(extrafont)

loadfonts()

# read in gps traces ####
breakneck_gps <- read.csv("data/Beacon Fire Tower via Breakneck Ridge, Wilkinson and Notch Trails.csv")
lemonsqueeze_gps <- read.csv("data/Lemon Squeeze and Eagle Cliff.csv")
lehigh_gps <- read.csv("data/Lehigh Furnace Gap via Appalachian Trail.csv")
sunrise_gps <- read.csv("data/Sunrise Ridge Trail to Mount Angeles.csv")
ellinor_gps <- read.csv("data/Mount Ellinor Trail.csv")
hawkmt_gps <- read.csv("data/Hawk Mountain Loop.csv")
glenonoko_gps <- read.csv("data/Glen Onoko Falls and Lehigh Gorge Overlook Trail.csv")
oldrag_gps <- read.csv("data/Old Rag Mountain Loop.csv")
hawksbill_gps <- read.csv("data/Hawksbill Loop.csv")

# specify bounding boxes ####
breakneck_bb <- data.frame(x = c(-74, -73.92), y = c(41.43, 41.49))
lemonsqueeze_bb <- data.frame(x = c(-74.17, -74.13), y = c(41.74, 41.775))
lehigh_bb <- data.frame(x = c(-75.70, -75.60), y = c(40.74, 40.80))
sunrise_bb <- data.frame(x = c(-123.51, -123.45), y = c(47.96, 48))
ellinor_bb <- data.frame(x = c(-123.27, -123.23), y = c(47.500, 47.525))
hawkmt_bb <- data.frame(x = c(-76, -75.96), y = c(40.630, 40.650))
glenonoko_bb <- data.frame(x = c(-75.77, -75.75), y = c(40.880, 40.900))
oldrag_bb <- data.frame(x = c(-78.34, -78.28), y = c(38.54, 38.58))
hawksbill_bb <- data.frame(x = c(-78.405, -78.385), y = c(38.550, 38.560))

# get elevation rasters ####
prj_all <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

breakneck_elev <- get_elev_raster(breakneck_bb, z = 11, clip = 'bbox', prj = prj_all) 
breakneck_df <- as.data.frame(breakneck_elev, xy = TRUE)
colnames(breakneck_df) <- c('x','y','elev')

lemonsqueeze_elev <- get_elev_raster(lemonsqueeze_bb, z = 13, clip = 'bbox', prj = prj_all) 
lemonsqueeze_df <- as.data.frame(lemonsqueeze_elev, xy = TRUE)
colnames(lemonsqueeze_df) <- c('x','y','elev')

lehigh_elev <- get_elev_raster(lehigh_bb, z = 11, clip = 'bbox', prj = prj_all) 
lehigh_df <- as.data.frame(lehigh_elev, xy = TRUE)
colnames(lehigh_df) <- c('x','y','elev')

sunrise_elev <- get_elev_raster(sunrise_bb, z = 11, clip = 'bbox', prj = prj_all) 
sunrise_df <- as.data.frame(sunrise_elev, xy = TRUE)
colnames(sunrise_df) <- c('x','y','elev')

ellinor_elev <- get_elev_raster(ellinor_bb, z = 11, clip = 'bbox', prj = prj_all) 
ellinor_df <- as.data.frame(ellinor_elev, xy = TRUE)
colnames(ellinor_df) <- c('x','y','elev')

hawkmt_elev <- get_elev_raster(hawkmt_bb, z = 11, clip = 'bbox', prj = prj_all) 
hawkmt_df <- as.data.frame(hawkmt_elev, xy = TRUE)
colnames(hawkmt_df) <- c('x','y','elev')

glenonoko_elev <- get_elev_raster(glenonoko_bb, z = 11, clip = 'bbox', prj = prj_all) 
glenonoko_df <- as.data.frame(glenonoko_elev, xy = TRUE)
colnames(glenonoko_df) <- c('x','y','elev')

oldrag_elev <- get_elev_raster(oldrag_bb, z = 11, clip = 'bbox', prj = prj_all) 
oldrag_df <- as.data.frame(oldrag_elev, xy = TRUE)
colnames(oldrag_df) <- c('x','y','elev')

hawksbill_elev <- get_elev_raster(hawksbill_bb, z = 11, clip = 'bbox', prj = prj_all) 
hawksbill_df <- as.data.frame(hawksbill_elev, xy = TRUE)
colnames(hawksbill_df) <- c('x','y','elev')

## plot ####

# save theme 
plottheme <- theme(aspect.ratio = 1,
      text=element_text(family="Georgia"),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(), 
      plot.title = element_text(margin = margin(b = 0), hjust=.5, size = 13),
      plot.margin = unit(c(t=0,r=0,b=1,l=0), "cm"),
      legend.position = c(.5, -0.02),
      legend.direction="horizontal",
      legend.key.height = unit(.1, 'cm'), 
      legend.title=element_blank(),
      legend.background=element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(family = 'Arial', size = 10),
      plot.background = element_blank(),
      panel.background = element_blank())
      #plot.background=element_rect(fill = 'white', color=NA),
      #panel.background = element_rect(fill = 'white', color=NA)) 



# breakneck ridge
breakneck <- ggplot() +
  geom_raster(data = breakneck_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = breakneck_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = breakneck_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey20','white'), guide= 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8"), breaks = c(100, 250, 400)) +
  ggtitle('Breakneck Ridge Loop, NY') +
  plottheme
  
# lemon squeeze
lemonsqueeze <- ggplot() +
  geom_raster(data = lemonsqueeze_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = lemonsqueeze_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = lemonsqueeze_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8"), breaks = c(180, 280, 380)) +
  ggtitle('Lemon Squeeze & The Labyrinth, NY') +
  plottheme
  

# lehigh
lehigh <- ggplot() +
  geom_raster(data = lehigh_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = lehigh_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = lehigh_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide='none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8")) +
  ggtitle('Lehigh Furnace Gap, PA') +
  plottheme

# sunrise
sunrise <- ggplot() +
  geom_raster(data = sunrise_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = sunrise_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = sunrise_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8")) +
  ggtitle('Sunrise Ridge to Mount Angeles, WA') +
  plottheme

# ellinor
ellinor <- ggplot() +
  geom_raster(data = ellinor_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = ellinor_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = ellinor_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8"), breaks = c(900, 1200, 1500)) +
  ggtitle('Mount Ellinor, WA') +
  plottheme

# hawk mt
hawkmt <- ggplot() +
  geom_raster(data = hawkmt_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = hawkmt_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = hawkmt_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8"), breaks = c(250, 350, 450)) +
  ggtitle('Hawk Mountain, PA') +
  plottheme

# glen onoko
glenonoko <- ggplot() +
  geom_raster(data = glenonoko_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = glenonoko_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = glenonoko_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8")) +
  ggtitle('Glen Onoko Falls & Lehigh Gorge, PA') +
  plottheme

# old rag
oldrag <- ggplot() +
  geom_raster(data = oldrag_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = oldrag_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = oldrag_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8")) +
  ggtitle('Old Rag Mountain, VA') +
  plottheme

# hawksbill
hawksbill <- ggplot() +
  geom_raster(data = hawksbill_df, aes(x = x, y = y, fill = elev)) +
  geom_contour(data = hawksbill_df, aes(x = x, y = y, z = elev), colour = "white", size=.4) +
  geom_point(data = hawksbill_gps, aes(x = Longitude, y = Latitude, color= Elevation)) +
  scale_fill_gradientn(colors=c('grey40','white'), guide = 'none') +
  scale_color_gradientn(colors=c("#72efdd","#7400b8"), breaks = c(1100, 1200)) +
  ggtitle('Hawksbill Loop, VA') +
  plottheme

# year label
title <- ggdraw() + draw_label("2021", fontface='bold', fontfamily = "Elephant", size = 40)


# combine plots! 
plot_grid("","","",
      breakneck, lemonsqueeze, lehigh,
          sunrise, ellinor, hawkmt, 
          glenonoko, oldrag, hawksbill,
          "",title,"",
          nrow = 5, ncol = 3, 
          rel_heights = c(.07, 1, 1, 1, .5)) + 
  theme_grey()

ggsave('day21_elevation.png', height = 11, width = 10)
