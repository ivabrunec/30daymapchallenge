## Day 11: 3D.
# Plot population density in Toronto by postcode.
# data: https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/index-eng.cfm 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(dplyr)
library(ggplot2)
library(raster)
# calculate area of each polygon so we can plot normalized population.
# I was unable to figure out how to do this with a simple sf file, so I used the raster package to calculate it
# and then append the resulting area to the clean data frame
# it's not the most elegant, but it works (and doesn't take a lot of time to run).

# read in canada base file
canada_base <- shapefile("data/lfsa000b16a_e.shp", verbose=T) 

raster::crs(canada_base)
canada_base$area_sqkm <- raster::area(canada_base) / 1000000

# updated in 2023: there is a way to calculate this that is much faster
# in sf only
st_crs(canada_base)
canada_base$area_sqkm <- st_area(canada_base) / 1000000

areas <- cbind(canada_base$area_sqkm, canada_base$CFSAUID)
colnames(areas) <- c("area_sqkm","Geographic.code")
areas <- as.data.frame(areas)

# now read in the clean version of the canada shapefile
canada_base <- read_sf("data/lfsa000b16a_e.shp") 


# read in toronto postcodes and filter base file
toronto_postcodes <- read.table("data/postcodes_toronto.csv",header=T,sep=",")

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

# plot

# was grey30
to_plot <- ggplot() +
  geom_sf(data = toronto_base, aes(fill=population_log), color = 'grey30', size = .25) +
  scale_fill_gradientn(colors=c("#BAFCE9","#03045e")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="",
        legend.key.height = unit(.15, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_text(size=8),
        legend.text=element_text(size=8),
        plot.margin = unit(c(t=4,r=4,b=4,l=4), "cm"),
        plot.background=element_rect(fill = '#d8e2dc', color=NA),
        panel.background = element_rect(fill = '#d8e2dc', color=NA))

# background
to_plot


library(rayshader)
plot_gg(to_plot,width=7,height=5,scale=90,windowsize=c(1600,866),
       zoom = 0.16, phi = 50, theta=0,  sunangle = 95)

render_snapshot('day11', clear = F)

rayshader::render_highquality('day11_hires.png',samples=200)
