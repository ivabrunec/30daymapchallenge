## Day 12: Population

# % of renters/houseowners by census division 

library(tidyverse)
library(sf)
library(ggplot2)

# read in base shapefile containing all census divisions 
canada_base <- read_sf("data/lcd_000b16a_e.shp") 

# now, get the centroid for each polygon
# we need this so we can provide the long/lat location for the population data
sf_cent <- st_centroid(canada_base)

# if you need to plot to double check!: just overlay the geom_sf(canada_base) and sf_cent
# there is some weirdness on the coasts because islands, but it's minor so i'll just ignore it

# read in the census data with division names and population renting/owning
canada_pop_all <- read.table("data/canada_population_2016_census.csv", header=T, sep=",", fill=T)
canada_pop <- read.csv("data/canada_dwelling_cond.csv")

# add lon and lat from the calculated centroids
sf_cent$lonlat <- stringr::str_remove_all(sf_cent$geometry, "[c()]")
# this throws a warning but it works

# split into lon and lat columns
sf_cent <- sf_cent %>% separate(lonlat, c("lon","lat"), sep = "(,)")
sf_cent$lon <- as.numeric(paste0(sf_cent$lon))
sf_cent$lat <- as.numeric(paste0(sf_cent$lat))

# now, append population to the extracted lon/lat coordinates so we can visualize
sf_cent$population <- canada_pop$Housing.Tenure...Owner....distribution.2016.[match(sf_cent$CDUID,canada_pop$ï..Geographic.code)]
sf_cent$population_density <- canada_pop_all$Population.density.per.square.kilometre..2016[match(sf_cent$CDUID,canada_pop_all$Geographic.code)]
#sf_cent$population <- as.numeric(as.character(sf_cent$population))
sf_cent$population_renters <- 100 - sf_cent$population
#sf_cent$pop_dens_log <- log(sf_cent$population_density)

ggplot()+
  geom_sf(data = canada_base$geometry, fill='black', color='grey36', size=.35) +
  geom_point(data = sf_cent, aes(x = lon, y = lat, size=population_density, color=population_renters), alpha=.9) +
  scale_color_gradientn(colors = c('turquoise4','lightgoldenrod1')) +
  scale_size_continuous(breaks = c(0,10,50,100,1000,2000,3000),range=c(2,20))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), legend.position="bottom",
        legend.key.height = unit(.15, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.background=element_blank(),
        legend.text=element_text(color='white',size=8),
        #plot.margin = unit(c(t=4,r=4,b=4,l=4), "cm"),
        plot.background=element_rect(fill = 'black', color=NA),
        panel.background = element_rect(fill = 'black', color=NA))+
       guides(size=FALSE)

ggsave('day12.png',width=7,height=7,dpi=400)

