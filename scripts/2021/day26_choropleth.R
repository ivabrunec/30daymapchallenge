## Day 26: Choropleth.

# mapping NBA wins by state/province
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(plyr)
library(dplyr)
library(sf)
library(mapdata)
library(raster)

can_us <- map_data('world') %>%
  filter(., region == "Canada" | region == "USA")

us <- getData("GADM", country = "USA", level = 1) %>%
  st_as_sf(us)
canada <- getData("GADM", country = "CAN", level = 1) %>%
  st_as_sf(canada)

states <- c('Arizona','California','Colorado','Florida','Georgia',
            'Illinois','Indiana','Louisiana','Massachusetts',
            'Michigan','Minnesota','New York', 'North Carolina',
            'Ohio','Oklahoma','Oregon','Pennsylvania','Tennessee',
            'Texas','Utah','Wisconsin')
provinces <- c('Ontario')
us.states <- us[us$NAME_1 %in% states, ]
ca.provinces <- canada[canada$NAME_1 %in% provinces, ]

us_can <- rbind(us.states, ca.provinces)

nba_wins <- read.csv("data/state_prov_nba.csv")
nba_sum <- nba_wins %>%
  group_by(state) %>%
  dplyr::summarise(count_wins = n())
colnames(nba_sum) <- c("NAME_1","count_wins")

nba_2 <- merge(us_can, nba_sum, by=c("NAME_1"), all=T)

ggplot()+
  geom_polygon(data = can_us, aes(x = long, y = lat, group=group), fill=NA, color='white')+
  geom_sf(data = nba_2, aes(fill=count_wins), color = NA) +
  coord_sf(xlim = c(-130, - 70), ylim=c(25,60)) +
  scale_fill_gradientn(colors = c('#ABC3AC','#FDD78D', '#FD7515', '#FD5017'), na.value = 'grey60') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.key.height = unit(.2, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=9, color='white'),
        legend.background=element_blank(),
        panel.background = element_rect(fill = 'grey30', color = 'grey30'),
        plot.background = element_rect(fill = 'grey30', color = 'grey30')) 
  
ggsave('day26_choropleth.png', width = 6, height = 5, dpi= 400)

## the toronto raptors ##
can_only <- rbind(ca.provinces)

nba_3 <- merge(can_only, nba_sum, by=c("NAME_1"), all=T)

ggplot()+
  geom_polygon(data = can_us, aes(x = long, y = lat, group=group), fill=NA, color='white')+
  geom_sf(data = nba_3, fill ='#ABC3AC', color = NA) +
  coord_sf(xlim = c(-130, - 70), ylim=c(25,60)) +
  scale_fill_gradientn(colors = c('#ABC3AC','#FDD78D', '#FD7515', '#FD5017'), na.value = 'grey60') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.key.height = unit(.2, 'cm'), 
        legend.key.width = unit(.4, 'cm'),
        legend.title=element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size=9, color='white'),
        legend.background=element_blank(),
        panel.background = element_rect(fill = 'grey30', color = 'grey30'),
        plot.background = element_rect(fill = 'grey30', color = 'grey30')) 

ggsave('day26_choropleth_raptors.png', width = 6, height = 5, dpi= 400)
