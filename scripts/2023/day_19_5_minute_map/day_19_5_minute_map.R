## 5 minute map.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(elevatr)
library(raster)
library(metR)
library(showtext)

font_add_google(name = 'Alfa Slab One', family = 'Alfa')
showtext_auto()

# coordinates
tnp <- data.frame(x = c(13.638851248180606, 14.281684672839901), 
                  y = c(46.198931482208536, 46.4761230337486))

elev <- get_elev_raster(tnp, z = 8, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")

elev_df <- as.data.frame(rasterToPoints(elev))
colnames(elev_df) <- c('x','y','z')

color_pal <- c("#59322b", 
               "#a15334", 
               "#b27f4d", 
               "#87aa74", 
               "#75c398", 
               "#afc5b7")

ggplot(elev_df, aes(x, y)) +
  geom_contour_fill(aes(z = z)) +
  geom_contour_tanaka(aes(z = z)) +
  scale_fill_gradientn(colors = color_pal) +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(color = '#afc5b7', size = 60, 
                                  hjust = .5, family = 'Alfa'),
        plot.background = element_rect(fill = 'white', color = NA)) +
  labs(title = 'Triglav National Park') 

ggsave('day_19_5_min.png', dpi = 300, width = 8, height = 4)

# just a few refinements, add 1-2 mins
ggplot(elev_df, aes(x, y)) +
  geom_contour_fill(aes(z = z)) +
  geom_contour_tanaka(aes(z = z)) +
  scale_fill_gradientn(colors = color_pal) +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(color = '#59322b', size = 145, 
                                  hjust = .5, family = 'Alfa',
                                  vjust = 0),
        plot.background = element_rect(fill = '#afc5b7', color = NA)) +
  labs(title = 'Triglav National Park') 

ggsave('day_19_6_min.png', dpi = 300, width = 8, height = 6)
  