## Day 16: Minimal.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(dplyr)
library(elevatr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Squada One', family = 'Squada One')
showtext_auto()

# faroe islands
faroe_bb <- data.frame(x = c(-7.739681556714888, -5.8475310288244735), 
                       y = c(61.43629407900278, 62.47864911998253))

faroe_elev <- get_elev_raster(faroe_bb, z = 6, clip = 'bbox', 
                              prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 

faroe_df <- as.data.frame(faroe_elev, xy = TRUE)
colnames(faroe_df) <- c('x','y','elev')

faroe_above0 <- filter(faroe_df, elev > -5)

ggplot() +
  geom_contour(data = faroe_above0, aes(x = x, y = y, z = elev, color = ..level..), size=.25) +
  scale_color_gradientn(colors=c("#673c4f","#81d0b0"), breaks = c(0, 250, 400)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey10'),
        legend.position = '',
        plot.caption = element_text(color = "#A4C7AE", hjust = 0.05,
                                    family = 'Squada One', size = 60)) +
  labs(caption = 'Faroe Islands | Føroyar | Færøerne')

ggsave('day16_minimal.png', width = 6, height = 7, dpi = 400)


