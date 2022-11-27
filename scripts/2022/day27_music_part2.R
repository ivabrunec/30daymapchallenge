## Day 27: Music.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(dplyr)
library(elevatr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Syne Mono', family = 'Syne Mono')
font_add_google(name = 'Syne', family = 'Syne')
showtext_auto()

# see part 1 for converting .wav into spectrogram
# read in spectrogram as raster
spect <- raster('audio_spectro_cropped.tif')

# read in world countries
world_geom <-
  giscoR::gisco_get_countries(resolution = "20")

# get just the outlines
limits_outer <- world_geom |>
  summarise(geometry = sf::st_union(geometry)) 

# get extent
bb <- extent(limits_outer)
extent(spect) <- bb
spect <- setExtent(spect, bb, keepres=TRUE)

# crop & mask raster
r2 <- crop(spect, extent(limits_outer))
r3 <- mask(r2, limits_outer)

spect_df <- as.data.frame(rasterToPoints(r3))
colnames(spect_df) <- c('x','y','hz')

ggplot()+
  geom_tile(data = spect_df, aes(x = x, y = y, fill = hz)) +
  colorspace::scale_fill_continuous_sequential(palette = 'BuPu') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#010101', color = NA),
        legend.position = '',
        plot.title = element_text(family = 'Syne Mono', color = '#c8dfeb', 
                                  hjust = .1, size = 40,lineheight = 0.3),
        plot.caption = element_text(family  = 'Syne', color = '#c8dfeb',
                                    hjust = .95, size = 20)) +
  labs(title = "There is a crack, a crack in everything \nThat's how the light gets in",
       caption = 'Leonard Cohen: Anthem')

ggsave('day27_music.png', width = 6, height = 4, dpi = 300)
