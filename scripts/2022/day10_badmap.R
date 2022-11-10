
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(showtext)

font_add_google(name = 'Roboto Condensed', family = 'Roboto Condensed')
showtext_auto()

# data from https://download.geonames.org/export/dump/
data <- read.table('data/AT/AT.txt', header = F, fill = T, sep = '\t')

bad_data <- filter(data, V7 == 'P')
bad_data <- bad_data |>
  filter(stringr::str_detect(V2, 'Bad'))

aust_geom <-
  giscoR::gisco_get_countries(resolution = "20", country = "Austria")

bad_data$V5 <- as.numeric(bad_data$V5)
bad_data$V6 <- as.numeric(bad_data$V6)

ggplot()+
  geom_sf(data = aust_geom, fill = 'grey60', color = '#bcabae') +
  geom_point(data = bad_data, aes(x = V6, y = V5 - .05)) +
  geom_text(data = bad_data, aes(x = V6, y = V5+.02, label = V2),
            size = 6, color = '#2d2e2e', family = 'Roboto Condensed') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#DAC8CB', color = NA),
        plot.title = element_text(hjust = .5, color = '#2d2e2e',
                                  family = 'Roboto Condensed', size = 20)) +
  labs(title = 'This is a Bad map')

ggsave('day10_badmap.png', width = 6, height = 3, dpi = 300)
