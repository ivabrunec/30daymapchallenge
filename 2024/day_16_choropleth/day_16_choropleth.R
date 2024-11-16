## Day 16: Choropleth.
# climate data from: https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/county/mapping
# following approach: https://github.com/VictimOfMaths/Maps/blob/master/SunVsRain.R

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(tigris)
library(sf)
library(dplyr)
library(gtools)
library(cowplot)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

# climate data: precipitation + temperature
precip_dat <- read.csv('data/data_annual_precipitation.csv', skip = 4)
temp_dat <- read.csv('data/data_annual_temp.csv', skip = 4)

# join climate data together
climate_dat <- left_join(precip_dat, temp_dat, by = c("ID", "Name", "State"), 
                         suffix = c("_precip", "_temp"))

# now read in us counties
counties <- counties(cb = TRUE, resolution = "5m")


# extract codes to allow us to merge with county data
climate_dat <- climate_dat |>
  mutate(
    STUSPS = substr(ID, 1, 2),
    COUNTYFP = substr(ID, 4, 6)
  )

# join spatial data
climate_map <- counties |>
  left_join(climate_dat, by = c("STUSPS", "COUNTYFP")) |>
  # filter out rows with NAs in Value_precip; this is arbitrary, could be any of the climate columns
  filter(!is.na(Value_precip))

climate_map$Value_precip <- as.numeric(climate_map$Value_precip)
climate_map$Value_temp <- as.numeric(climate_map$Value_temp)

# this part is directly lifted from:
# https://github.com/VictimOfMaths/Maps/blob/master/SunVsRain.R
# but instead of tertiles I'm doing quartiles of precipitation & temperature
climate_map <- climate_map |>
  mutate(
    rainquart = quantcut(Value_precip, q = 4, labels = FALSE),
    tempquart = quantcut(Value_temp, q = 4, labels = FALSE),
    
    key = case_when(
      rainquart == 1 & tempquart == 1 ~ 1,
      rainquart == 2 & tempquart == 1 ~ 2,
      rainquart == 3 & tempquart == 1 ~ 3,
      rainquart == 4 & tempquart == 1 ~ 4,
      rainquart == 1 & tempquart == 2 ~ 5,
      rainquart == 2 & tempquart == 2 ~ 6,
      rainquart == 3 & tempquart == 2 ~ 7,
      rainquart == 4 & tempquart == 2 ~ 8,
      rainquart == 1 & tempquart == 3 ~ 9,
      rainquart == 2 & tempquart == 3 ~ 10,
      rainquart == 3 & tempquart == 3 ~ 11,
      rainquart == 4 & tempquart == 3 ~ 12,
      rainquart == 1 & tempquart == 4 ~ 13,
      rainquart == 2 & tempquart == 4 ~ 14,
      rainquart == 3 & tempquart == 4 ~ 15,
      rainquart == 4 & tempquart == 4 ~ 16
    ),
    
    colour = case_when(
      key == 1 ~ "#f3f3f3", # lowest precipitation & temp
      key == 2 ~ "#d9e6f3",
      key == 3 ~ "#b4d3e1",
      key == 4 ~ "#509dc2", # low temp, high precip
      key == 5 ~ "#f3e6d9", # slightly warmer temp, gradient of precip
      key == 6 ~ "#cccccc",
      key == 7 ~ "#9fbfbf",
      key == 8 ~ "#376387",
      key == 9 ~ "#f3d1a6", # warm, low precip
      key == 10 ~ "#bf9f9f",
      key == 11 ~ "#808080",
      key == 12 ~ "#1f4f73", # warm, high precip
      key == 13 ~ "#f3b300",  # highest temp, low precip
      key == 14 ~ "#b36600",
      key == 15 ~ "#663300",
      key == 16 ~ "#000000"  # highest precipitation & temp
    )
  )

# generate dataframe for key
keydata <- climate_map %>%
  filter(!is.na(colour)) %>%
  group_by(rainquart, tempquart) %>%
  summarise(RGB=unique(colour))

# plot key, plot bivariate plot
key <- ggplot(keydata)+
  geom_tile(aes(x=rainquart, y=tempquart, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("rainier" %->%  ""),
       y = expression("hotter" %->%  "")) +
  # make font small enough
  theme(
    axis.title = element_text(size = 30), axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = 'grey80', color = NA))+
  # quadratic tiles
  coord_fixed()

bivar <- ggplot() +
  geom_sf(data = climate_map, aes(fill=colour, colour=colour))+
  scale_fill_identity()+
  scale_colour_identity() +
  labs(title = 'how hot/cold × dry/rainy is it where you are?') +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey80', color = NA),
        plot.title = element_text(size = 50, family = 'roboto', hjust = 0.5))

# combine & save
ggdraw() +
  draw_plot(bivar, 0, 0, 1, 1) + 
  draw_plot(key, 0.0, 0.1, 0.2, 0.2)

ggsave('day_16_choropleth_climate.png', width = 8, height = 5, dpi = 300)
  