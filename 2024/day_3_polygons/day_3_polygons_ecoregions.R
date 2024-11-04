## Day 3: Polygons
# Ecoregions of North America
# data: https://www.epa.gov/eco-research/ecoregions-north-america
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Orelega One', family = 'orelega')
font_add_google(name = 'Poppins', family = 'poppins')
showtext_auto()

climate_dat <- read_sf('data/na_cec_eco_l1/NA_CEC_Eco_Level1.shp')

# code for this from https://github.com/gkaramanis/30DayMapChallenge/blob/main/2022/16-minimal-world/16-minimal-world.R
crs_string <- "+proj=ortho +lon_0=-90 +lat_0=40 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs"
climate_dat <- climate_dat |>
  st_transform(crs = crs_string)

world <- giscoR::gisco_countries
world <- world |>
  st_transform(crs = crs_string)

ocean <- st_point(x = c(-90, 40)) |>
  st_buffer(dist = 6370000) |>
  st_sfc(crs = crs_string)

climate_dat <- climate_dat |>
  mutate(NA_L1NAME = stringr::str_to_title(NA_L1NAME))

ecoregion_colors <- c("grey92",   # Water
                      "#c1c4e1",  # Arctic cordillera
                      "#8CC4D8",  # Tundra
                      "#6F9DCC",  # Taiga
                      "#FFB3B3",  # Hudson plain
                      "#6AD0B5",  # Northern forests
                      "#7abe6c",  # Northwestern forested mountains
                      "#306921",  # Marine west coast forest
                      "#B8E0B6",  # Eastern temperate forests
                      "#FFD700",  # Great plains
                      "#E2B55D",  # North american deserts
                      "#2e9568",  # Mediterranean california
                      "#FF7F50",  # Southern semiarid highlands
                      "#C45C3D",  # Temperate sierras
                      "#96A52A",  # Tropical dry forests
                      "#c63436")  # Tropical wet forests


# specify order of factor levels
order_mapping <- climate_dat |>
  distinct(NA_L1CODE, NA_L1NAME) |>
  mutate(NA_L1CODE = as.numeric(NA_L1CODE)) |>
  arrange(NA_L1CODE)

climate_dat <- climate_dat |>
  mutate(NA_L1NAME = factor(NA_L1NAME, 
                            levels = order_mapping$NA_L1NAME))

ggplot() +
  geom_sf(data = ocean, fill = 'grey92', color= NA) +
  geom_sf(data = world, color = NA, fill = 'grey75') +
  geom_sf(data = climate_dat,
          aes(fill = NA_L1NAME),
          color = NA) +
  scale_fill_manual(values = ecoregion_colors) +
  labs(title = 'ecoregions',
       subtitle = 'of North America') +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 25, family = 'poppins', color = '#272932'),
        plot.title = element_text(size = 160, family = 'orelega', hjust = 0, color = '#272932'),
        plot.subtitle = element_text(size = 40, family = 'poppins', hjust = 0.5, color = '#272932'),
        plot.background = element_rect(fill = '#FCFCFC', color = NA)) 

ggsave('day_3_ecoregions.png', width = 8, height = 8)
