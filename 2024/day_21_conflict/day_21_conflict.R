## Day 21: Conflict
# Acute food insecurity: https://www.ipcinfo.org/fileadmin/user_upload/ipcinfo/manual/IPC_Technical_Manual_3_Final.pdf
# Data from https://data.humdata.org/dataset/global-acute-food-insecurity-country-data

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(sf)
library(rnaturalearth)
library(countrycode)
library(dplyr)
library(lubridate)
library(gganimate)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

dat <- read.csv('data/ipc_global_national_long_latest.csv', comment.char="#")
dat <- dat |>
  filter(Validity.period != 'first projection') |>
  filter(Phase == "3+")

# assign country names
dat$country_name <- countrycode(dat$Country, origin = "iso3c", destination = "country.name")

# grab world map & join polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

dat_world <- merge(dat, world, by.x = 'Country', by.y = 'iso_a3') |>
  st_as_sf()
dat_world <- st_transform(dat_world, crs = st_crs(world))

dat_world <- dat_world |>
  mutate(
    date = parse_date_time(Date.of.analysis, orders = "my"), 
    # create a factor ordered by unique sorted dates
    date_factor = factor(Date.of.analysis, levels = unique(Date.of.analysis[order(date)]))
  ) 

dat_world <- dat_world |>
  mutate(
    geometry = if_else(
      st_is_valid(geometry), 
      geometry, 
      st_make_valid(geometry)
    )
  )

dat_world <- dat_world |>
  mutate(
    is_valid = st_is_valid(geometry), 
    geometry = ifelse(!is_valid, st_make_valid(geometry), geometry)
  ) |>
  dplyr::select(-is_valid) |>
  st_as_sf() |>
  st_transform(dat_world, crs = st_crs(world)) # this throws an error but works? idk

dat_world <- dat_world |>
  mutate(
    centroid = st_centroid(geometry),
    centroid_x = st_coordinates(centroid)[, 1],
    centroid_y = st_coordinates(centroid)[, 2] 
  )


dat_temp <- filter(dat_world, date_factor == "Sep 2024")

a <- ggplot() +
  geom_sf(data = world,
          color = '#1C1D21', fill = '#5C5552') +
  geom_point(data = dat_world,
          aes(x = centroid_x, y = centroid_y,
              size = Percentage, group = date_factor,
              alpha = 1),
          color = '#F0544F') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#1C1D21', color = NA),
        plot.title = element_text(color = '#F7F9F9', size = 30, hjust = 0.5, family = 'roboto')) +
  transition_states(date_factor, wrap = TRUE, transition_length = 40, state_length = 30) +
  ease_aes('cubic-in-out') +
  labs(title = '{closest_state}') +
  enter_fade(alpha = 0) + 
  exit_fade(alpha = 0) 

library(gifski)
library(av)

gganimate::animate(a, fps = 10, height = 600, width = 1000, detail = 7, type = 'cairo', renderer = av_renderer(),
                   nframes = 200, start_pause = 20)
anim_save("day_21.mp4")
