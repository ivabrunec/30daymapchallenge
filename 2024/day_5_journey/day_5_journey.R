## Day 5: A journey.
# MTA hourly ridership
# data: https://data.ny.gov/Transportation/MTA-Subway-Hourly-Ridership-Beginning-July-2020/wujg-7c2s/about_data
# subway lines: https://data.cityofnewyork.us/Transportation/Subway-Lines/3qz8-muuu

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(sf)
library(dplyr)
library(gganimate)
library(showtext)

font_add_google(name = 'Roboto Mono', family = 'roboto')
showtext_auto()

# subway network
subway_net <- read_sf('data/Subway Lines.geojson')

# data downloaded for sept 1 2024
# there is an API but I was lazy
mta_dat <- read.csv('data/MTA_Subway_Hourly_Ridership__Beginning_July_2020_20241104.csv')
mta_dat <- mta_dat |>
  filter(transit_mode == "subway")

# summarize total number of journeys per station, per hour
# in raw data they are split by payment type
mta_dat_sum <- mta_dat |>
  group_by(transit_timestamp, station_complex_id, station_complex, latitude, longitude) |>
  summarize(total_trips = sum(ridership)) |>
  # turn timestamp into usable index
  mutate(parsed_time = as.POSIXct(transit_timestamp, format = "%m/%d/%Y %I:%M:%S %p", tz = "America/New_York")) |>
  mutate(time_only = format(parsed_time, "%I %p")) |>
  ungroup() |>
  mutate(rank = dense_rank(parsed_time)) |>
  # get top station per hour & clean up name
  group_by(transit_timestamp) |>
  mutate(top_station = station_complex[which.max(total_trips)]) |> 
  mutate(top_station = sub("\\s*\\(.*", "", top_station)) |>
  ungroup() 

# take two specific times: 9AM, 9PM
mta_temp <- mta_dat_sum |>
  filter(time_only == "12 PM" | time_only == "12 AM")

ggplot() +
  geom_sf(data = subway_net,
          color = '#333333',
          linewidth = 0.2) +
  geom_point(data = mta_temp,
             aes(x = longitude, y = latitude, 
                 size = total_trips, group = time_only, color = total_trips)) +
  scale_color_gradient(low = "#FF9F1C", high = '#E71D36') +
  theme_void() +
  geom_text(data = mta_temp,
            aes(x = -74.1, y = 40.9, label = time_only),
            hjust = 0, family = 'roboto', size = 20, color = '#333333') +
  geom_text(data = mta_temp,
            aes(x = -74.1, y = 40.88, label = top_station), 
            hjust = 0, family = 'roboto', size = 20, color = '#333333') +
  facet_wrap(~time_only) +
  theme_void() +
  theme(plot.background = element_rect(fill = '#C4E7D4', color = NA),
        legend.position = 'none',
        panel.grid = element_line(color = 'grey', linewidth = 0.1, linetype = 'dashed'))

ggsave('day_15_mta.png', height = 8, width = 13, dpi = 300)


# now animate
# order times by rank
mta_dat_sum <- mta_dat_sum |>
  mutate(time_only = factor(time_only, levels = unique(time_only[order(rank)]))) 

a <- ggplot() +
  geom_sf(data = subway_net,
          color = '#333333',
          linewidth = 0.2) +
  geom_point(data = mta_dat_sum,
             aes(x = longitude, y = latitude, 
                 size = total_trips, group = time_only, color = total_trips)) +
  scale_color_gradient(low = "#FF9F1C", high = '#E71D36') +
  labs(title = 'Time: {closest_state}') +
  theme_void() +
  theme(plot.background = element_rect(fill = '#C4E7D4', color = NA),
        legend.position = 'none',
        plot.title = element_text(color = '#333333', family = 'roboto', size = 20,
                                  hjust = 0.1)) +
  transition_states(time_only, wrap = T, transition_length = 20, state_length = 16) +
  ease_aes('linear') 

library(gifski)
library(av)

animate(a, fps = 10, height = 800, width = 800, detail = 7, type = 'cairo', renderer = av_renderer())
anim_save("day_5.mp4")
