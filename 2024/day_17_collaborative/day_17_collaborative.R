## Day 17: Collaborative map
# data from: https://open.toronto.ca/dataset/bike-share-toronto/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(jsonlite)
library(osmdata)
library(osrm)
library(showtext)

font_add_google(name = 'Abel', family = 'abel')
showtext_auto()

# not sure why this is so convoluted
bike_share_json <- fromJSON('data/bike-share-json.json')
station_info_url <- bike_share_json$data$en$feeds[bike_share_json$data$en$feeds$name == "station_information", "url"]

# download station info
station_info <- fromJSON(station_info_url)
station_data <- station_info$data$stations

# convert to geometry
station_data_sf <- st_as_sf(station_data, 
                            coords = c("lon", "lat"), 
                            crs = st_crs(iso))

ggplot() +
  geom_sf(data = station_data_sf)

# grab underlying streets
to_bbox <- st_bbox(station_data_sf)

roads <- to_bbox |>
  opq() |>
  add_osm_feature(key = "highway", 
                  value = c("primary", "trunk",
                            "secondary")) |>
  osmdata_sf()


# get info on travel time from the Union docking station
locations <- tibble::tribble(
  ~place,       ~lon,     ~lat,
  "Union", -79.38039, 43.64561)

iso <- osrmIsochrone(loc = c(locations$lon, locations$lat), 
                     breaks = seq(from = 0,to = 60, by = 10),
                     res = 40,
                     osrm.profile = "bike")
ggplot() +
  geom_sf(data = iso)

# now, count up the number of stations that fall within each isochrone
stations_with_iso <- st_join(station_data_sf, iso, join = st_within)

col_pal <- scico::scico(6, palette = 'buda')

ggplot() +
  geom_sf(data = roads$osm_lines,
          color = 'grey30', linewidth = 0.2) +
  geom_sf(data = iso, aes(fill = isomax), color = NA, alpha = 0.3) +
  geom_sf(data = stations_with_iso, aes(color = isomax), size = 0.4) +
  scale_fill_gradientn(colors = col_pal) +
  scale_color_gradientn(colors = col_pal, na.value = 'grey') +
  labs(title = "From here to everywhere",
       subtitle = "on two wheels",
       caption = "travel time (minutes) between Bike Share stations, starting at Union") +
  guides(color = "none")  +
  theme_void() +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key.height = unit(0.2, 'cm'),
        legend.text = element_text(color = 'grey90', family = 'abel', size = 30),
        plot.background = element_rect(fill = 'black', color = NA),
        plot.title = element_text(color = 'grey90', hjust = 0.5, 
                                  family = 'abel', size = 100),
        plot.subtitle = element_text(color = 'grey90', hjust = 0.75,
                                     family = 'abel', size = 50),
        plot.caption = element_text(color = 'grey90', hjust = 0.5, 
                                    family = 'abel', size = 40))


ggsave('day_17_collaborative.png', width = 10, height = 8, dpi = 300)



