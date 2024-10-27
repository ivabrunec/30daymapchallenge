## Day 21: Movement.

# map of traces from Google location history

library(dplyr) # data wrangling
library(lubridate) # date parsing
library(ggplot2) # plotting
library(jsonlite) # reading in json file
library(sf) # spatial
library(osmdata) # osm api
library(sfnetworks) # spatial
library(showtext) # fonts

# switch to current wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# add font
font_add_google(name = "Inter", family = "Inter")
showtext_auto()

# read in file
myHistory <- jsonlite::fromJSON("Takeout2/Location History/Records.json", flatten=TRUE)

# data cleaning & transformations
# based on https://medium.com/geekculture/explore-your-activity-on-google-with-r-how-to-analyze-and-visualize-your-location-history-2ea8edabe733
# grab just locations
myData <- myHistory$locations

myData <- myData %>% 
  filter(activity!="NULL")

# clean up dates and lat/long
myData <- myData %>% 
  mutate(time  = as_datetime(myData$timestamp),
         date = date(time),
         hour  = paste(hour(time),minute(time),sep=":"),
         week = isoweek(time),
         month = paste(month(time)),
         year = isoyear(time),
         latitude = latitudeE7/1e7,
         longitude= longitudeE7/1e7) %>%
  dplyr::select(-timestamp,-latitudeE7,-longitudeE7)


# for toronto: everything after the end of April
to_data <- filter(myData, date > '2022-04-30')

# add underlying osm map

# for toronto:
bbx = c(-79.54, 43.59,
        -79.31, 43.76)

# all roads for dataviz
# takes a while
roads_all <- bbx %>%
  opq() %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# larger roads only to snap to network
# takes a second to run
highways <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      'road',
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link",
      'unclassified'
    )
  ) %>%
  osmdata_sf()

# plot
to_filtered <- filter(to_data, longitude < -79.30 & longitude > -79.54,
                      latitude < 43.76 & latitude > 43.59)

to_filtered$datetime <- as.Date(to_filtered$time)

# plot toronto
library(gganimate)
library(av)
a <- ggplot()+
  geom_sf(data = highways$osm_lines, col = 'grey40', size = .1) +
  #geom_sf(data = st_as_sf(filtered, "edges"), col = "#339999") +
  geom_point(data = to_filtered, aes(x = longitude, y = latitude), 
             size = 2, col = '#75e8ad') +
  theme_void() +
  theme(plot.background=element_rect(fill = 'grey20', color=NA),
        panel.background = element_rect(fill = 'grey20', color=NA),
        plot.margin = unit(c(t=.2,r=.2,b=1,l=.2), "cm"),
        plot.caption = element_text(color = "#75e8ad", size = 14, 
                                    hjust = .5, family = "Inter")
  )+
  transition_states(to_filtered$datetime) +
  labs(caption = "Where did you come from, where did you go: {closest_state}")

a

b <- animate(a, duration = 20, height = 600, width = 800,
             fps = 60, renderer = av_renderer())
anim_save("day23_clip_2.mp4", b)
