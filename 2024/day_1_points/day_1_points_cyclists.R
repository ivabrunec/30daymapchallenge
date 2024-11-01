## Day 2: Cyclist injuries and deaths in Toronto
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(osmdata)
library(lubridate)
library(stringr)
library(showtext)
library(magick)

font_add_google(name = 'Inconsolata', family = 'inconsolata')
showtext_auto()

dat <- read.csv('data/CYCLIST_KSI_7491207683449996246.csv')

# extract year from date
dat <- dat |>
  mutate(year = year(mdy_hms(DATE))) |>
  mutate(year_group = cut(
    year,
    breaks = c(2005, 2011, 2017, 2023),
    labels = c("2006 - 2011", "2012 - 2017", "2018 - 2023"),
    right = TRUE
  ))

# identify cyclist + driver involved accidents, cyclist injury status
cyclist_driver_cases <- dat |>
  group_by(ACCNUM) |>
  filter(any(INVTYPE == "Cyclist") & any(INVTYPE == "Driver")) |>
  ungroup() |>
  # get cyclist injury status
  filter(INVTYPE == "Cyclist") |>
  mutate(ACCLASS = ifelse(ACCLASS == "Non-Fatal Injury", "Non-Fatal", ACCLASS))

# identify cyclist + pedestrian involved accidents, pedestrian injury status
cyclist_pedestrian_cases <- dat |>
  group_by(ACCNUM) |>
  filter(INVTYPE == 'Pedestrian') |>
  # recode to reflect pedestrian injury
  mutate(ACCLASS = ifelse(ACCLASS == "Non-Fatal Injury", "Pedestrian Non-Fatal", ACCLASS))

# separating out so the fatal cases can be plotted on top
cyclist_driver_nonfatal <- cyclist_driver_cases |>
  filter(ACCLASS == "Non-Fatal")

cyclist_driver_fatal <- cyclist_driver_cases |>
  filter(ACCLASS == "Fatal")

# checking duplicates, turns out this is mostly due to NAs in the ACCNUM column
duplicate_accnum_cases <- cyclist_driver_cases |>
  group_by(ACCNUM) |>
  filter(n() > 1) |>
  ungroup()

# grab min & max coordinates for plotting
min_lat <- min(dat$LATITUDE)
max_lat <- max(dat$LATITUDE)
min_long <- min(dat$LONGITUDE)
max_long <- max(dat$LONGITUDE)

# grab OSM map: all roads within bounding box
bbx = c(-79.61, 43.59,
        -79.13, 43.83)

roads <- bbx |>
  opq() |>
  add_osm_feature(key = "highway", 
                  value = c("primary", "trunk",
                            "secondary")) |>
  osmdata_sf()


parks <- bbx |>
  opq() |>
  add_osm_feature(key = "leisure", 
                  value = c("park")) |>
  osmdata_sf()

# some facts to display next to maps
# sources: 
# 10k in 2006 - https://spacing.ca/toronto/2007/01/07/best-bike-lane-2006/
# jarvis - https://x.com/observinthecity/status/1852406520656302254
# 6km cycle tracks - https://www.toronto.ca/wp-content/uploads/2019/01/94e8-Cycling-Implementation-Plan-Table-of-Contents-Section-1-4.pdf
# bloor - https://www.communitybikewaysto.ca/a-short-history-of-bike-lanes-on-bloor
# 96k - https://www.nationalobserver.com/2024/02/28/news/how-these-two-cities-are-making-wheels-turn-cyclists
# bike share - https://www.cbc.ca/news/canada/toronto/bike-share-toronto-ridership-record-2024-1

cycle_facts <- data.frame(
  label = c(
    '10km of bike lanes constructed in 2006. \n Bike Share Toronto begins operating in 2011.',
    "In 2015, only 6km of the network comprises cycle tracks. \n Pilot bike lanes on Bloor installed in 2016.",
    "More than 96km of new bike lanes installed 2018-2022. \n Record 5.7 million Bike Share trips in 2023."
  ),
  x = rep(-79.61, 3),
  y = rep(43.57, 3),
  year_group = c("2006 - 2011", "2012 - 2017", "2018 - 2023"),
  hjust = rep(0, 3)
) 

ggplot() +
  geom_sf(data = roads$osm_lines,
          color = 'grey', linewidth = .2) +
  geom_sf(data = parks$osm_polygons,
          fill = '#8cc0a2', color = NA) +
  geom_point(data = cyclist_driver_nonfatal,
             aes(x = LONGITUDE, y = LATITUDE,
                 color = ACCLASS),
             size = 1) +
  geom_point(data = cyclist_pedestrian_cases,
             aes(x = LONGITUDE, y = LATITUDE,
                 color = ACCLASS),
             size = 1) +
  geom_point(data = cyclist_driver_fatal,
             aes(x = LONGITUDE, y = LATITUDE,
                 color = ACCLASS),
             size = 2) +
  geom_text(data = cycle_facts,
            aes(x = x, y = y, label = label),
            size = 8.5,
            hjust = 0,
            family = 'inconsolata',
            lineheight = .25
  ) +
  scale_color_manual(values = c('#ED474A','grey30','cornflowerblue')) +
  facet_wrap(~year_group, nrow = 3) +
  theme_void() +
  theme(legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(family = 'inconsolata', 
                                   size = 30,
                                   color = 'grey20'),
        plot.background = element_rect(fill = 'grey92', color = NA),
        strip.text = element_text(hjust = 0.1,
                                  family = 'inconsolata',
                                  size = 50,
                                  color = 'grey30'),
        plot.title = element_text(family = 'inconsolata',
                                  hjust = 0.5,
                                  size = 70,
                                  color = 'grey20'),
        plot.subtitle = element_text(family = 'inconsolata',
                                     hjust = 0.2,
                                     size = 30,
                                     color = 'grey30')) +
  labs(title = "bike lanes save lives",
       subtitle = "cycling accidents in Toronto, 2006-2023")

ggsave('cyclist_maps.png', height = 10, width = 6, dpi = 300)

# number of injuries
all_cases <- rbind(cyclist_driver_cases, cyclist_pedestrian_cases)
injury_count <- all_cases |>
  group_by(year_group, ACCLASS) |>
  summarize(count = n())

# loop over year ranges to create a smaller inset graph for each range
for (i in levels(injury_count$year_group)){
  dat_temp <- filter(injury_count, year_group == i)
  ggplot(dat_temp, 
         aes(x = ACCLASS, y = count, fill = ACCLASS)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = count, color = ACCLASS), 
              position = position_dodge(width = 0.9), 
              vjust = -0.5,
              family = 'inconsolata',
              size = 30) +
    scale_fill_manual(values = c('red','grey30','cornflowerblue')) +
    scale_color_manual(values = c('red','grey30','cornflowerblue')) +
    theme_minimal() +
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_text(family = 'inconsolata',
                                     size = 30,
                                   color = 'grey20'),
          axis.text.x = element_blank()) +
    ylim(c(0, 360))
  
  plot_title <- paste0('injuries_deaths_', i, '.png')
  ggsave(plot_title, height = 3, width = 3, dpi = 300)
}

## combine images and add annotations
img <- image_read('cyclist_maps.png')
inset_1 <- image_read('injuries_deaths_2006 - 2011.png')
inset_2 <- image_read('injuries_deaths_2012 - 2017.png')
inset_3 <- image_read('injuries_deaths_2018 - 2023.png')

img_full <- image_composite(
  img, 
  image_scale(inset_1, 'x350'), 
  offset = '+1300+700'
) |>
  image_composite(
    image_scale(inset_2, 'x350'), 
    offset = '+1300+1600'
  ) |>
  image_composite(
    image_scale(inset_3, 'x350'), 
    offset = '+1300+2550'
  )

img_full
crop_width <- 1800 -  200 # original minus amount you want to crop
crop_height <- 3000

img_full_cropped <- image_crop(img_full, geometry = paste0(crop_width, "x", crop_height, "+200+0"))
img_full_cropped

image_write(img_full_cropped, 'day_1_points_cyclists.png')
