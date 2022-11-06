## Day 6: Network.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(raster)
library(osmdata)
library(sf)
library(ggplot2)
library(tidyverse)
library(showtext)

font_add_google(name = 'PT Mono', family = 'PT Mono')
showtext_auto()

underground <- opq(bbox = 'london') |>
  add_osm_feature(key = 'railway', value = 'subway') |>
  osmdata_sf()

# just main roads
main_roads <- opq(bbox = 'london') |>
  add_osm_feature(key = 'highway',
                  value = c('motorway',
                            'trunk',
                            'primary',
                            'motorway_link',
                            'trunk_link',
                            'primary_link')) |>
  osmdata_sf()


ggplot() +
  geom_sf(data = main_roads$osm_lines)

# these data downloaded from https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london
limits <- st_read('data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp')
# combine boroughs into a single polygon
limits_outer <- limits |>
  summarise(geometry = sf::st_union(geometry)) 

# match crs to outer limit crs
underground$osm_lines <- underground$osm_lines |>
  st_transform(crs = crs(limits_outer))

main_roads$osm_lines <- main_roads$osm_lines |>
  st_transform(crs = crs(limits_outer))

underground_limit = st_filter(underground$osm_lines, limits_outer$geometry)

road_limit = st_filter(main_roads$osm_lines, limits_outer$geometry)

# stations from https://github.com/oobrien/vis/
stn_ids <- read.csv('data/london_combinedstations_630.csv') |>
  filter(category == 'LU')

stn_sf <- sf::st_as_sf(stn_ids, wkt = "wkt_geom" ) |>
  st_set_crs(crs(limits_outer))

# entry exit counts
stn_counts <- read.csv('data/station_entry_exit_counts.csv', header = F)
colnames(stn_counts) <- c('nlc_id', 'year', 'entry_exit', 'count')
stn_sf$nlc_id <- stringr::str_pad(stn_sf$nlc_id, 4, pad = "0")

# filter only year + entry_exit ?
stn_sum <- filter(stn_counts, 
                  year == '2014' & entry_exit != 'tot_yr')

stn_sum <- filter(stn_sum, entry_exit != 'in' & entry_exit != 'out' &
                    entry_exit != 'sat_in' & entry_exit != 'sat_out' &
                    entry_exit != 'sun_in' & entry_exit != 'sun_out')

stn_sum$order <- plyr::mapvalues(stn_sum$entry_exit, 
                               from=c("early_in","early_out",'am_in','am_out',"mid_in",'mid_out',
                                      'pm_in','pm_out', 'late_in', 'late_out'), 
                               to=c(1,2,3,4,5,6,7,8,9,10))


stn_sum$label <- plyr::mapvalues(stn_sum$entry_exit, 
                                 from=c("early_in","am_in","mid_in",'pm_in','late_in',
                                        'early_out','am_out','mid_out','pm_out','late_out'), 
                                 to=c('Early AM in','AM in','Mid-day in','PM in','Late PM in',
                                      'Early AM out','AM out','Mid-day out','PM out','Late PM out'))

stn_sum$order <- as.numeric(stn_sum$order)
stn_sf2 <- merge(stn_sf, stn_sum, by = c('nlc_id'))

separated_coord <- stn_sf2 %>%
  mutate(long = unlist(map(stn_sf2$geometry,1)),
         lat = unlist(map(stn_sf2$geometry,2)))

library(ggshadow)
library(gganimate)

separated_coord <- separated_coord |>
  group_by(name) |>
  mutate(z_score = scale(count)) |>
  ungroup()

col_pal <-colorRampPalette(c('#05668d','#007f5f','#aacc00','#ffff3f'))

a <- ggplot() +
  #geom_rect(data = separated_coord, aes(xmin = 495000, xmax = 565000, ymin = 150000, ymax = 208000, fill = order), 
  #          alpha = 0.5) +
  #colorspace::scale_fill_continuous_sequential(palette = rev('Batlow'), begin = 0, end = 1) +
  geom_sf(data = limits_outer$geometry, fill = 'grey30', color = 'grey20') +
  geom_sf(data = road_limit$geometry, color = 'grey20', size = .1) +
  geom_sf(data = underground_limit$geometry, color = 'grey70') +
  geom_glowpoint(data = separated_coord, aes(x = long, y = lat, color = z_score, size = count), shadowsize = .4) +
  scale_color_gradientn(colours = col_pal(10)) +  
  geom_text(data = separated_coord, aes(label = label),
            x = 510000, y = 200000,
            color = 'white', check_overlap = TRUE, family = 'PT Mono') +
  theme_void() +
  theme(plot.background = element_rect(fill = 'grey20', color = NA),
        panel.background = element_rect(fill = 'grey20', color = NA),
        legend.position = '',
        plot.caption = element_text(color = 'white',
                                    family = 'PT Mono',
                                    vjust = 10, hjust = 1)) +
  labs(caption = 'Data: github.com/oobrien/vis') +
  transition_states(order, wrap = F) +
  ease_aes('linear')

animate(a, 
        duration = 10,
        height = 600, width = 800)
anim_save('test.gif')
