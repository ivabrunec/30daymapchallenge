## Day 3: Polygons
## worldwide accuracy of smell recognition, data from 1986 NatGeo study
## Accessible through the pyrfume repo (see links below)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(readr)
library(mapdata)
library(MetBrewer)
library(zipcodeR)
library(showtext)

font_add_google(name = 'Monoton', family = 'Monoton')
font_add_google(name = 'Nixie One', family = 'Nixie One')
font_add_google(name = 'PT Mono', family = 'PT Mono')
showtext_auto()


# note that this is reading from an older commit
urlfile1="https://media.githubusercontent.com/media/pyrfume/pyrfume-data/27f9eba0e106cb27f7abe5bdc4a09a09360f40f7/nat_geo_1986/NGS.csv"
NGS_data <- read_csv(url(urlfile1))

urlfile2="https://raw.githubusercontent.com/pyrfume/pyrfume-data/main/nat_geo_1986/DataDictionary.csv"
data_dict <- read_csv(url(urlfile2)) 

# get usa data only
us_source <- filter(NGS_data, source == 2)
us <- map_data('state')

# add leading zero if necessary
us_source$ZIP <- stringr::str_pad(us_source$ZIP, 5, pad = "0")

us_source$zip_out <- ''
us_source$state <- ''
for (i in 1:nrow(us_source)){
  us_source$zip_out[i] <- reverse_zipcode(us_source$ZIP[i])$county
  us_source$state[i] <- reverse_zipcode(us_source$ZIP[i])$state
  print(i)%%10
}

# the loop above takes a while, wrote this to this csv
us_source <- read.csv('data/us_source_counties.csv')

us_source$subregion <- tolower(gsub(' County', '',us_source$zip_out))

state_abbr <- read.csv('data/state_abbr.csv')
us_source <- merge(us_source, state_abbr, 
                   by = c('state'))

# recode:
us_source$corr6[us_source$corr6 == 0] <- NA
us_source$corr6[us_source$corr6 == 2] <- 0

# remove PR and DC, not on the map
us_source <- filter(us_source, state != 'PR' & state != 'DC')

# calculate the difference between actual and perceived ability
# first, z-score each column because they're different ranges
us_source$corr_z <- scale(us_source$totcorr)
us_source$self_z <- scale(us_source$SELF_RATE_SMELL)

# now calculate the difference
us_source$diff <- us_source$corr_z - us_source$self_z

us_sum <- us_source |>
  group_by(region) |>
  dplyr::summarise(mean_corr = mean(totcorr), 
                   mean_self = mean(SELF_RATE_SMELL),
                   mean_diff = mean(diff))

us$corr_all <- us_sum$mean_corr[match(us$region,us_sum$region)]
us$mean_self <- us_sum$mean_self[match(us$region,us_sum$region)]
us$mean_diff <- us_sum$mean_diff[match(us$region,us_sum$region)]

# plot
col_pal <- colorRampPalette(c('#651b3b','#b23038', '#ff7f45', '#7dad82','#51bc96'))

ggplot(data = us) + 
  geom_polygon(aes(x = long, y = lat, group = as.factor(group), 
                   fill = mean_diff), 
               color = '#151a30', size = .2,) +
  scale_fill_gradientn(colours = rev(col_pal(10)),
                       limits = c(-.21, .23),
                       breaks = c(-.15, 0, .15),
                       labels=c('Worse sense of smell \nthan self-rated','Accurate','Better sense of smell \nthan self-rated')) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#1b0c1c', color = NA),
        plot.background = element_rect(fill = '#1b0c1c', color = NA),
        legend.position = 'bottom',
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.key.width = unit(dev.size()[1] / 5, 'inches'),
        legend.key.height = unit(dev.size()[2]/20, 'inches'),
        legend.title = element_blank(),
        legend.text = element_text(color = 'grey80', size = 20,
                                   lineheight = .25, family = 'PT Mono'),
        plot.title = element_text(color = '#ff7f45', size = 75,
                                  hjust = 0.5, vjust = 1, family = 'Monoton'),
        plot.subtitle = element_text(color = 'grey80', size = 25,
                                     hjust = 0.5, vjust = -.5, family = 'PT Mono',
                                     lineheight=.15),
        plot.caption = element_text(color = 'grey80', size = 18,
                                    family = 'PT Mono', hjust = .5)) +
  labs(title = 'United Scents of America',
       subtitle = 'In 1986, National Geographic surveyed 26,200 Americans about their self-reported and actual sense of smell.
       \n\nResidents of New Jersey, Nevada, and Massachusetts were most likely to overestimate their sense of smell.
       \nResidents of Idaho, New Mexico, and Kentucky were most likely to underestimate their sense of smell.',
       caption = 'Data: Nat Geo via {pyrfume}')

ggsave('day3_polygon.png', width = 6, height = 4, dpi = 400)

## plotting by country ####
# ended up not using this code but potentially useful
data_country <- filter(data_dict, VARNAME == 'COUNTRY') 

data_country <- as.data.frame(t(data_country)) 

colnames(data_country) <- data_country$V1[1]

data_country <- data_country %>%
  tidyr::separate(COUNTRY, into = c('COUNTRY', 'country_name'), sep = "=") %>%
  na.omit()

# append country codes, summarize measures of interest
NGS_data <- merge(NGS_data, data_country, by = c('COUNTRY'))
NGS_sum <- NGS_data |>
  group_by(country_name, COUNTRY) %>%
  add_tally() |>
  dplyr::summarise(mean_corr = mean(totcorr), 
                   mean_self_rate = mean(SELF_RATE_SMELL))

# difference between self-rated and actual smell accuracy
NGS_sum$diff <- NGS_sum$mean_self_rate - NGS_sum$mean_corr

# now add world map
world <- map_data("world")

# coalesce columns so subregions remain if relevant
world <- world %>% 
  mutate(subregion = coalesce(subregion, region))

# annoyingly, Trinidad & Tobago are separate
# combine
world$loc_name[world$subregion == 'Trinidad'] <- 'Trinidad & Tobago'
world$loc_name[world$subregion == 'Tobago'] <- 'Trinidad & Tobago'

# get unique country names to see which ones have to be updated
world_names <- as.data.frame(unique(world$subregion))

# rename countries that need to be renamed
# misspelled
NGS_sum$country_name[NGS_sum$country_name == 'Luxemborugh'] <- 'Luxembourg'
NGS_sum$country_name[NGS_sum$country_name == ' Trin & Tabago'] <- 'Trinidad & Tobago'

# bc data is from 1986, czechoslovakia & yugoslavia will be remapped to modern-day countries
# first I just add a label for one of the countries
# then i append rows as needed below
NGS_sum$country_name[NGS_sum$country_name == 'Czechos'] <- 'Czech Republic' # still not correct name
NGS_sum$country_name[NGS_sum$country_name == 'USSR'] <- 'Russia'
# there is no East Germany so West Germany will have to represent all of it
NGS_sum$country_name[NGS_sum$country_name == 'West Germany'] <- 'Germany'

# now for the countries that need to be replaced by multiple

# repeat yugoslavia x7
# & remove from the NGS dataset
yugoslavia <- filter(NGS_sum, country_name == 'Yogoslavia') |> # lol @ yogo
  slice(rep(1:n(), each = 7))
NGS_sum <- filter(NGS_sum, country_name != 'Yogoslavia')

yugoslavia$country_name <- c('Slovenia','Croatia','Serbia',
'Bosnia and Herzegovina','Montenegro','Kosovo','North Macedonia')

# we want to add Slovakia in addition to Czechia
slovakia <- filter(NGS_sum, country_name == 'Czech Republic')
slovakia$country_name <- 'Slovakia'

# USSR: we want to add other soviet republics
ussr <- filter(NGS_sum, country_name == 'Russia') |> # lol @ yogo
  slice(rep(1:n(), each = 14))

ussr$country_name <- c('Lithuania','Latvia','Estonia',
                       'Georgia','Azerbaijan','Armenia',
                       'Moldova', 'Belarus','Kazakhstan',
                       'Kyrgyzstan','Tajikistan','Uzbekistan',
                       'Turkmenistan','Ukraine')


NGS_sum <- rbind(NGS_sum, yugoslavia, slovakia, ussr)

# merge back with world data
world$diff <- NGS_sum$diff[match(world$region,NGS_sum$country_name)]
world$mean_corr <- NGS_sum$mean_corr[match(world$region,NGS_sum$country_name)]
world$mean_self_rate <- NGS_sum$mean_self_rate[match(world$region,NGS_sum$country_name)]

# now plot
ggplot(data = world) + 
  geom_polygon(aes(x = long, y = lat, group = group, fill=mean_self_rate), 
               color = 'white', size = .2) + 
  coord_fixed(1.3) +
  theme_minimal() 

