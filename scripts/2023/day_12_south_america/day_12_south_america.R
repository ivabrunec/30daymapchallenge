## tierra del fuego
 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(elevatr)
library(showtext)

font_add_google(name = 'Shrikhand', family = 'Shrikhand')
font_add_google(name = 'Rubik', family = 'Rubik')
showtext_auto()

# data from: https://data.humdata.org/dataset/cod-ab-arg
tdf_data <- st_read('arg_adm_unhcr2017_shp/arg_admbnda_adm1_unhcr2017.shp') |>
  filter(ADM1_ES == 'Tierra del Fuego')

# remove z dimension 
tdf_data <- st_zm(tdf_data)

# elevation raster
tdf_elev <- get_elev_raster(tdf_data, z = 5, clip = "location")

tdf_df <- as.data.frame(tdf_elev, xy = TRUE)
colnames(tdf_df) <- c('x','y','elev')

tdf_above0 <- filter(tdf_df, elev > 0)

colors <- c(
  "#2f2a27", 
  "#9d5a58", 
  "#e18575", 
  "#c6a065", 
  "#c0b17f", 
  "#c5c0b1"
)

x_coord <- c(-7640000, -7638000, -7636000)
y_coord <- c(-6920000, -6922000, -6924000)
label_text <- c('Tierra del Fuego', 'Tierra del Fuego', 'Tierra del Fuego')
label_df <- data.frame(x_coord, y_coord, label_text)

col_pal <- c('orange','#d9b6ad','#95994b')

caption_x_coord <- c(-7370500)
caption_y_coord <- c(-7100000)
caption_text <- c(" ''The End of the World'' ")
caption_df <- data.frame(caption_x_coord, caption_y_coord, caption_text)

# see below for the ggchicklet source/logic
ggplot() +
  ggchicklet:::geom_rrect(
    aes(xmin = -7641000, xmax = -7100000, 
      ymin = -7400000, ymax = -6880000),
    r = unit(0.1, 'npc'), alpha = 1, fill = '#ecdce4') +
  geom_contour_filled(data = tdf_above0, aes(x = x, y = y, z = elev),
                      bins = 6) +
  geom_sf(data = tdf_data, fill = NA, linewidth=2, color = '#ecdce4') +
  geom_text(data = label_df, aes(x = x_coord, y = y_coord, label = label_text),
            size = 93, hjust=0, family = 'Shrikhand', color=col_pal) +
  geom_text(data = caption_df, aes(x = caption_x_coord, y = caption_y_coord,
                                   label = caption_text),
            family = 'Shrikhand', color = 'orange', size = 40, hjust=.5) +
  scale_fill_manual(values=colors) + 
  theme_minimal() +
  theme(legend.position = 'none',
    plot.background = element_rect(fill = '#c5cef9', color=NA),
    axis.title = element_blank(),
    axis.text = element_text(size = 46, color = '#9d5a58', family = 'Rubik'))

ggsave('day_12_tierra_del_fuego.png', height = 12, width = 12, dpi = 300)

### ggchicklet ####
# super useful implementation of an internal function from: 
# https://albert-rapp.de/posts/ggplot2-tips/11_rounded_rectangles/11_rounded_rectangles.html
ggplot() +
  ggchicklet:::geom_rrect(
    aes(
      xmin = 1, 
      xmax = 2, 
      ymin = 1, 
      ymax = 2, 
      fill = 'red',
      alpha = .6
    ),
    # Use relative npc unit (values between 0 and 1)
    # This ensures that radius is not too large for your canvas
    r = unit(0.1, 'npc')
  )
