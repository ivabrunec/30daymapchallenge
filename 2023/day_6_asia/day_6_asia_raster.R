## Day 6: Asia
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(rayshader)
library(rayrender)

asia <- giscoR::gisco_get_countries(region = "Asia") |>
  elevatr::get_elev_raster(z = 6, clip = 'location')

asia1 <- aggregate(asia, 50)
plot(asia1)

dat_df <- as.data.frame(rasterToPoints(asia1))
colnames(dat_df) <- c('x','y','elev')

dat_df <- dat_df |>
  mutate(elev_level = ntile(elev, 20))

col_pal = colorRampPalette(colors = c(
    "#030503", 
    "#64532b", 
    "#cfa94d", 
    "#c6c57c", 
    "#b4e0aa"
))
cur_pal = col_pal(100)

bg_col = '#ccc1d9'

temp <- ggplot() +
  geom_tile(data = dat_df, aes(x = x, y = y, fill = elev_level)) +
  scale_fill_gradientn(colors=cur_pal)+
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = bg_col, color=NA),
        panel.background = element_rect(fill = bg_col, color=NA)) +
  coord_equal(ratio = 1.3)

plot_gg(temp,
        raytrace = T,
        scale=30,
        windowsize=c(1400,866),
        zoom = 0.55, 
        background = bg_col,
        shadow_intensity = .8,
        solid =F
        )

render_highquality('day_6_asia_2.png', 
                   lightaltitude = 10,
                   lightdirection = 40,
                   lightintensity = 800,
                   samples = 500,
                   # tried something, commenting out but not deleting for future use
                   scene_elements = rayrender:: add_object(cube(y=16,ywidth=0.1,xwidth=1000,zwidth=1000,
                                                                 material=dielectric(color = 'pink', attenuation=c(1,0.2,1)))) 
                   #scene_elements = rayrender::add_object(sphere(x = 650, y = -10, z = -50, radius = 100,
                   #                                              material = rayrender::light(color= '#e38424',
                   #                                                                          intensity = 7)))
)
