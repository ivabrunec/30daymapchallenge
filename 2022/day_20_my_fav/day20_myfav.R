## Day 20: My favourite - hike


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(elevatr)
library(ggplot2)
library(dplyr)

# pollett's cove, nova scotia
polletts_bb <- data.frame(x = c(-60.72877393549791, -60.61160141514791), 
                       y = c(46.89456087085914, 46.950102483075526))

 
polletts_elev <- get_elev_raster(polletts_bb, z = 11, clip = 'bbox', 
                              prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") 

polletts_df <- as.data.frame(polletts_elev, xy = TRUE)
colnames(polletts_df) <- c('x','y','elev')

polletts_above0 <- filter(polletts_df, elev > 0)

polletts_df <- polletts_above0 |>
  mutate(elev_level = ntile(elev, 12))

# experimenting with colours
col_pal = colorspace::sequential_hcl(12, palette = "Dark Mint")

col_pal = colorRampPalette(colors = c('#0d204d','#214f67','#7ca7ad','#bbe0d7','#bfb3c1','#eda48f','#c6584f'))
col_pal2 = colorRampPalette(colors = c('#6ba391','#6c8964','#ab8816','#fccc24','#f39d0f','#c6584f'))
cur_pal = col_pal2(12)

# tried plotting with geom_tile but kept getting a weird artefact after rayshading
# also tried plotting with theme_void but the base just looked like a black rectangle no matter what
pc <- ggplot() +
  geom_raster(data = polletts_df, aes(x = x, y = y, fill = elev_level)) +
  scale_fill_gradientn(colors=cur_pal)+
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = '#6ba391'),
        panel.background = element_rect(fill = '#6ba391'))

pc

library(rayshader)
plot_gg(pc,
        raytrace = T,
        width=5,height=5,
        scale=150,
        windowsize=c(1400,866),
        zoom = 0.55)


render_highquality('day20_myfav.png', 
                   lightaltitude = 30,
                   lightdirection = 0,
                   lightintensity = 500,
                   samples = 500
                   )
