## Day 6: Raster
# Mount Rainier

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(sf)
library(dplyr)
library(raster)
library(elevatr)
library(rayshader)

mt_rainier <- data.frame(x = c( -121.86, -121.65), 
                         y = c(46.80, 46.91))

elev <- get_elev_raster(mt_rainier, z = 13, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")
plot(elev)
col_pal <- scico::scico(30, palette = 'devon')
elmat <- raster_to_matrix(elev)
elmat |>
  height_shade(texture = col_pal) %>%
  plot_3d(elmat, zscale = 10,  baseshape = 'circle', theta = 90, phi = 30, zoom = .6, 
          windowsize = c(1200, 800), background = '#f3eaea', solid = T, shadow = T,
          solidcolor = '#2C194C', solidlinecolor = '#2C194C')

# animate
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = -90 + 45 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))
render_movie(filename = 'test', type = "custom",
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)

render_movie(filename = 'day_6_animated.mp4', type = "custom",
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)

# re-render with ray shading and high resolution
elmat |>
  sphere_shade(texture = 'imhof4') %>%
  plot_3d(elmat, zscale = 10,  baseshape = 'circle', theta = 90, phi = 30, zoom = .6, 
          windowsize = c(1200, 800), background = '#f3eaea', solid = T, shadow = T,
          solidcolor = 'grey50', solidlinecolor = 'grey50', shadowdepth = 0.6)

render_highquality(filename = 'day_6_hires.png')

