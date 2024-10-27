## Old map overlaid on elevation raster

library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# map from: # map from: https://www.dlib.si/details/URN:NBN:SI:IMG-9VE34LKT/?query=%27source%3dzemljevidi%27&browse=zemljevidi&node=slike&pageSize=25&flocation=Narodna+in+univerzitetna+knji%c5%benica&sortDir=ASC&sort=date
map_pic <- png::readPNG("Illyricum_URN_NBN_SI_IMG-9VE34LKT_trimmed.png")

illyricum_ish <- data.frame(x = c(13.260000, 18.500000), 
                            y = c(44.300000, 46.836482))

elev <- get_elev_raster(illyricum_ish, z = 7, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")

elmat <- raster_to_matrix(elev) 


elmat %>%
  sphere_shade() %>%
  add_overlay(map_pic, alphalayer = 0.9) %>%
  plot_3d(elmat, zscale = 50, fov = 0, theta = 0, phi = 60,
          windowsize = c(1200, 800),
          zoom = 0.7,
          background = "#e8d5bf",
          shadow_darkness = .6)

render_highquality(filename = 'day_11_retro.png',
                   samples = 500,
                   lightintensity = 600)



