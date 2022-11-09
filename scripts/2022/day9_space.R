## Day 9: Space.

# raster of the moon
# moon data from https://svs.gsfc.nasa.gov/cgi-bin/details.cgi?aid=4720

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
library(rayrender)
library(dplyr)

# background
# from https://www.pexels.com/photo/milky-way-galaxy-during-nighttime-1252890/

# testing rayrender
scene = 
  add_object(sphere(y=0.2,material=glossy(image_texture = 'data/moon.png',reflectance=0.05),angle=c(0,-90,0))) |>
  add_object(sphere(y=1.5, x = 1.5, radius = .25, 
                    material=glossy(image_texture = 'data/2kearth.png',reflectance=0.05),angle=c(0,-90,0))) 

# static/interactive image
render_scene(scene, parallel = TRUE, width = 800, 
             ambient_light = TRUE,
             height = 800, samples = 800,
             environment_light = 'data/pexels-hristo-fidanov-1252890.jpg',
             intensity_env = 3,
             filename = 'moon_earth6.png')


# short video: render times are a bit silly so i abandoned this, but it would work
frames = 360

camerax=-25*cos(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)
cameraz=25*sin(seq(0,360,length.out = frames+1)[-frames-1]*pi/180)

for(i in 1:frames) {
  render_scene(scene, width=600, height=600, fov=35,
               lookfrom = c(camerax[i],25,cameraz[i]),
               lookat = c(0,9,0), samples = 300, parallel = TRUE,
               filename=glue::glue("earth_moon{i}"))
}

av::av_encode_video(glue::glue("earth_moon{1:(frames-1)}.png"), framerate=60, output = "earth_moon.mp4")
file.remove(glue::glue("earth_moon{1:(frames-1)}.png"))
 