## Day 16: Oceania
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(raster)
library(sf)
library(ggplot2)
library(dplyr)
library(rayshader)
library(rayrender)

# let's try something weird
# map of outlines of oceania, based on the cable map code
oceania <- giscoR::gisco_get_countries(region = "Oceania")

# extract just line strings
sf_data <- st_as_sf(oceania, wkt = "geometry")
sf_data <- st_cast(sf_data, "POLYGON")
# this df contains just linestrings, same as the cable map geojson
boundary_lines <- st_boundary(sf_data)

# plot to check
ggplot() +
  geom_sf(data = boundary_lines)

# taken directly from: https://gist.github.com/tylermorganwall/b222fcebcac3de56a6e144d73d166322
# now my data is linestrings (not multilinestrings) so the loop below is a little simplified
outline_scene = list()
counter = 1

for (i in 1:length(boundary_lines$geometry)) {
  temp <- boundary_lines$geometry[[i]]
  outline_val <- data.frame(
    x = sinpi(temp[, 1]/180) * cospi(temp[, 2]/180),
    y = sinpi(temp[, 2]/180),
    z = cospi(temp[, 1]/180) * cospi(temp[, 2]/180)
  )
  
  # Don't lower start of line at the 180/0 longitude border
  if (abs(temp[1, 1] - 180) > 0.001 && abs(temp[1, 1] + 180) > 0.001) {
    outline_val[1, ] <- outline_val[1, ] * 1/1.02
  }
  
  nr <- nrow(temp)
  
  # Don't lower end of line at the 180/0 longitude border
  if (abs(temp[nr, 1] - 180) > 0.001 && abs(temp[nr, 1] + 180) > 0.001) {
    nr <- nrow(outline_val)
    outline_val[nr, ] <- outline_val[nr, ] * 1/1.02
  } 
  
  outline_scene[[counter]] <- path(outline_val, width = 0.005, material = light(color = '#47ECD1', intensity = 6))
  counter <- counter + 1
}

full_outline_scene = do.call(rbind,outline_scene)

# could do more adjustments but this will have to do
group_objects(full_outline_scene,scale=c(1,1,1)*1.02) %>% 
  add_object(sphere(radius=0.99,material=diffuse(image_texture = "2k_earth_daymap.jpg"),angle=c(0,-90,0))) %>% 
  group_objects(angle=c(0,-1/2,0)) %>% 
  add_object(sphere(y=2,z=1,x=5,material=light(intensity = 120,color="lightblue"))) %>% 
  #add_object(sphere(y=6,z=-5,x=-5,material=light(intensity = 80,color="orange"))) %>% 
  #add_object(sphere(y=-10,material=light(intensity = 80,color="white"))) %>%
  render_scene(samples=900,width=1200,height=1200,fov=17,aperture=0, ortho_dimensions = c(2.3,2.3),
               sample_method = "sobol",filename=sprintf('day_16_oceania.png'),
               lookfrom = c(4, -2, -10), 
               lookat = c(1, -.5, -.7),
               clamp_value = 5)

