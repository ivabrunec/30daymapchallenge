## Day 26: Minimal.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(sf)
library(raster)
library(rayshader)
library(rayrender)
library(osmdata)

x_min = -79.46902701368063
x_max = -79.34592544938062
y_min = 43.63642272932266
y_max = 43.698116062611376

bb = c(x_min, y_min, 
       x_max, y_max)


# get all cycleway designated roads
cycle_1 <- opq(bbox = bb) |>
  add_osm_feature(key = 'cycleway') |>
  osmdata_sf()

# extract lines
cycle_lines <- cycle_1$osm_lines

# get all the main roads
roads_main <- opq(bbox = bb) |>
  add_osm_feature(key = 'highway',
                  value = c('primary',
                            'secondary',
                            'motorway',
                            'trunk',
                            'primary_link',
                            'secondary_link',
                            'trunk_link',
                            'motorway_link'
                  )) |>
  osmdata_sf()

road_lines <- roads_main$osm_lines
  

# edit the bounding box to match elevatr requirements
bb_elev <- data.frame(x = c(x_min, x_max),
                      y = c(y_min, y_max))

elev <- elevatr::get_elev_raster(bb_elev, z = 10, clip = 'bbox',
                                 prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                                 src = "aws")

elev_df <- as.data.frame(rasterToPoints(elev))
colnames(elev_df) <- c('x','y','z')

# this is a super hacky solution, I assume there's a more elegant way
# but basically this takes the elevation matrix & places the linestrings on it
# you can't skip the heightmap argument, but presumably you can generate a 'dummy' one?
# anyway for now i'm sticking with this approach
elmat <- raster_to_matrix(elev) 

temp <- ggplot() +
  geom_rect(aes(xmin = x_min, ymin= y_min, xmax = x_max, ymax= y_max),
            fill = 'white', color = NA) +
  theme(plot.background = element_rect(fill = 'white', color = 'white'),
        panel.background = element_rect(fill = 'white', color = 'white'),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank()) 

plot_gg(temp,
        raytrace = T, 
        height = 4,
        width = 6,
        windowsize=c(1000,800))

for (i in 1:nrow(road_lines)){
  render_path(road_lines$geometry[i], extent = extent(road_lines), heightmap = elmat, color='black',
              linewidth = .5, zscale=1000, offset = 4500, clear_previous = F)
}


for (i in 1:nrow(cycle_lines)){
  render_path(cycle_lines$geometry[i], extent = extent(cycle_lines), heightmap = elmat, color='orange',
              linewidth = .5, zscale=1000, offset = 4500, clear_previous = F)
}



render_highquality(
  filename = 'day_26_minimal.png',
  clamp_value=10,
  samples = 1200,
  sample_method = 'sobol_blue',
  ambient_light = T,
  #backgroundhigh = "#231f66", backgroundlow = "#4b45a1",
  path_material = rayrender::metal,
  scene_elements = rayrender:: add_object(cube(y=1,ywidth=0.1,xwidth=1000,zwidth=1000,
                                               material=metal(color = 'grey'))) 
  #scene_elements = rayrender:: add_object(cube(y=1,ywidth=0.1,xwidth=1000,zwidth=1000,
  #                                             material=dielectric(color = 'black', attenuation=c(0,0.2,1)))) 
)



#### older: code for reference ####
# i really really wanted to plot the bike lanes as paths hovering above a map
# however sadly i couldn't figure it out this time - the extents just were not matching up
# i'm leaving this code here on purpose for posterity, if i ever come back to solve this
# but be warned this does not work: the bike lanes are tiny tiny 
# and no amount of adjusting the extent fixed it


temp <- ggplot() +
  geom_rect(aes(xmin = x_min, ymin= y_min, xmax = x_max, ymax= y_max),
            fill = 'white', color = NA) +
  #geom_tile(data = elev_df, aes(x = x, y = y, fill = z))
  geom_sf(data = roads_main$osm_lines, color = 'grey') +
  geom_sf(data = cycle_lines$geometry, color = 'black') +
  theme(plot.background = element_rect(fill = 'white', color = 'white'),
        panel.background = element_rect(fill = 'white', color = 'white'),
        axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.ticks = element_blank()) 

plot_gg(temp,
        raytrace = T, 
        height = 4,
        width = 6,
        windowsize=c(1000,800))

for (i in 1:nrow(cycle_lines)){
  render_path(cycle_lines$geometry[i], extent = extent(cycle_lines), heightmap = elmat, color='red',
              linewidth = 2, zscale=1000, offset = 4500, clear_previous = F)
}
