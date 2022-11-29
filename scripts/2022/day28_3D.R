## Day 28: 3D.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(raster)
library(ggplot2)
library(elevatr)
library(purrr)
library(tidyr)
library(dplyr)

tdp_slice <- data.frame(x = c(-73.15019769318883, -73.0528926591736), 
                        y = c(-51.04076380296734, -50.957990256593526))

elev <- get_elev_raster(tdp_slice, z = 8, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")

elev_ra <- as.data.frame(rasterToPoints(elev))
colnames(elev_ra) <- c('x','y','elev')

library(isoband)
library(rayshader)
library(rayrender)
elmat <- raster_to_matrix(elev)

mountain_contours = isoband::isolines(x = 1:ncol(elmat), 
                                     y = 1:nrow(elmat), 
                                     z = elmat, 
                                     levels=seq(100,2200,by=200))

contours = isoband::iso_to_sfg(mountain_contours)
sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))
sf_contours$level <- as.numeric(sf_contours$level)
ggplot(sf_contours) + geom_sf(aes(color = level))

# code from here on is from David Friggens: @dakvid
# incredibly helpful: https://gist.github.com/dakvid/414006d86417880c2dfc2aa236d9ae6f
NUM_CONTOURS <- length(contours)

# increase HEIGHT_REDUCE to shrink the gaps between
# HEIGHT_LOWER (I think) determines where the model starts
# HEIGHT_OFFSET (I think) determines how centered the isobands are 
HEIGHT_LOWER <- 0
HEIGHT_REDUCE <- 150
HEIGHT_OFFSET <- 0

# I centered mine
X_OFFSET <- 0
Y_OFFSET <- 0

# radius of isobands: higher numbers mean thicker lines
RADIUS <- 0.15
INTENSITY = .8

create_segment <- 
  function(start_x, start_y, start_z,
           end_x, end_y, end_z,
           heat_color) {
    segment(start = c(start_x, start_y, start_z),
            end = c(end_x, end_y, end_z),
            radius = RADIUS,
            material = light(intensity = INTENSITY,
                             color = heat_color))
            #material = diffuse(color = heat_color))
  }

create_sphere <- 
  function(start_x, start_y, start_z,
           heat_color,
           ...) {
    sphere(x = start_x,
           y = start_y,
           z = start_z,
           radius = RADIUS,
           material = light(intensity = INTENSITY,
                            color = heat_color))
           
           #material = diffuse(color = heat_color))
  
           }

process_each_contour <- 
  function(the_contour, the_height, the_color) {
    
    # (h - L) / R - O
    transformed_height <- 
      the_height %>% 
      as.numeric() %>% 
      magrittr::subtract(HEIGHT_LOWER) %>% 
      magrittr::divide_by(HEIGHT_REDUCE) %>% 
      magrittr::subtract(HEIGHT_OFFSET)
    # Don't know about you, but I find x,y + h => x,y,z confusing...
    contour_coordinates <- 
      the_contour %>% 
      as_tibble() %>% 
      group_by(id) %>% 
      mutate(start_x = x - X_OFFSET,
             start_y = transformed_height,
             start_z = y - Y_OFFSET,
             end_x = lead(x) - X_OFFSET,
             end_y = transformed_height,
             end_z = lead(y) - Y_OFFSET,
             heat_color = the_color) %>% 
      ungroup() %>% 
      select(start_x, start_y, start_z,
             end_x, end_y, end_z,
             heat_color)
    
    # Same segments as before
    contour_segments <- 
      contour_coordinates %>% 
      drop_na() %>% 
      pmap_df(create_segment)
    
    # Add spheres at corners
    contour_spheres <- 
      contour_coordinates %>% 
      pmap_df(create_sphere)
    
    return(bind_rows(contour_segments,
                     contour_spheres))
  }

ki_scene_test <- 
  pmap_df(list(the_contour = mountain_contours,
               the_height = names(mountain_contours),
               the_color = terrain.colors(NUM_CONTOURS)),
          process_each_contour)

#FROM_TEST <- c(0, 45, 100)
FROM_TEST <- c(82, 24, 63)
#AT_TEST <- c(20, -1, 10)
AT_TEST <- c(13, -1.2, 18)
FOV_TEST <- 30

APERTURE_TEST <- 0
WIDTH_TEST <- 1200
HEIGHT_TEST <- 800

generate_ground(material = diffuse(color="grey20")) %>% 
  add_object(ki_scene_test) %>% 
  render_scene(fov = FOV_TEST,
               lookfrom = FROM_TEST, lookat = AT_TEST,
               samples = 200, aperture = APERTURE_TEST,
               width = WIDTH_TEST, height = HEIGHT_TEST,
               filename = 'day28_3d_6.png')


