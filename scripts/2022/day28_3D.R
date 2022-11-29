## Day 28: 3D.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(raster)
library(ggplot2)
library(elevatr)
library(purrr)
library(tidyr)


tdp_slice <- data.frame(x = c(-73.15019769318883, -73.0528926591736), 
                        y = c(-51.04076380296734, -50.957990256593526))

elev <- get_elev_raster(tdp_slice, z = 8, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")

elev_ra <- as.data.frame(rasterToPoints(elev))
colnames(elev_ra) <- c('x','y','elev')

library(isoband)
elmat <- raster_to_matrix(elev)

volcano_contours = isoband::isolines(x = 1:ncol(elmat), 
                                     y = 1:nrow(elmat), 
                                     z = elmat, 
                                     levels=seq(100,2000,by=200))

contours = isoband::iso_to_sfg(volcano_contours)
sf_contours = sf::st_sf(level = names(contours), geometry = sf::st_sfc(contours))
sf_contours$level <- as.numeric(sf_contours$level)
ggplot(sf_contours) + geom_sf(aes(color = level))

# code from here on is from @dakvid
# incredibly helpful: https://gist.github.com/dakvid/414006d86417880c2dfc2aa236d9ae6f
NUM_CONTOURS <- length(contours)

# increase HEIGHT_REDUCE to shrink the gaps between
# HEIGHT_LOWER (I think) determines where the model starts
# HEIGHT_OFFSET (I think) determines how centered the isobands are 
HEIGHT_LOWER <- 0
HEIGHT_REDUCE <- 200
HEIGHT_OFFSET <- 0

# I centered mine
X_OFFSET <- 0
Y_OFFSET <- 0

# radius of isobands: higher numbers mean thicker lines
RADIUS <- 0.3

create_segment_test <- 
  function(start_x, start_y, start_z,
           end_x, end_y, end_z,
           heat_color) {
    segment(start = c(start_x, start_y, start_z),
            end = c(end_x, end_y, end_z),
            radius = RADIUS,
            material = light(intensity = 2,
                             color = heat_color))
  }

process_each_contour_test <- 
  function(the_contour, the_height, the_color) {
    
    # (h - L) / R - O
    transformed_height <- 
      the_height %>% 
      as.numeric() %>% 
      magrittr::subtract(HEIGHT_LOWER) %>% 
      magrittr::divide_by(HEIGHT_REDUCE) %>% 
      magrittr::subtract(HEIGHT_OFFSET)
    
    # Don't know about you, but I find x,y + h => x,y,z confusing...
    contour_segments <- 
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
      drop_na() %>% 
      select(start_x, start_y, start_z,
             end_x, end_y, end_z,
             heat_color) %>% 
      pmap_df(create_segment_test)
    
    return(contour_segments)
  }

ki_scene_test <- 
  pmap_df(list(the_contour = volcano_contours,
               the_height = names(volcano_contours),
               the_color = 'khaki'),
          process_each_contour_test)

FROM_TEST <- c(0, 45, 100)
AT_TEST <- c(20, -1, 10)
FOV_TEST <- 30

APERTURE_TEST <- 0
WIDTH_TEST <- 1200
HEIGHT_TEST <- 800

generate_ground(material = diffuse(color="grey20")) %>% 
  add_object(ki_scene_test) %>% 
  render_scene(fov = FOV_TEST,
               lookfrom = FROM_TEST, lookat = AT_TEST,
               samples = 200, aperture = APERTURE_TEST,
               width = WIDTH_TEST, height = HEIGHT_TEST)




raster::plot(elev)
elmat <- raster_to_matrix(elev_cut)
elmat[elmat < -2000] = 0
elmat[elmat > 2000] = 0



elmat |>
  sphere_shade(texture = 'imhof1') |>
  ##sphere_shade(texture = create_texture('#723d46', '#472d30', '#e26d5c','#ffe1a8',  '#c9cba3'), sunangle = 90) |>
  #sphere_shade(texture = create_texture('#723d46', '#695958', '#6D7789','#cfffe5',  '#A2C6AD'), sunangle = 90) |>
  #add_shadow(ray_shade(ukr_mat,sunaltitude=30, sunangle = 0),max_darken=0.65) |>
  plot_3d(elmat, zscale = .1, windowsize = (c(1000,800)),
          theta = 90, zoom = .8, solid =F)
