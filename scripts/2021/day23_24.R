## Days 23-24: GHSL data + historical map
# https://ghsl.jrc.ec.europa.eu/download.php?ds=bu
# plotting built environment over time in tile 19_3: Slovenia

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(dplyr)
library(sf)
library(sfheaders)
library(raster)
library(mapdata)

# data from this tile by year
data_1975 <- raster("data/GHS_BUILT_LDS1975_GLOBE_R2018A_54009_250_V2_0_19_3.tif")
# aggregate output
agg_1975 <- aggregate(data_1975, fact = 20) 
# reproject to match country
agg_1975 <- projectRaster(agg_1975, crs = 4326)
# as data frame
matrix_1975 <- as.data.frame(rasterToPoints(agg_1975)) 
colnames(matrix_1975) <- c('x', 'y', 'built')
sf_1975 = st_as_sf(matrix_1975, coords = c("x", "y"), crs = 4326, agr = "constant")
sf_1975 <- filter(sf_1975, built > 0)

data_1990 <- raster("data/GHS_BUILT_LDS1990_GLOBE_R2018A_54009_250_V2_0_19_3.tif")
agg_1990 <- aggregate(data_1990, fact = 20)
agg_1990 <- projectRaster(agg_1990, crs = 4326)
matrix_1990 <- as.data.frame(rasterToPoints(agg_1990)) 
colnames(matrix_1990) <- c('x', 'y', 'built')
sf_1990 = st_as_sf(matrix_1990, coords = c("x", "y"), crs = 4326, agr = "constant")
sf_1990 <- filter(sf_1990, built > 0)

data_2000 <- raster("data/GHS_BUILT_LDS2000_GLOBE_R2018A_54009_250_V2_0_19_3.tif")
agg_2000 <- aggregate(data_2000, fact = 20)
agg_2000 <- projectRaster(agg_2000, crs = 4326)
matrix_2000 <- as.data.frame(rasterToPoints(agg_2000)) 
colnames(matrix_2000) <- c('x', 'y', 'built')
sf_2000 = st_as_sf(matrix_2000, coords = c("x", "y"), crs = 4326, agr = "constant")
sf_2000 <- filter(sf_2000, built > 0)

data_2014 <- raster("data/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_250_V2_0_19_3.tif")
agg_2014 <- aggregate(data_2014, fact = 20)
agg_2014 <- projectRaster(agg_2014, crs = 4326)
matrix_2014 <- as.data.frame(rasterToPoints(agg_2014)) 
colnames(matrix_2014) <- c('x', 'y', 'built')
sf_2014 = st_as_sf(matrix_2014, coords = c("x", "y"), crs = 4326, agr = "constant")
sf_2014 <- filter(sf_2014, built > 0)

# get outline of slovenia
slovenia <- map_data('world') %>%
  filter(., region == "Slovenia")

x = 125
color = 'gray40'

ggplot() + 
  geom_sf(data=sf_1975 %>% rotate_data(), aes(color=built))+
  annotate("text", label='Terrain', x=x, y= -8.0, hjust = 0, color=color) +
  geom_sf(data = sf_1990 %>% rotate_data(), aes(color=built)) +
  annotate("text", label='Population', x=x, y= -7.9, hjust = 0, color=color) +
  #geom_sf(data = sf_2000 %>% rotate_data(), aes(color=built)) +
  annotate("text", label='Schools', x=x, y= -7.8, hjust = 0, color=color) +
  #geom_sf(data = sf_2014 %>% rotate_data(), aes(color=built)) +
  annotate("text", label='Hospitals', x=x, y= -7.7, hjust = 0, color=color) +
  scale_color_gradientn(colors = c('grey80','firebrick3'), limits = c(0, 100)) 
  #geom_polygon(data = slovenia, aes(x = long, y = lat, group = group), color = 'black', fill = NA) 
  #coord_fixed(1) +
  #coord_sf(xlim = c(13, 17), ylim = c(44, 48))

### let's try something
# from https://www.urbandemographics.org/post/figures-map-layers-r/ 

rotate_data <- function(data, x_add = 0, y_add = -5) {
  
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x){ 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  data %>% 
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

