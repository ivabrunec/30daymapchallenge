## Day 14: a world map
# natural earth raster: https://www.naturalearthdata.com/downloads/50m-raster-data/50m-natural-earth-1/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rnaturalearth)
library(sf)
library(raster)
library(rayshader)

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

rivers <- rivers[!st_is_empty(rivers), ]

earth_dat <- raster('data/NE1_50M_SR_W/NE1_50M_SR_W.tif') |>
  aggregate(fact = 20)
plot(earth_dat)

elmat <- raster_to_matrix(earth_dat)

elmat |>
  sphere_shade(texture = 'bw') |>
  plot_3d(elmat, zscale = 50, windowsize = c(1000, 800))

rivers2 <- head(rivers, 5)

render_path(
  extent = attr(earth_dat, "extent"),
  rivers2,
  heightmap = elmat,
  zscale = 50,
  color = "blue", antialias = TRUE
)


render_highquality(filename = 'day_14.png',
                   path_material = rayrender::diffuse)


