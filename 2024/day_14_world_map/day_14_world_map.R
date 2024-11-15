## Day 14: a world map
# natural earth raster: https://www.naturalearthdata.com/downloads/50m-raster-data/50m-natural-earth-1/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(rnaturalearth)
library(sf)
library(raster)
library(rayshader)

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

rivers <- rivers[!st_is_empty(rivers), ]


earth_dat <- raster('NE1_50M_SR_W.tif') |>
  aggregate(fact = 10)
plot(earth_dat)

elmat <- raster_to_matrix(earth_dat)

elmat |>
  sphere_shade(texture = create_texture("#ffffff", "#EDEDED", "#E0E0E0", "#D6D6D6", "#CCCCCC")) |>
  plot_3d(elmat, zscale = 20, windowsize = c(1000, 800))

#rivers2 <- head(rivers, 400)

render_path(
  extent = attr(earth_dat, "extent"),
  rivers,
  heightmap = elmat,
  zscale = 20,
  color = "orange", 
  clear_previous = T
)

render_highquality(filename = 'day_14.png',
                   path_material = rayrender::glossy)


## new: globe
# rivers of the world on globe
# honestly this is heavily lifted from last year's oceania submission but I'm rolling with it

rivers <- ne_download(scale = 50, type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")

rivers <- rivers[!st_is_empty(rivers), ]
rivers_lines <- st_cast(rivers, "LINESTRING")
ggplot() +
  geom_sf(data = rivers_lines)



outline_scene = list()
counter = 1

for (i in 1:length(rivers_lines$geometry)) {
  temp <- rivers_lines$geometry[[i]]
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

group_objects(full_outline_scene,scale=c(1,1,1)*1.02) %>% 
  add_object(sphere(radius=0.99,material=diffuse(),angle=c(0,-90,0))) %>% 
  group_objects(angle = c(0, 120, 0)) %>%
  add_object(sphere(y=2,z=1,x=5,material=light(intensity = 120,color="lightblue"))) %>% 
  #add_object(sphere(y=6,z=-5,x=-5,material=light(intensity = 80,color="orange"))) %>% 
  #add_object(sphere(y=-10,material=light(intensity = 80,color="white"))) %>%
  render_scene(samples=900,width=1200,height=1200,fov=17,aperture=0, ortho_dimensions = c(2.3,2.3),
               sample_method = "sobol",filename=sprintf('day_16_oceania.png'),
               lookfrom = c(3, -1.5, -9),
               lookat = c(1, -.5, -.7),
               clamp_value = 5)




## new: world in blocky raster
world <- giscoR::gisco_get_countries() |>
  elevatr::get_elev_raster(z = 2, clip = 'location')

world1 <- aggregate(world, 10)
plot(world1)

elmat <- raster_to_matrix(world)

elmat |>
  sphere_shade(texture = create_texture("#ffffff", "#EDEDED", "#E0E0E0", "#D6D6D6", "#CCCCCC")) |>
  plot_3d(elmat, solid = T, zscale = 300, windowsize = c(1000, 800))

volcanoes <- read_sf('data/volcano.json') 
volcanoes <- volcanoes |>
  dplyr::filter(H_active == 1)
coordinates <- st_coordinates(volcanoes)

# create vectors
longitude <- coordinates[, 1]
latitude <- coordinates[, 2]

render_points(extent = attr(world,"extent"), 
              lat = latitude, long = longitude,
              color = 'red',
              heightmap = elmat, zscale=300)

render_highquality(filename = 'test.png',
                   point_material = rayrender::light,
                  point_material_args = list(intensity = 200, color = 'red'))


dat_df <- as.data.frame(rasterToPoints(world1))
colnames(dat_df) <- c('x','y','elev')

dat_df <- dat_df |>
  mutate(elev_level = ntile(elev, 20))

col_pal = colorRampPalette(colors = c(
  "#030503", 
  "#64532b", 
  "#cfa94d", 
  "#c6c57c", 
  "#b4e0aa"
))
cur_pal = col_pal(100)

bg_col = '#ccc1d9'

temp <- ggplot() +
  geom_tile(data = dat_df, aes(x = x, y = y, fill = elev_level)) +
  scale_fill_gradientn(colors=cur_pal)+
  theme(legend.position = '',
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = bg_col, color=NA),
        panel.background = element_rect(fill = bg_col, color=NA)) +
  coord_equal(ratio = 1.3)

plot_gg(temp,
        raytrace = T,
        scale=30,
        windowsize=c(1400,866),
        zoom = 0.55, 
        background = bg_col,
        shadow_intensity = .8,
        solid =F
)
render_highquality('temp.png', 
                   lightaltitude = 10,
                   lightdirection = 40,
                   lightintensity = 800,
                   samples = 500,
                   # tried something, commenting out but not deleting for future use
                   scene_elements = rayrender:: add_object(cube(y=16,ywidth=0.1,xwidth=1000,zwidth=1000,
                                                                material=dielectric(color = 'pink', attenuation=c(1,0.2,1)))) 
                   #scene_elements = rayrender::add_object(sphere(x = 650, y = -10, z = -50, radius = 100,
                   #                                              material = rayrender::light(color= '#e38424',
                   #                                                                          intensity = 7)))
)
