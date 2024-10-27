## Day 11: Red.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(elevatr)
library(sf)
library(rayvista)

bryce_vista <- plot_3d_vista(37.588402313632265,-112.1690193359227,radius=5000,
                              overlay_detail = 13, elevation_detail=14,
                              show_vista = FALSE)


# edit the texture using the {magick} package
library(magick)
temp_img <- tempfile(fileext = '.png')
png::writePNG(bryce_vista$texture, temp_img)
mag_img <- magick::image_read(temp_img)

mag_img

edited_texture <- magick::image_modulate(mag_img, brightness = 110,
                                         saturation = 75, hue = 280) |>
  magick::image_contrast(sharpen = 1)
  magick::image_enhance()

edited_texture

magick::image_write(edited_texture, temp_img)
edit_tex <- png::readPNG(temp_img)

#plot directly with rayshader
library(rayshader)
plot_3d(edit_tex, bryce_vista$dem_matrix, zscale=1,
                   windowsize = 1200, zoom=0.75, phi=80, theta=0)
render_highquality(lightaltitude = 50, clear=TRUE, lightcolor = '#f2d4d4',
                   lightintensity = 600,
                   filename = 'day11_red.png')
 

# other attempts
library(tanaka)

# read area of interest
# coordinates
#bryce_slice <- data.frame(x = c(-112.182816, -112.138056), 
#                        y = c(37.590061, 37.628400))
bryce_slice <- data.frame(x = c(-112.209603, -112.111860), 
                        y = c(37.576828, 37.663840))

elev <- get_elev_raster(bryce_slice, z = 12, clip = 'bbox',
                        prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",
                        src = "aws")
ras <- terra::rast(elev)
col_pal = rev(colorspace::sequential_hcl(9, palette = "Reds"))

#colorspace::scale_fill_continuous_sequential(palette = rev('Batlow'), begin = 0, end = 1) +

#tiff("test.tiff", units="in", width=5, height=5, res=300)

tanaka(ras, 
       breaks=seq(1800,3000,75),
       col = col_pal,
       dark = 'darkred'
       )

# insert ggplot code
#dev.off()
