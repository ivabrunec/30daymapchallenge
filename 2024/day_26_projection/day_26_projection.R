## Day 26: Projection.
# new zealand in different projections
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(sf)
library(ggplot2)
library(patchwork)
library(showtext)

font_add_google(name = 'Nixie One', family = 'nixie')
showtext_auto()

nz <- giscoR::gisco_get_countries(country = 'New Zealand', resolution = "03")

bbox <- st_bbox(c(
  xmin = 165, xmax = 180, 
  ymin = -48, ymax = -34
), crs = st_crs(4326))

# avoid 'wraparound' map
nz_cropped <- st_crop(nz, bbox)

projections <- list(
  "WGS84" = "+proj=longlat +datum=WGS84",
  "Mercator" = "+proj=merc",
  "Albers" = "+proj=aea +lat_1=-45 +lat_2=-40 +lon_0=173",
  "Lambert" = "+proj=laea +lat_0=-41 +lon_0=173",
  "Robinson" = "+proj=robin",
  "Mollweide" = "+proj=moll",
  "Transverse" = "+proj=tmerc +lat_0=0 +lon_0=173 +k=1 +x_0=500000 +y_0=0 +datum=WGS84",
  "Sinusoidal" = "+proj=sinu +lon_0=173",
  "Oblique" = "+proj=sterea +lat_0=-41 +lon_0=173 +k=0.9999 +x_0=0 +y_0=0 +datum=WGS84"
)

# transform and plot each projection
plots <- lapply(names(projections), function(name) {
  nz_proj <- st_transform(nz_cropped, crs = st_crs(projections[[name]]))
  ggplot() +
    geom_sf(data = nz_proj, fill = "#27132c", color = "#c5be2f") +
    coord_sf() +
    labs(title = name) +
    theme_void() +
    theme(plot.background = element_rect(fill = '#27132c', color = NA),
          plot.title = element_text(color = '#c5be2f', size = 50, family = 'nixie'))
})

combined_plot <- wrap_plots(plots, ncol = 3) &
  theme(
    plot.background = element_rect(fill = '#27132c', color = NA),
    plot.margin = margin(1, 1, 1, 1)
  )

ggsave(plot = combined_plot, 'day_26_projection_nz.png', height = 9, width = 5.5, dpi = 300)
