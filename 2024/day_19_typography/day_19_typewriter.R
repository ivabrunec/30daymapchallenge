## Day 19: Typography.
# typewriter elevation map: https://nrennie.rbind.io/blog/creating-typewriter-maps-r/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)
library(dplyr)
library(sf)
library(elevatr)

chile <- giscoR::gisco_get_countries(country = "Chile")

# exclude island
bbox <- st_bbox(chile)
bbox["xmin"] <- -75 
bbox["ymin"] <- -56
bbox["xmax"] <- -66
bbox["ymax"] <- -17

# Create a new spatial object using the adjusted bounding box
chile <- st_crop(chile, bbox)

elev_data <- get_elev_raster(chile, z = 4, clip = 'location')
  
# now following tutorial steps
elev_mat <- terra::as.matrix(elev_data, wide = TRUE)
colnames(elev_mat) <- 1:ncol(elev_mat)
elev_df <- elev_mat |> 
  as_tibble() |> 
  mutate(y = row_number()) |> 
  tidyr::pivot_longer(-y, names_to = "x") |> 
  mutate(x = as.numeric(x))
  
chars <- c("l", "H", "C")
chars_map <- data.frame(value = seq_len(length(chars)),
                          value_letter = chars)
  
elev_plot <- elev_df |> 
  mutate(value = ntile(value, n = length(chars))) |> 
  left_join(chars_map, by = "value")
  
ggplot() +
  geom_text(data = elev_plot, 
            mapping = aes(x = x, y = y, label = value_letter),
            size = 2) +
  scale_y_reverse() +
  coord_fixed() +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = NA))

ggsave('test.png', height = 20, width = 6)
