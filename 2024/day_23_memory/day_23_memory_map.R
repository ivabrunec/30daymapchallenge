## Day 23: Memory.
# not really a map

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(ggplot2)
library(ggforce)
library(dplyr)
library(showtext)
library(geomtextpath)

font_add_google(name = 'Work Sans', family = 'work',bold.wt = 800)
showtext_auto()

circle_dat <- data.frame(
  r = c(32, 30, 27, 22, 19),
  city = c('Toronto', 'Philadelphia', 'Toronto', 'London', 'Ljubljana')
)

circle_dat$x <- 0
circle_dat$y <- 0
circle_dat$circle_id <- factor(seq_len(nrow(circle_dat)), levels = order(-circle_dat$r))

generate_circle <- function(radius, n_points = 1000) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  data.frame(
    x = radius * cos(theta),
    y = radius * sin(theta)
  )
}

circle_paths <- do.call(rbind, lapply(seq_len(nrow(circle_dat)), function(i) {
  path <- generate_circle(circle_dat$r[i])
  path$city <- circle_dat$city[i]
  path$r <- circle_dat$r[i]
  path
}))


ggplot() +
  geom_textpath(data = circle_paths, aes(x, y, group = interaction(city, r),
                                         label = city, color = city),
                family = 'work',
                #fontface = 'bold',
                size = 16, 
                upright = TRUE,
                hjust = 0.1) +
  scale_color_manual(values = c('#F6AE2D', '#73A580', '#33658A', '#F26419')) +
  geom_point(aes(x = 0, y = 0), color = '#F6AE2D') + 
  geom_point(aes(x = 20, y = -25), color = '#F26419') +
  annotate('text', x = 22, y = -25, label = 'you are here', 
           hjust = 0, size = 10, family = 'work', color = '#F26419') +
  coord_fixed() +
  labs(title = 'time is a flat circle') +
  theme_void() +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 100, family = 'work'),
        plot.background = element_rect(fill = 'grey90', color = NA))

ggsave('day_23_memory.png', height = 6, width = 5, dpi = 300)  
