## Day 19: Typography
# subway/metro stations w/ typography

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(sf)
library(osmdata)
library(cowplot)
library(showtext)
library(magick)


# honestly this is a smorgasbord of me using system fonts and importing fonts
font_add(family = "Parisine", regular = "C:/Users/ivakr/AppData/Local/Microsoft/Windows/Fonts/Parisine Regular.otf")
font_add(family = "SeoulNamsan", regular = "C:/Users/ivakr/AppData/Local/Microsoft/Windows/Fonts/SeoulNamsanEB.ttf")
font_add(family = "Toronto Subway", regular = "C:/Users/ivakr/AppData/Local/Microsoft/Windows/Fonts/Toronto-Subway-W01-Regular.ttf")


# specify list of city names + fonts
cities_df <- data.frame(
  city = c("London",
           "New York City",
           "MONTREAL",
           "Paris",
           "서울",
           "TORONTO"),
  font = c("London Tube",
           "Helvetica Neue",
           "Univers",
           "Parisine",
           "SeoulNamsan",
           "Toronto Subway"))
