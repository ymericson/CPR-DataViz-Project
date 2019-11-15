library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)

il_df <- subset(map_data("state"), region == "illinois")
counties <- map_data("county")
il_county <- subset(counties, region == "illinois")
il_base <- ggplot(data = il_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
il_base + theme_nothing() + 
  geom_polygon(data = il_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)