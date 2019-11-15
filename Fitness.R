library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)

# Chicago Business Licenses - Current Active
# https://bit.ly/2NP2U9K
setwd("C:/Users/ymeri/Documents/R/CPR-DataViz-Project")
businesses <- read.csv('Business_Licenses.csv')
colnames(businesses) <- tolower(colnames(businesses))
businesses <- businesses %>% rename(long = longitude, lat = latitude)
na.omit(businesses, cols=c("ward", "long", "lat"))
gyms <- businesses %>% 
  filter(str_detect(business.activity, "Fitness")) %>%
  filter(!str_detect(business.activity, 'Wrecking')) %>%
  select(legal.name, doing.business.as.name, address, city, state,
         zip.code, ward, precinct, license.description, 
         business.activity, long, lat)
gyms_by_ward <- 
  gyms %>% group_by(ward) %>% count(ward)

# Boundaries - Wards (2015-)
# https://bit.ly/32Qj2gV
il_spdf <- readOGR( 
  dsn= getwd() , 
  layer="geo_export_754fd6d2-6eab-484b-b590-75f1a6d0e041",
  verbose=FALSE)

il_spdf <- merge(il_spdf, gyms_by_ward, by='ward')
il_spdf@data[["n"]][is.na(il_spdf@data[["n"]])] <- 0

palette(colorRampPalette(c("white", "red"))(128))
pop <- il_spdf@data[["n"]]
cols <- (pop - min(pop))/diff(range(pop))*127+1
ggplot() +
  plot(il_spdf, col=cols)



ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group), fill=NA, color="light grey") +
  geom_point(data=gyms, aes(x=long, y=lat)) + 
  theme_void() +
  coord_fixed(1.3)
  



ggplot(gyms, aes(x=ward)) +
  geom_bar(stat="count")




# ggplot() +
#   theme_bw() +
#   geom_point(data=gyms, aes(x=long, y=lat)) +
#   coord_fixed(1.3) +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(),
#         axis.ticks.x=element_blank())


#Feedback
# shape files
# income, where people are working, living
# PUBLIC recreational centers
# American Comiunity Survey
# traffic cdensity
# cook county data - employment
# Grocery stores
# children
# obesity by zip code
# climbing gyms

