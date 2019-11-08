library(tidyverse)
library(ggplot2)
library(dplyr)
#library(httr)
library(ggmap)
#library(tmap)

# r <- GET("https://data.cityofchicago.org/api/odata/v4/uupf-x98q")
# Chicago Business Licenses - Current Active
# https://bit.ly/2NP2U9K
setwd("C:/Users/ymeri/Documents/R/CPR-DataViz-Project")
businesses <- read.csv('Business_Licenses.csv')
colnames(businesses) <- tolower(colnames(businesses))
businesses <- businesses %>% rename(long = longitude, 
                                    lat = latitude)
na.omit(businesses, cols=c("ward", "long", "lat"))

gyms <- businesses %>% 
  filter(str_detect(business.activity, "Fitness")) %>%
  filter(!str_detect(business.activity, 'Wrecking'))
  select(legal.name, doing.business.as.name, address, city, state,
         zip.code, ward, precinct, license.description, 
         business.activity, long, lat)
  
ggplot(gyms, aes(x=ward)) +
  geom_bar(stat="count")


ggplot() +  
  theme_bw() + 
  geom_point(data=gyms, aes(x=long, y=lat)) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.x=element_blank())

usa <- map_data("usa")
ggplot(usa, aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  coord_map() +
  theme_nothing()

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

