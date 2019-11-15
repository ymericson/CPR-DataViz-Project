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
businesses <- businesses %>% 
  rename(long = longitude, lat = latitude, 
         name = doing.business.as.name,
         facility.type = business.activity)
na.omit(businesses, cols=c("ward", "long", "lat"))
priv_fit <- businesses %>% 
  filter(str_detect(facility.type, "Fitness")) %>%
  filter(!str_detect(facility.type, 'Wrecking')) %>%
  select(name, ward, facility.type, long, lat)
priv_fit$type <- 'Private'

# Chicago Park District Parks
# https://bit.ly/2OdU3P9
pub_parks <- read.csv('CPD_Parks.csv')
colnames(pub_parks) <- tolower(colnames(pub_parks))
pub_parks <- pub_parks %>%
  select(park_no, ward, park_class) %>%
  filter()
colnames(pub_parks)[1] <- "park.number"
# Chicago Park Fitness Centers
# https://bit.ly/2KpfRWP
pub_fit <- read.csv('Chicago_Park_Fitness_Centers.csv')
colnames(pub_fit) <- tolower(colnames(pub_fit))
colnames(pub_fit)[1] <- "park"
pub_fit$type <- 'Public'
pub_fit <- pub_fit %>% 
  extract(location, c("lat", "long"), "\\(([^,]+), ([^)]+)\\)")
# merge public parks with public fitness centers
pub_fit <- merge(pub_fit, pub_parks, by='park.number') %>%
  select(ward, park, facility.name, long, lat, type) %>%
  rename(name = park, facility.type = facility.name)
pub_fit$long <- as.numeric(as.character(pub_fit$long))
pub_fit$lat <- as.numeric(as.character(pub_fit$lat))

all_fit <- rbind(pub_fit, priv_fit)
gyms_by_ward <- 
  all_fit %>% group_by(ward) %>% count(ward)

# Boundaries - Wards (2015-)
# https://bit.ly/32Qj2gV
il_spdf <- readOGR( 
  dsn= getwd() , 
  layer="geo_export_754fd6d2-6eab-484b-b590-75f1a6d0e041",
  verbose=FALSE)
il_spdf <- merge(il_spdf, gyms_by_ward, by='ward')
il_spdf@data[["n"]][is.na(il_spdf@data[["n"]])] <- 0

palette(colorRampPalette(c("white", "red"))(128))
n_gym <- il_spdf@data[["n"]]
cols <- (n_gym - min(n_gym))/diff(range(n_gym))*127+1
plot(il_spdf, col=cols)


ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill="dark grey") +
  theme_void() +
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90") +
  coord_fixed(1.3)


ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill=NA, color="dark grey") +
  geom_point(data=all_fit, aes(x=long, y=lat, colour=type)) +
  theme_void() +
  labs(colour = "Fitness Center Type") +
  coord_fixed(1.3)





ggplot(all_fit, aes(x=ward)) +
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

