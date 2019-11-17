library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(gridExtra)
library(grid)


# Chicago Business Licenses - Current Active
# https://bit.ly/2NP2U9K
setwd("C:/Users/ymeri/Documents/R/CPR-DataViz-Project/Data")
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
  select(name, ward, facility.type, zip.code, long, lat)
colnames(priv_fit)[4] <- "zip"
priv_fit$type <- 'Private'
write.csv(priv_fit, file='Private_fitness_centers.csv')

# Chicago Park District Parks
# https://bit.ly/2OdU3P9
pub_parks <- read.csv('CPD_Parks.csv')
colnames(pub_parks) <- tolower(colnames(pub_parks))
pub_parks <- pub_parks %>%
  select(park_no, ward, park_class, zip) %>%
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
  select(ward, park, facility.name, zip, long, lat, type) %>%
  rename(name = park, facility.type = facility.name)
pub_fit$long <- as.numeric(as.character(pub_fit$long))
pub_fit$lat <- as.numeric(as.character(pub_fit$lat))

all_fit <- rbind(pub_fit, priv_fit)
gyms_by_ward <- 
  all_fit %>% group_by(ward) %>% count(ward)

# Chicago Community Areas
il_spdf <- readOGR( 
  dsn= getwd() , 
  #layer="geo_export_754fd6d2-6eab-484b-b590-75f1a6d0e041",
  layer="geo_export_17e99094-f66e-4bf7-89b3-fa329ded2341",
  verbose=FALSE)
il_spdf <- merge(il_spdf, gyms_by_ward, by='ward')
il_spdf@data[["n"]][is.na(il_spdf@data[["n"]])] <- 0

palette(colorRampPalette(c("white", "red"))(128))
n_gym <- il_spdf@data[["n"]]
cols <- (n_gym - min(n_gym))/diff(range(n_gym))*127+1
#plot(il_spdf, col=cols)

# two ggplot maps side-by-side
p1 <- ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill=NA, color="dark grey") +
  geom_point(data=priv_fit, aes(x=long, y=lat), color='#F8766D') +
  theme_void() +
  coord_fixed(1.3)
p2 <- ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill=NA, color="dark grey") +
  geom_point(data=pub_fit, aes(x=long, y=lat), color='#00BFC4') +
  theme_void() +
  coord_fixed(1.3)
p3 <- ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill=NA, color="dark grey") +
  geom_point(data=all_fit, aes(x=long, y=lat, colour=type)) +
  theme_void() +
  labs(colour = "Fitness Center Type", size=100) +
  coord_fixed(1.3) +
  theme(legend.position='bottom')

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend <- get_legend(p3)

tg <- textGrob("Locations of Private and Public Fitness Centers in Chicago", gp=gpar(fontsize=15, fontface="bold"))
sg <- textGrob("Private centers are concentrated in central and north side, while public fitness centers are spread out more evenly across Chicago", gp=gpar(fontsize=11))
map_sub <- textGrob("Data Source: Chicago Open Data Portal", x = 0.9, y = 0.5, gp=gpar(fontsize=8))
p <- grid.arrange(p1, p2, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))
grid.arrange(tg, sg, p, nrow=3, 
             bottom = map_sub, 
             heights=c(0.05, 0.05, 0.9))


ggplot(gyms_by_ward, aes(reorder(ward, -n), n)) +
  geom_col()

ggplot(gyms_by_ward, aes(x=ward, y=n)) +
  geom_col()

library(grid); library(gridExtra)
tg <- textGrob("Title Goes Here", gp=gpar(fontsize=30))
sg <- textGrob("more subtle subtitle ", gp=gpar(fontsize=15, fontface=3L))
margin <- unit(0.5)
grid.newpage()
grid.arrange(tg, sg,
             heights = unit.c(grobHeight(tg) + 1.2*margin, 
                              grobHeight(sg) + margin, 
                              unit(1,"null")),
             p1, p2, legend)

# ggplot() +
#   theme_bw() +
#   geom_point(data=gyms, aes(x=long, y=lat)) +
#   coord_fixed(1.3) +
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
#         axis.text.x=element_blank(), axis.text.y=element_blank(),
#         axis.ticks.x=element_blank())


# income, where people are working, living
# PUBLIC recreational centers
# American Comiunity Survey
# cook county data - employment
# obesity by zip code

## gghiglight



