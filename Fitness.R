library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
library(gridExtra)
library(grid)
library(sf)
library(RColorBrewer)

# ----------Chicago Business Licenses https://bit.ly/2NP2U9K----------
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
  select(name, ward, zip.code, long, lat)
colnames(priv_fit)[3] <- "zip"
priv_fit$type <- 'Private'
write.csv(priv_fit, file='Private_fitness_centers.csv')
# ----------Chicago Park District Parks https://bit.ly/2OdU3P9----------
pub_parks <- read.csv('CPD_Parks.csv')
colnames(pub_parks) <- tolower(colnames(pub_parks))
pub_parks <- pub_parks %>%
  select(park_no, ward, park_class, zip) %>%
  filter()
colnames(pub_parks)[1] <- "park.number"
# ----------Chicago Park Fitness Centers https://bit.ly/2KpfRWP----------
pub_fit <- read.csv('Chicago_Park_Fitness_Centers.csv')
colnames(pub_fit) <- tolower(colnames(pub_fit))
colnames(pub_fit)[1] <- "park"
pub_fit$type <- 'Public'
pub_fit <- pub_fit %>% 
  extract(location, c("lat", "long"), "\\(([^,]+), ([^)]+)\\)")
# merge public parks with public fitness centers
pub_fit <- merge(pub_fit, pub_parks, by='park.number') %>%
  select(ward, park, zip, long, lat, type) %>%
  rename(name = park) #, facility.type = facility.name)
pub_fit$long <- as.numeric(as.character(pub_fit$long))
pub_fit$lat <- as.numeric(as.character(pub_fit$lat))
# ----------Chicago Community Areas ShapeFile https://bit.ly/37f1Btw----------
il_spdf <- readOGR( 
  dsn= getwd() , 
  layer="geo_export_17e99094-f66e-4bf7-89b3-fa329ded2341",
  verbose=FALSE)
il_spdf@data$community <- str_to_title(il_spdf@data$community)
# ---------- Public Health Statistics https://bit.ly/35pX99O ----------
health_stat <- read.csv('Public_Health_Statistics.csv')


all_fit <- rbind(pub_fit, priv_fit)
all_fit <- all_fit[!with(all_fit,is.na(long)& is.na(lat)),]

coordinates(all_fit) <- ~long+lat
proj4string(all_fit) <- proj4string(il_spdf)
# Convert to sf-objects
il_spdf.sf <- st_as_sf(il_spdf)
all_fit.sf <- st_as_sf(all_fit)
# Keep all "il_spdf.sf", sort by row.names(il_spdf.sf). Default overlay is "intersects".
il_spdf_all_fit <- st_join(il_spdf.sf, all_fit.sf) 
# Keeps all "all_fit.sf", sort by row.names(all_fit.sf)
all_fit_il_spdf <- st_join(all_fit.sf, il_spdf.sf)
all_fit_il_spdf$community <- str_to_title(all_fit_il_spdf$community)

community_stats <- 
  all_fit_il_spdf %>% group_by(community) %>% count(community) %>%
  as.data.frame(community_stats)
community_stats <- merge(community_stats, as.data.frame(il_spdf@data), by='community', all=TRUE) %>% 
  select(community, n)
community_stats$community[community_stats$community == "Ohare"] <- "O'Hare"
community_stats$community[community_stats$community == "Mckinley Park"] <- "McKinley Park"
community_stats$community[community_stats$community == "Montclare"] <- "Montclaire"
community_stats[is.na(community_stats)] <- 0
community_stats <- merge(community_stats, health_stat, by.x='community', by.y='Community.Area.Name', all=TRUE) %>% 
  select(community, n, Diabetes.related, Below.Poverty.Level, Per.Capita.Income, Unemployment)



ggplot(community_stats, aes(x=Per.Capita.Income, y=Diabetes.related)) +
  geom_point(aes(size=n)) + 
  geom_smooth(method=lm)



# bar graph sorted by # of gyms
community_stats %>% arrange(desc(n)) %>%
  slice(1:77) %>%
  ggplot(., aes(reorder(community, n), y=n)) +
  geom_col() + 
  labs(x = "Community Areas", y = "Number of Fitness Centers") +
  coord_flip() + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) 




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
all_fit <- as.data.frame(all_fit)
p3 <- ggplot() +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group),
               fill=NA, color="dark grey") +
  geom_point(data=all_fit, aes(x=long, y=lat, colour=type)) +
  theme_void() +
  labs(colour = "Fitness Center Type", size=100) +
  coord_fixed(1.3) +
  theme(legend.position='bottom')
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
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


# contour heat maps
ggplot() +
  stat_density2d(data=all_fit, aes(x=long, y=lat, fill=..level..,alpha=..level..), bins=20, geom = "polygon") +
  scale_fill_gradientn(colours=rev(brewer.pal(11, "Spectral"))) +
  geom_polygon(data=il_spdf, aes(x=long, y=lat, group=group), fill=NA, color="dark grey") +
  guides(alpha = FALSE) + 
  theme_void() +
  coord_fixed(1.3) + 
  ggtitle("Concentration of Private and Public Gyms in Chicago")

