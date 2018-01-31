library(jsonlite)
library(ggmap)
library(tidyverse)
library(sp)
library(rgeos)
library(geosphere)
library(gmapsdistance)

d <- fromJSON("http://www.poznan.pl/mim/plan/map_service.html?mtype=pub_transport&co=parking_meters")

d_coords <- data.frame(matrix(unlist(d$features$geometry$coordinates), nrow = nrow(d$features), byrow = T))
names(d_coords) <- c(c("lon", "lat"))

d_prop <- d$features$properties

strefy <- cbind(d_prop, d_coords)

get_poznan <- get_map(c(16.917, 52.41), zoom = 14)
poznan <- ggmap(get_poznan)

poznan + geom_point(data=strefy, aes(x=lon, y=lat, colour=zone)) +
  scale_color_manual(values=c("#F80500", "#F4C604", "#07D500"), guide = guide_legend(title="Strefa")) +
  ylab("") + xlab("")

poznan + geom_point(data=strefy, aes(x=lon, y=lat, colour=zone)) +
  scale_color_manual(values=c("#F80500", "#F4C604", "#07D500"), guide = guide_legend(title="Strefa")) +
  ylab("") + xlab("") +
  geom_density2d(data=d_coords, aes(x=lon, y=lat), size = 0.3, bins = 8)

strefa_a <- strefy %>%
  filter(zone=="A")
  
strefa_a <- strefa_a %>%
  mutate(id=1:nrow(strefa_a))

strefa_a_sp <- strefa_a
coordinates(strefa_a_sp) <- ~lon+lat

dist <- distm(strefa_a_sp)

min_dist <- apply(dist, 1, function(x) order(x, decreasing=F)[2])

strefa_a_dist <- cbind(strefa_a, strefa_a[min_dist,], 
                       apply(dist, 1, function(x) sort(x, decreasing=F)[2]))

colnames(strefa_a_dist) <- c(colnames(strefa_a), paste0("n_", colnames(strefa_a)), "odl_prosta")

# google

g_dist <- gmapsdistance(origin = paste0(strefa_a_dist$lat,"+",strefa_a_dist$lon),
                        destination = paste0(strefa_a_dist$n_lat,"+",strefa_a_dist$n_lon), 
                        mode = "walking",
                        key = "AIzaSyAcL0L_vq3_rlbn7_oYRoKcw5HRmuB-YqU",
                        combinations = "pairwise")

g_dist$Status %>%
  group_by(status) %>%
  count()

strefa_a_dist <- strefa_a_dist %>%
  mutate(odl_google=g_dist$Distance$Distance,
         czas_google=g_dist$Time$Time)

ggplot(strefa_a_dist, aes(odl_prosta, odl_google)) + 
  geom_point() +
  xlab("Odległość w linii prostej") + 
  ylab("Odległość na podstawie Google Maps") +
  theme_light()

# który parkomat jest najbliższym sąsiadem dla największej liczby parkomatów

l_sasiad <- strefa_a_dist %>% 
  group_by(n_id) %>%
  count()

strefa_a_sasiad <- left_join(strefa_a_dist, l_sasiad, by=c("id"="n_id")) %>%
  mutate(n=ifelse(is.na(n),0,n))

poznan + geom_point(data=strefa_a_sasiad, aes(x=lon, y=lat, colour=as.character(n))) 
