library(tidyverse)
library(ggmap)
library(ggtext)
library(maps)
#set up google account with api
#enable: Static Maps, Map Embed, Geolocation, Geocoding.



#bike routes
bike_route <- trek("st.petersburg, florida", "portland, oregon", structure = "route", mode="bicycling")
biked_miles<- trek("st.petersburg, florida", "twin falls, idaho", structure = "route", mode="bicycling")

#point locations
points<-data.frame(
  city = c("st.pete","portland","current"),
  lon = c(bike_route$lon[1],bike_route$lon[nrow(bike_route)],biked_miles$lon[nrow(biked_miles)]),
  lat = c(bike_route$lat[1],bike_route$lat[nrow(bike_route)],biked_miles$lat[nrow(biked_miles)])
)


#get state polygon with maps::map_data()
map_state<-map_data("state")

map_state<-map_state|>mutate(passed_through = case_when(region %in% c("florida","alabama","mississippi","louisiana",
                                                                      "texas","new mexico","utah","idaho") ~ "yes",
                                             TRUE ~"no"))


tanya<-"https://raw.githubusercontent.com/tashapiro/drag-race/main/images/cropped/Adriana.png"

label=paste0("<img src='",tanya,"' width='20'/>")

ggplot()+
  geom_polygon(data=map_state, color="white", size=0.2,
               mapping=aes(group=group, x=long, y=lat, fill=passed_through), 
               show.legend = FALSE)+
  scale_fill_manual(values=c("grey80","#7DB9BB"))+
  geom_path(
    data=bike_route,
    aes(x = lon, y = lat),  colour = "grey40",
    size = 0.8, 
    #alpha = .5,
    lineend = "round"
  )+
  geom_path(
    data=biked_miles,
    aes(x = lon, y = lat),  colour = "#A42CFF",
    size = 0.8, 
    #alpha = .5,
    lineend = "round"
  )+
  geom_textbox(
    data=points,
    box.size=NA, halign=0.5, fill=NA,
    mapping=aes(x=lon, y=lat, label=label))+
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
  theme_void()
