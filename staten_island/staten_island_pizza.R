library(osmdata)
library(ggplot2)
library(sf)
library(dplyr)
library(raster)
library(rgeos)

#get coord plot for Staten Island
coords<-getbb("Staten Island")

#get polygon shape
shape <- getbb("Staten Island", format_out = "sf_polygon")

#gather main streets
streets <- coords%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

streets<-streets$osm_lines%>%
  st_intersection(shape$multipolygon)

small_streets <-coords%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

small_streets<-small_streets$osm_lines%>%
  st_intersection(shape$multipolygon)

leisure <- coords%>%
  opq()%>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()

leisure_poly<-leisure$osm_polygons%>%
  st_intersection(shape$multipolygon)

pizza_shops<-read.csv("si_pizza.csv")

col_island<-'#2A628F'
col_road<-'#46B3FF'
col_water<-'#16324F'

library(ggrepel)

ggplot() +
  geom_sf(data=shape$multipolygon,
          fill=col_island,
          color=col_road,
          size=.4)+
  geom_sf(data = streets,
          inherit.aes = FALSE,
          color = col_road,
          size = .4)+
  geom_sf(data = small_streets,
          inherit.aes = FALSE,
          color = col_road,
          size = .2,
          alpha = .8)+
 geom_sf(data=leisure_poly%>%filter(leisure %in% c("nature_reserve","park","golf_course")),
          fill="#52BF93",
          color=col_island
  )+
  geom_point(data=pizza_shops,
          inherit.aes = FALSE, color = "white",
          #fill="white",
          aes(x=Longitude,y=Latitude, fill=review_avg),
          alpha = 1, size =4, shape = 21,
          stroke=0.6)+
  geom_label_repel(data=pizza_shops%>%filter(review_avg>=4.2 & reviews>=600),
            aes(x=Longitude,y=Latitude, label=abbr), fill=col_water, color="white",
            size=3.25,
            family="Gill Sans")+
 # scale_fill_gradient(low = "white", high = "#F35B04", na.value = NA)+
  scale_fill_steps(n.breaks=6, low ="#EF3054", high="#FFE74C",
                   guide=guide_legend(override.aes = list(size=10)))+
  guides(fill = 
           guide_legend(title.position = "top",
                        title.hjust =0.5,
                        label.position = "bottom",
                        nrow = 1)
         )+
  theme_void()+
  theme(
    plot.background = element_rect(fill=col_water),
    text=element_text(family="Gill Sans",color="white"),
    plot.margin = unit(c(0, 2, 0.2, 2), "cm"),
    plot.title =element_text(family="Gill Sans Bold",color="white",size=20, hjust=0.99, vjust= - 78),
    plot.subtitle=element_text(size=14, hjust=0.92, vjust=-97.5),
    plot.caption=element_text(size=10, margin=margin(t=10,b=5)),
    legend.position = c(0.1,1)
  )+
  labs(title="Slice of Staten Island",
       subtitle="Map of Staten Island Pizzerias",
       caption= "Data from Google & OpenStreetMap | Chart @tanya_shapiro",
       fill="Google Review Average"
  )
                         

ggsave("staten_island.png")
#ggsave("staten_island.png", width = 9.5, height = 10, units='in')                          

