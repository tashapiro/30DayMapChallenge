library(tidyverse)
library(ggtext)
library(ggrepel)
library(osmdata)
library(sf)
library(sysfonts)
library(showtext)

sysfonts::font_add_google("galada")
showtext_auto()

#get coord plot for Staten Island
coords<-getbb("Staten Island")

#get polygon shape
shape <- getbb("Staten Island, New York",  format_out = "sf_polygon")
shape<-shape[3,]

#gather main streets
streets <- coords%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

#eliminate streets outside of Staten Island shape
streets<-streets$osm_lines%>%
  st_intersection(shape)


small_streets <-coords%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

small_streets<-small_streets$osm_lines%>%
  st_intersection(shape)


leisure <- coords%>%
  opq()%>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()

leisure_poly<-leisure$osm_polygons%>%
  st_intersection(shape)


pizza_shops<-read.csv("https://raw.githubusercontent.com/tashapiro/30DayMapChallenge/main/staten_island/si_pizza.csv")

title = "<span style='font-family:galada;font-size:22pt;'>**Slice of Staten Island**</span><br><br>
<span style='font-size:10pt;'>Map of Staten Island pizzerias and their<br> respective Google review scores. Best pizzerias <br>with a minimum of 600 reviews  are labeled.</span>"

pal_bg<-'#16324F'
pal_shape<-'#2A628F'
pal_road<-'#46B3FF'
pal_font<-'white'
pal_park<-'#54B66F'
#plot
ggplot()+
  geom_sf(data=shape, color=pal_road, fill=pal_shape)+
  geom_sf(data = streets,
          inherit.aes = FALSE,
          color = pal_road,
          size = .4)+
  geom_sf(data = small_streets,
          inherit.aes = FALSE,
          color = pal_road,
          size = .2,
          alpha = .8)+
  geom_sf(data=leisure_poly%>%filter(leisure %in% c("nature_reserve","park","golf_course")),
          fill=pal_park,
          color=pal_shape
  )+
  geom_point(data=pizza_shops,
             inherit.aes = FALSE, color = "white",
             aes(x=Longitude,y=Latitude, fill=review_avg),
             alpha = 1, size =4, shape = 21,
             stroke=0.6)+
  geom_label_repel(data=pizza_shops%>%filter(review_avg>=4.2 & reviews>=600),
                   aes(x=Longitude,y=Latitude, label=abbr), fill=pal_bg, color="white",
                   size=2.5)+
  geom_richtext(aes(x=-74.08, y=40.515, label=title),
                color="white", label.color = NA, fill = NA)+
  scale_y_continuous(limits=c(40.49,40.66))+
  scale_x_continuous(limits=c(-74.26,-74.04))+
  scale_fill_steps(n.breaks=6, low ="#EF3054", high="#FFE74C",
                   guide=guide_legend(override.aes = list(size=10)))+
  guides(fill = guide_legend(title.position = "bottom",
                        title.hjust =0.5,
                        title="Avg Google Review",
                        label.position = "bottom",
                        nrow = 1))+
  labs(caption="Source OpenStreetMap & Google | Chart @tanya_shapiro")+
  theme_void()+
  theme(
    text=element_text(color="white"),
    plot.caption=element_text(color=pal_road, hjust=0.05),
    plot.background = element_rect(fill=pal_bg, color=NA),
    legend.title=element_text(size=9, face="bold", margin=margin(t=5)),
    legend.position = c(0.79, 0.0475),
    plot.margin = margin(t=20,b=20,l=20,r=20)
  )


#ggsave("pizzerias.png", height=8, width=8)
