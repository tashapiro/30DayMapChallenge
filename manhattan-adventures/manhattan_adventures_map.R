library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)
library(ggplot2)
library(dplyr)


#OSM Data Sets for Map
manhattan_streets <- getbb("Manhattan")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

ny_bridges <-getbb("New York")%>%
  opq()%>%
  add_osm_feature(key = "bridge") %>%
  osmdata_sf()

manhattan_river <- getbb("Manhattan")%>%
  opq()%>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()

manhattan_water <- getbb("Manhattan")%>%
  opq()%>%
  add_osm_feature(key = 'water', value = c('lake','pond',
                                           'river', 'stream',
                                           'reservoir','canal'
                                          )) %>%
  osmdata_sf()
manhattan_water <- manhattan_water$osm_multipolygons

nj_water<-getbb("New Jersey")%>%
  opq()%>%
  add_osm_feature(key = 'water', value = c('lake','pond',
                                           'river', 'stream',
                                           'reservoir','canal'
  )) %>%
  osmdata_sf()

ny_water<-getbb("New York")%>%
  opq()%>%
  add_osm_feature(key = 'water') %>%
  osmdata_sf()


#park ids from osm
id = c(427818536,197100516,22727025,138141251)
#sf with major NYC Parks
parks<-opq_osm_id (type = "way", id = id) %>%
  opq_string () %>%
  osmdata_sf ()


#data with date spots
our_spots<-read.csv("our_spots.csv")

#font import
font_add("Lemon Milk Bold", "/Library/Fonts/LEMONMILK-BOLD.otf")
title_font<-"Lemon Milk Bold"

font_add("Lemon Milk Light", "/Library/Fonts/LEMONMILK-LIGHT.otf")
chart_font<-"Lemon Milk Light"

#Dark Version of Manhattan Adventures

#dark map color palette
background_color<-'#151515'
street_color<-'#BCBDC0'
river_color<-'#1C7293'
font_color<-'#FFFFFF'
dot_colors <-c('#FFD23F','#3BCEAC','#EE4266','#6976EA')
park_color<-'#1E441E'

manhattan_dark<-ggplot() +
  geom_sf(data = parks$osm_polygons,
          inherit.aes = FALSE,
          colour = NA,
          fill = park_color)+
  geom_sf(data = manhattan_water, fill = river_color, colour = NA) +
  geom_sf(data = ny_water$osm_polygons, fill = river_color, colour = NA) +
  geom_sf(data = manhattan_river$osm_lines,
          inherit.aes = FALSE,
          color = river_color,
          size = .3,
          alpha = .8) +
  geom_sf(data = manhattan_streets$osm_lines,
          inherit.aes = FALSE,
          color = street_color,
          size = .3,
          alpha = .8) +
  geom_sf(data = ny_bridges$osm_lines,
          inherit.aes = FALSE,
          color = street_color,
          size = .2,
          alpha = .8) +
  geom_point(data=our_spots, aes(Longitude,Latitude, color=Category)
             )+
  scale_color_manual(values=dot_colors)+
  guides(colour = guide_legend(title.position = "top", title.hjust =0.5))+
  coord_sf(xlim = c(-74.04722, -73.90616), 
           ylim = c(40.68394, 40.88045),
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(family=title_font,
                                  color=font_color,
                                  size = 18, face="bold", hjust=.5),
        plot.subtitle = element_text(color=font_color,
                                     family=chart_font,
                                     size = 12, hjust=.5,
                                     margin=margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = background_color),
        legend.position="top",
        legend.text = element_text(color=font_color, size=7,
                                   family=chart_font),
        legend.title = element_text(color=font_color, size=9,
                                    family=chart_font),
        plot.margin=unit(c(0,1.6,0.4,1.6),"cm")
  ) +
  labs(title = "TANYA & KRISTEN",
       subtitles="Adventures in the city",
       color="CATEGORY"
       )


ggsave("manhattan_adventures_dark.jpeg", width=5.7, height=9.35)




#Light Version of Manhattan Adventures

#light map color palette
font_color2<-'#151515'
street_color2<-'#746C70'
river_color2<-'#57B8FF'
background_color2<-'#FFFFFF'
dot_colors2 <-c('#f8bf04','#3BCEAC','#EE4266','#6976EA')
park_color2<-'#62AB37'

manhattan_light<-ggplot() +
  geom_sf(data = parks$osm_polygons,
          inherit.aes = FALSE,
          colour = NA,
          fill = park_color2)+
  geom_sf(data = manhattan_water, fill = river_color2, colour = NA) +
  geom_sf(data = ny_water$osm_polygons, fill = river_color2, colour = NA) +
  geom_sf(data = manhattan_river$osm_lines,
          inherit.aes = FALSE,
          color = river_color2,
          size = .3,
          alpha = .8) +
  geom_sf(data = manhattan_streets$osm_lines,
          inherit.aes = FALSE,
          color = street_color2,
          size = .3,
          alpha = .8) +
  geom_sf(data = ny_bridges$osm_lines,
          inherit.aes = FALSE,
          color = street_color2,
          size = .2,
          alpha = .8) +
  geom_point(data=our_spots, aes(Longitude,Latitude, color=Category)
  )+
  scale_color_manual(values=dot_colors2)+
  guides(colour = guide_legend(title.position = "top", title.hjust =0.5))+
  coord_sf(xlim = c(-74.04722, -73.90616), 
           ylim = c(40.68394, 40.88045),
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(family=title_font,
                                  color=font_color2,
                                  size = 18, face="bold", hjust=.5),
        plot.subtitle = element_text(color=font_color2,
                                     family=chart_font,
                                     size = 12, hjust=.5,
                                     margin=margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = background_color2),
        legend.position="top",
        legend.text = element_text(color=font_color2, size=7,
                                   family=chart_font),
        legend.title = element_text(color=font_color2, size=9,
                                    family=chart_font),
        plot.margin=unit(c(0,1.6,0.4,1.6),"cm")
  ) +
  labs(title = "TANYA & KRISTEN",
       subtitles="Adventures in the city",
       color="CATEGORY"
  )

manhattan_light

ggsave("manhattan_adventures_light.jpeg", width=5.7, height=9.35)

