library(tidyverse)
library(osmdata)
library(sf)
library(extrafont)



getbb("Basel Switzerland")


streets <- getbb("Basel Switzerland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()
streets

small_streets <- getbb("Basel Switzerland")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()


river <- getbb("Basel Switzerland")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()


background_color<-'#1E212B'
street_color<-'#FAD399'
small_street_color<-'#D4B483'
river_color<-'#0ACDFF'
font_color<-'#FFFFFF'

font_add("Courier New")
font_add("Optimus Princeps", "/Library/Fonts/OptimusPrinceps.ttf")
chart_font<-"Optimus Princeps"
basel_dark<-ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = street_color,
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = small_street_color,
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = river_color,
          size = .2,
          alpha = .5) +
  coord_sf(xlim = c(7.55, 7.64), 
           ylim = c(47.52, 47.59),
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(family=chart_font,
                                  color=font_color,
                                  size = 18, face="bold", hjust=.5,
                                  vjust=2.5),
        panel.border = element_rect(colour = "white", fill=NA, size=3),
        plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
        plot.subtitle = element_text(color=font_color,
                                     family=chart_font,
                                     vjust=2.5,
                                     size = 12, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = "#282828")) +
  labs(title = "BASEL, SWITZERLAND", subtitle = "47.559°N | 7.588°E")


ggsave("basel_map_dark.jpeg", width=8, height=9)

background_color2<-'#faf9ed'
street_color2<-'#13130c'
small_street_color2<-'#37261a'
river_color2<-'#5985ab'
font_color2<-'#13130c'

basel_light<-ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = river_color2,
          alpha=.5,
          size = .4) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = street_color2,
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = small_street_color2,
          size = .2,
          alpha = .6) +
  coord_sf(xlim = c(7.55, 7.64), 
           ylim = c(47.52, 47.59),
           expand = FALSE) +
  theme_void() +
  theme(plot.title = element_text(family=chart_font,
                                  color=font_color2,
                                  size = 18, face="bold", hjust=.5,
                                  vjust=2.5),
        panel.border = element_rect(colour = font_color2, fill=NA, size=3),
        plot.margin=unit(c(0.6,1.6,1,1.6),"cm"),
        plot.subtitle = element_text(color=font_color2,
                                     family=chart_font,
                                     vjust=2.5,
                                     size = 12, hjust=.5, margin=margin(2, 0, 5, 0)),
        plot.background = element_rect(fill = background_color2)) +
  labs(title = "BASEL, SWITZERLAND", subtitle = "47.559°N | 7.588°E")

basel_light

ggsave("basel_map_light.jpeg", width=8, height=9)

grid.arrange(basel_dark,basel_light, nrow=1)
