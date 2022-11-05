library(tidyverse)
library(sf)
library(raster)
library(ggtext)
library(sysfonts)
library(showtext)
library(spMaps)


#download Kyiv Type (by Ukrainian font designer Dmytro Rastvort) font https://fontesk.com/kyiv-type-typeface/
sysfonts::font_add('kyiv', regular= 'fonts/KyivTypeTitling-Regular.otf', bold='fonts/KyivTypeTitling-Medium.otf')
#download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Poppins","defaultsans")
showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)


#map from spMaps
outline<-getSpMaps(countries = "UKR")

#Map Cropping based on Josh O'Brien's answer form Stack Overflow: https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe

#create bounding boxes for top half and lower half
box_top <- as(extent(22, 41, 49, 53), "SpatialPolygons")
box_bottom <- as(extent(22, 41, 42, 49), "SpatialPolygons")

proj4string(box_top) <- CRS(proj4string(outline))
proj4string(box_bottom) <- CRS(proj4string(outline))

#create new polygons using the intersection from bounding boxes
top_ukraine <- gIntersection(outline, box_top, byid=TRUE)
bottom_ukraine <- gIntersection(outline, box_bottom, byid=TRUE)

#palette - Ukrainian Flag
pal_blue<-"#0057b7"
pal_gold<-"#ffd700"
pal_bg<-"#292E31"

#plot title & caption
title<-paste0(
  "<span style='font-family:defaultsans;font-size:15pt;color:white;'>**#30DayMapChallenge**</span><br>",
  "<p><span style='font-family:kyiv;font-size:10pt;color:#ffd700;'>Kyiv Type Tilting</span>",
  "<span style='font-family:defaultsans;font-size:10pt;color:white;'> font by Dmytro Rastvortsev</span></p>")

caption<-paste0(
  "<span style='font-family:fb;color:grey80;font-size:8pt;'>&#xf099;</span>",
  "<span style='font-family:defaultsans;color:grey80;font-size:8pt;'> @tanya_shapiro   </span>",
  "<span style='font-family:fb;color:grey80;font-size:8pt;'>&#xf09b;'</span>",
  "<span style='font-family:defaultsans;color:grey80;font-size:8pt;'> tashapiro</span>"
)


#PLOT
ggplot()+
  geom_polygon(data=outline,
               mapping=aes(group=group, x=long, y=lat),
               fill=NA, color="white", size=4)+
  geom_polygon(data=outline,
               mapping=aes(group=group, x=long, y=lat),
               fill=NA, color=pal_bg, size=0.25)+
  geom_polygon(data=top_ukraine,
               mapping=aes(group=group, x=long, y=lat),
               fill=pal_blue)+
  geom_polygon(data=bottom_ukraine,
               mapping=aes(group=group, x=long, y=lat),
               fill=pal_gold)+
  geom_text(mapping=aes(x=29.5, y=49.75, label="SLAVA"), 
                  size=15, color=pal_gold,family="kyiv", fontface="bold")+
  geom_text(mapping=aes(x=33.8, y=48.25, label="UKRAINI"), 
                  size=15, color=pal_blue, family="kyiv", fontface="bold")+
  #title
  geom_textbox(mapping=aes(label=title, x=37, y=43), halign=1, box.size=NA, fill=NA,
               width = unit(4, "inch"))+
  #caption
  geom_textbox(mapping=aes(label=caption, x=22.65, y=42.5), halign=0.5, box.size=NA, fill=NA,
               width = unit(4, "inch"))+
  coord_map()+
  scale_y_continuous(limits=c(42,53), expand=c(0,0))+
  scale_x_continuous(limits=c(19,44), expand=c(0,0))+
  theme_void()+
  theme(
    plot.background = element_rect(fill=pal_bg, color=pal_bg),
    plot.margin = margin(t=30, b=5)
  )


#save plot
ggsave("slava-ukraini.png", height=5.3, width=9, units="in")
