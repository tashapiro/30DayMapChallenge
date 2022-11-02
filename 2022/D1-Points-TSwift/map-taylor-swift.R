library(rvest)
library(tidyverse)
library(tidygeocoder)
library(maps)
library(sysfonts)
library(showtext)
library(ggtext)

#import fonts from Google Font library - use abbreviation 
font_add_google("DM Serif Display","dm")
font_add_google("Open Sans","os")
#download fonts https://www.dafontfree.co/neue-haas-grotesk/#google_vignette
sysfonts::font_add('neue', 'fonts/NeueHaasGrosek.otf')
sysfonts::font_add('neuebold', 'fonts/NeueHaasGrosekBold.otf')
#download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
font_add('fs', 'fonts/Font Awesome 6 Free-Solid-900.otf')


showtext_auto()
#adjust dpi in showtext -- fix issues with saving (showtext + ggtext problem)
showtext::showtext_opts(dpi = 300)

#tour dates for Taylor Swift 
df_taylor<-read.csv("tswift_tour.csv")

#geo code city info with tidygeocoder to produce lat/long
geo_tswift<-df_taylor|>geocode(city, method = 'osm', lat = lat , long = long)

geo_tswift<-geo_tswift|>
  arrange(date)|>
  mutate(stop= row_number(),
         date=as.Date(date, "%m/%d/%y"))|>
  group_by(city)|>
  mutate(first_date = min(date))|>
  ungroup()|>
  group_by(first_date)|>
  mutate(id = cur_group_id())


#get state polygon with maps::map_data()
map_state<-map_data("state")

#pre-plotting aesthetics 222945
pal_poly<-"grey80"
pal_line<-"white"
pal_poly<-"#AC9EB8"
pal_dot<-"#1D243E"
pal_bg<-"#1D243E"

#map
map<-ggplot()+
  #us map shapes
  geom_polygon(data=map_state, 
               mapping=aes(group=group, x=long, y=lat),
               fill = pal_poly, color=pal_line, size=0.2)+
  #taylor swift tour locations
  geom_point(data= geo_tswift, 
             mapping = aes(x=long, y=lat),
             shape = 21, fill=pal_dot, color=pal_line, size=4.7)+
  geom_text(data=geo_tswift, 
            mapping=aes(x=long, y=lat, label=id), 
            color="white", family="neue",size=2.2)+
  #coord to round out map
  coord_map("albers",  lat0 = 45.5, lat1 = 29.5)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = pal_bg, color=pal_bg)
  )

map

#create data frame of months for timeline labels
df_months<-data.frame(month = seq(from = min(geo_tswift$date) , to = max(geo_tswift$date) , by="month"),
           abbr = format(seq(from = min(geo_tswift$date) , to = max(geo_tswift$date) , by="month"),"%b"))


df_timeline<-geo_tswift|>
  group_by(id, city, stadium, first_date)|>
  summarise(dates = paste(substr(format(date,"%m/%d"),2,5), collapse=" & "))|>
  mutate(pos_id = case_when(id %% 2 ==0 ~ 0.95, TRUE ~ 1.05),
         pos_label = case_when(id %% 2 ==0 ~ 0.922, TRUE ~ 1.078),
         )|>
  separate(city, into=c("city","state"), sep=", ")|>
  mutate(city = trimws(city),
         city = case_when(city == "East Rutherford"~ "E. Rutherford",
                          city == "Philadelphia" ~ "Philly",
                          TRUE ~ city))

acts<-geo_tswift|>
  mutate(
    openers = trimws(str_replace(openers,"with","")),
    openers = strsplit(openers,", "))|>
  unnest(openers)|>
  distinct(id, city, stadium, first_date, openers)|>
  group_by(id, city, stadium)|>
  mutate(act_count = n(),
         act_num = row_number(),
         date = case_when(act_count==2 & act_num==1 ~ first_date-1.5,
                          act_count==2 & act_num==2 ~ first_date+1.5,
                          act_count==3 & act_num==1 ~ first_date-3,
                          act_count==3 & act_num==2 ~ first_date,
                          act_count==3 & act_num==3 ~ first_date+3,
                             ),
         pos_act = case_when(id %% 2 ==0 ~ 0.9, 
                             TRUE ~ 1.1),
         color = case_when(
           openers == "Paramore" ~ "#D0AF86",
           openers == "girl in red" ~ "#DF4A62",
           openers == "MUNA" ~ "#9CDEF8",
           openers == "Phoebe Bridgers" ~ "white",
           openers == "beabadoobee" ~ "#FE7FAF",
           openers == "HAIM" ~"#BDF5B4",
           openers == "GAYLE" ~ "#CFB1E0ff",
           openers == "OWENN" ~ "#FACD72",
           openers == "Gracie Abrams" ~ "grey60"
         ),
         icon = case_when(
           openers == "Paramore" ~ "<span style='font-family:fs'>&#xf0e7;</span>",
           openers == "girl in red" ~ "<span style='font-family:fs'>&#xf7a6;</span>",
           openers == "beabadoobee" ~ "<span style='font-family:fs'>&#xf025;</span>",
           openers == "GAYLE" ~ "<span style='font-family:fs'>&#xf3a5;</span>",
           openers == "Gracie Abrams" ~ "<span style='font-family:fs'>&#xf004;</span>",
           openers == "HAIM" ~ "<span style='font-family:fs'>&#xf005;</span>",
           openers == "OWENN" ~ "<span style='font-family:fs'>&#xf130;</span>",
           openers == "MUNA" ~ "<span style='font-family:fs'>&#xf8d9;</span>",
           TRUE ~ "<span style='font-family:fs'>&#xf001;</span>"
         ))


pal_timeline<-'#F1E1FF'

#PLOT 2 -- TIMELINE
timeline<-ggplot()+
  #timeline
  geom_segment(mapping=aes(y=1, yend=1, x=min(geo_tswift$date)-10, xend=max(geo_tswift$date)+5), 
               color=pal_timeline, size=0.3, arrow = arrow(ends = "both", length=unit(0.1, units="inches")))+
  #line connectors
  geom_segment(data=df_timeline, mapping=aes(x=first_date, xend=first_date, y=pos_id, yend=0.999), 
               size = 0.3, color=pal_timeline)+
  #month labels
  geom_label(data=df_months, mapping=aes(x=month, y=1, label=abbr), color="white", size=3, fill=pal_dot, family="dm")+
  geom_text(data=df_months, mapping=aes(x=month, y=1, label=abbr), color="white", size=3, family="dm")+
  #stops 
  geom_point(data=df_timeline,
             mapping = aes(x=first_date, y=pos_id),
             color=pal_poly,
             fill = pal_dot, size=5,
             shape = 21)+
  #stop numbers
  geom_text(data=df_timeline, 
            mapping=aes(x=first_date, y=pos_id, label=id), 
            color="white", family="neue",size=2.5)+
  #date and city info
  geom_textbox(data=df_timeline, 
            mapping=aes(x=first_date, y=pos_label, 
                        label=paste0("<span style='font-family:neue;font-size:7pt;'>",city)), 
            fill=NA, box.size=NA, color="white",halign=0.5)+
  #acts
  geom_textbox(data=acts,
             mapping=aes(x=date, y=pos_act, color=color, label=icon), 
             halign=0.5, fill = NA, box.size=NA, show.legend=FALSE, size=2.4)+
  scale_y_continuous(limits=c(0.85,1.15), expand=c(0,0))+
  scale_color_identity()+
  labs(color="Performing With")+
  theme_void()+
  theme(legend.position = "bottom", 
        legend.text = element_text(family="neue", color="white"),
        legend.title = element_text(family="dm", color="white", size=9),
        axis.title=element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill=pal_bg, color=pal_bg))


df_legend<-acts|>
  ungroup()|>
  distinct(openers, color,icon)|>
  arrange(openers)|>
  mutate(id = row_number(),
         y_pos = case_when(id>5 ~ 1, TRUE ~ 1.75))

#create x positions
df_legend$x_pos<-c(seq(from=1, length.out=5, by=0.37),seq(from=1, length.out=4, by=0.37))


#PLOT 3 - LEGEND
legend<-ggplot()+
  geom_textbox(data=df_legend, mapping=aes(y=y_pos, x=x_pos, color=color, label=icon),  
                fill = NA, box.size=NA, size= 3, halign=0.5, show.legend=FALSE)+
  geom_text(data=df_legend, mapping=aes(y=y_pos, x=x_pos+0.04, label=openers), size=2.5, hjust=0, family="neue", color="white")+
  annotate(geom="text", family="dm", color="white", size=3, label = "Performing With", x=0.5, hjust=0, y=1.375)+
  scale_y_continuous(limits=c(0.4,2.2))+
  scale_x_continuous(0, 2)+
  scale_color_identity()+
  theme_void()+
  theme(
    plot.background = element_rect(fill=pal_bg, color=pal_bg)
  )

legend

#title for ggtext::element_textbox_simple
title<-paste0(
  "<span style='font-family:dm;color:white;font-size:35pt;'>TAYLOR SWIFT</span><br>",
  "<span style='font-family:dm;color:white;font-size:33pt;'>THE ERAS TOUR</span>",
  "<br><span style='font-family:neue;color:#D3D3D3;font-size:10pt;'>Map of scheduled tour locations. Artists scheduled to perform with Taylor included in timeline. </span><br>")

#caption for ggtext::element_textoxsimple
caption<-paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:os;'> @tanya_shapiro   </span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:os;'> tashapiro </span>")

#combine plots
ggplot()+
  #legend plot on top
  annotation_custom(ggplotGrob(legend), xmin=0.45, xmax=5.25, ymin=4, ymax=4.4)+
  #map plot in the middle
  annotation_custom(ggplotGrob(map), xmin=-0.1, xmax=6.1, ymin=1, ymax=4)+
  #timeline plot last
  annotation_custom(ggplotGrob(timeline), xmin=0.1, xmax=5.9, ymin=-.4, ymax=1)+
  scale_x_continuous(limits=c(-0.25,6.25), expand=c(0,0))+
  scale_y_continuous(limits=c(-.4,4.3), expand=c(0,0))+
  labs(title=title, caption = caption)+
  theme_void()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_textbox_simple(halign=0.5),
    plot.background = element_rect(fill=pal_bg, color=pal_bg),
    panel.background = element_rect(fill=pal_bg, color=pal_bg),
    plot.caption = element_textbox_simple(color="white", halign=0.98, size=8),
    plot.margin = margin(b=10, t=20)
  )




ggsave("tswift.jpeg", height=8, width = 8, unit="in")

