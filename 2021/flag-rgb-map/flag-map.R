library(ggplot2)
library(dplyr)
library(extrafont)
library(showtext)
library(grid)
library(cowplot)

df<-read.csv("flag_data.csv")

#get shape file for world from map_data
world_map<-map_data("world")
world_map<-world_map%>%filter(region!="Antarctica")
#create id to join on
world_map$join_name<-tolower(world_map$region)
world_map[world_map$join_name=="usa","join_name"]="united states"
world_map[world_map$join_name=="uk","join_name"]="united kingdom"
world_map[world_map$join_name=="ivory coast","join_name"]="côte d’ivoire"
world_map[world_map$join_name=="democratic republic of the congo","join_name"]="congo - kinshasa"
world_map[world_map$join_name=="czech republic","join_name"]="czechia"
world_map[world_map$join_name=="myanmar","join_name"]="myanmar (burma)"
#join_id
df$join_name<-tolower(df$country_en)

#merge data
merged_data<-left_join(world_map,df,by=c("join_name"="join_name"))

missing<-merged_data %>% filter(is.na(col_hex_1))%>%group_by(country,join_name)%>%
  summarise(test=n())


#create new map data sets based on colors
blue_data<-merged_data%>%filter(blue=="col_hex_1"  |  blue=="col_hex_2" | blue== "col_hex_3")
blue_data$hex<-ifelse(blue_data$blue=="col_hex_1",blue_data$col_hex_1,
                      ifelse(blue_data$blue=="col_hex_2",blue_data$col_hex_2,
                             ifelse(blue_data$blue=="col_hex_3",blue_data$col_hex_3, 'grey'
                      )))


red_data<-merged_data%>%filter(red=="col_hex_1"  |  red=="col_hex_2" | red== "col_hex_3")
red_data$hex<-ifelse(red_data$red=="col_hex_1",red_data$col_hex_1,
                      ifelse(red_data$red=="col_hex_2",red_data$col_hex_2,
                             ifelse(red_data$red=="col_hex_3",red_data$col_hex_3, 'grey'
                             )))

green_data<-merged_data%>%filter(green=="col_hex_1"  |  green=="col_hex_2" | green== "col_hex_3")
green_data$hex<-ifelse(green_data$green=="col_hex_1",green_data$col_hex_1,
                     ifelse(green_data$green=="col_hex_2",green_data$col_hex_2,
                            ifelse(green_data$green=="col_hex_3",green_data$col_hex_3, 'grey'
                            )))

#formatting
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path
chart_font<-"Gill Sans"
title_font<-"Gill Sans Bold"

#color format
background<-"#f8f9fa"
line_color<-"white"
no_data_fill<-'#ced4da'

#BLUE MAP
blue_map<-ggplot()+
  geom_polygon(data=world_map,
               aes(x=long,y=lat,group=group),
               fill=no_data_fill,
               color=line_color, size=0.2)+
  geom_polygon(
    data=blue_data,
    aes(x=long,y=lat,group=group, fill=hex),
    color="white", size=0.2
  )+
  scale_fill_identity(guide = "legend", labels = blue_data$hex)+
  theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill=background, color=background),
    panel.background = element_rect(fill=background, color=background), 
    panel.border=element_blank(),
    plot.title=element_text(family=title_font,hjust=.5, vjust=5),
    plot.subtitle =element_text(family=chart_font)
  )+
  coord_map(xlim=c(-180,180))+
  labs(
    title=""
  )

blue_map

#RED MAP
red_map<-ggplot()+
  geom_polygon(data=world_map,
               aes(x=long,y=lat,group=group),
               fill=no_data_fill,
               color=line_color, size=0.2)+
  geom_polygon(
    data=red_data,
    aes(x=long,y=lat,group=group, fill=hex),
    color="white", size=0.2
  )+
  scale_fill_identity(guide = "legend", labels = red_data$hex)+
  theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill=background, color=background),
    panel.background = element_rect(fill=background, color=background), 
    panel.border=element_blank(),
    plot.title=element_text(family=title_font,hjust=.5, vjust=5),
    plot.subtitle =element_text(family=chart_font)
  )+
  coord_map(xlim=c(-180,180))+
  labs(
    title=""
  )

#GREEN MAP
green_map<-ggplot()+
  geom_polygon(data=world_map,
               aes(x=long,y=lat,group=group),
               fill=no_data_fill,
               color=line_color, size=0.2)+
  geom_polygon(
    data=green_data,
    aes(x=long,y=lat,group=group, fill=hex),
    color="white", size=0.2
  )+
  scale_fill_identity(guide = "legend", labels = green_data$hex)+
  theme_void()+
  theme_void()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill=background, color=background),
    panel.background = element_rect(fill=background, color=background), 
    panel.border=element_blank(),
    plot.title=element_text(family=title_font,hjust=.5, vjust=5),
    plot.subtitle =element_text(family=chart_font)
  )+
  coord_map(xlim=c(-180,180))+
  labs(
    title=""
  )

#Create Donut Graphs of Different Hex Values

blue_donut<-blue_data%>%group_by(hex)%>%
  summarise(countries=n_distinct(region))
blue_donut$fraction = blue_donut$countries / sum(blue_donut$countries)
blue_donut$ymax = cumsum(blue_donut$fraction)
blue_donut$ymin = c(0, head(blue_donut$ymax, n=-1))

blue_pie<-ggplot(blue_donut[order(blue_donut$fraction),], aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=hex))+
  geom_rect()+
  scale_fill_identity(guide = "legend", labels = blue_donut$hex)+
  geom_rect() +
  theme(legend.position="none")+
  coord_polar(theta="y") + 
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill=background, color=background),
        panel.background = element_rect(fill=background, color=background), 
        panel.border=element_blank())+
  xlim(c(2, 4)) 

green_donut<-green_data%>%group_by(hex)%>%
  summarise(countries=n_distinct(region))
green_donut$fraction = green_donut$countries / sum(green_donut$countries)
green_donut$ymax <- cumsum(green_donut$fraction)
green_donut$ymin <- c(0, head(green_donut$ymax, n=-1))

green_pie<-ggplot(green_donut[order(green_donut$fraction),], aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=hex))+
  geom_rect()+
  scale_fill_identity(guide = "legend", labels = green_donut$hex)+
  geom_rect() +
  theme(legend.position="none")+
  coord_polar(theta="y") + 
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill=background, color=background),
        panel.background = element_rect(fill=background, color=background), 
        panel.border=element_blank())+
  xlim(c(2, 4)) 

green_pie

red_donut<-red_data%>%group_by(hex)%>%
  summarise(countries=n_distinct(region))
red_donut$fraction = red_donut$countries / sum(red_donut$countries)
red_donut$ymax <- cumsum(red_donut$fraction)
red_donut$ymin <- c(0, head(red_donut$ymax, n=-1))

red_pie<-ggplot(red_donut[order(red_donut$fraction),], aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=hex))+
  geom_rect()+
  scale_fill_identity(guide = "legend", labels = red_donut$hex)+
  geom_rect() +
  theme(legend.position="none")+
  coord_polar(theta="y") + 
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill=background, color=background),
        panel.background = element_rect(fill=background, color=background), 
        panel.border=element_blank())+
  xlim(c(2, 4)) 


p_chart<-grid.arrange(red_pie,green_pie,blue_pie,nrow=1)

p_chart<-cowplot::ggdraw(p_chart) + 
  theme(plot.margin = unit(c(0, 4, 0, 4), "cm"),
        plot.background = element_rect(fill=background, color = NA))

#save pie chart
ggsave("pie_chart.png", width = 32, height = 18, units='cm')


# stitch them together
plot <- grid.arrange(red_map,green_map,blue_map, ncol=2)
# final touch
plot <- cowplot::ggdraw(plot) + 
  theme(plot.background = element_rect(fill=background, color = NA))

plot

#save flag maps
ggsave("flag_maps.png", width = 32, height = 18, units='cm')

