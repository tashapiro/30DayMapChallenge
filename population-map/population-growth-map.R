
library(ggplot2)
library(tidyverse)
library(maps)
library(gganimate)
library(dplyr)
library(showtext)
library(extrafont)
#data taken from https://ourworldindata.org/world-population-growth
df<-read.csv("population-growth-rates.csv")
#get map shape file
world_map<-map_data("world")
#remove antarctica
world_map<-world_map%>%filter(region!="Antarctica")
#relabel USA and UK before merge
world_map["region"][world_map["region"] == "USA"] <- "United States"
world_map["region"][world_map["region"] == "UK"] <- "United Kingdom"
world_map["region"][world_map["region"] == "Czech Republic"] <- "Czechia"
world_map["region"][world_map["region"] == "Democratic Republic of the Congo"] <- "Congo"
world_map["region"][world_map["region"] == "Republic of Congo"] <- "Democratic Republic of Congo"
world_map["region"][world_map["region"] == "Ivory Coast"] <- "Cote d'Ivoire"
world_map["region"][world_map["region"] == "Swaziland"] <- "Eswatini"
#create breaks
df$breaks<-cut(df$estimates,breaks=c(-4,-2,-1,-0.5,0,0.5,1,1.5,2,4),
    include.lowest = TRUE)
#merge data
merged_df<-left_join(world_map,df,by=c("region"="Entity"))

#fonts
chart_font<-"Gill Sans"
bold_font<-"Gill Sans Bold"

pal2<-c(
  '#b2182b', #R4 -4 to -2
  '#d6604d', #R3 -2 to -1
  '#f4a582', #R2 -1 to -0.5
  '#fddbc7', #R1 -0.5 to 0
  #'#f7f7f7', #white 0 to 0.25
  '#e4eef3',
  '#d1e5f0', #B1 0.25 to 0.5
  '#92c5de', #B2 0.5 to 0.1
  '#4393c3', #B3 0.5 to 1
  '#2166ac', #B4 1 to 2
  '#1a5189' #B5 2 to 4
)

values<-c("[-4,-2]",
          "(-2,-1]",
          "(-1,-0.5]",
          "(-0.5,0]",
          "(0,0.5]",
          "(0.5,1]",
          "(1,1.5]",
          "(1.5,2]",
  "(2,4]")

value_labels<-c("[-4,-2]",
          "[-2,-1]",
          "[-1,-0.5]",
          "[-0.5,0]",
          "[0,0.5]",
          "[0.5,1]",
          "[1,1.5]",
          "[1.5,2]",
          "[2,4]")

#test with regular map
ggplot()+
  geom_polygon(data=merged_df%>% filter(Year==2020),
               aes(x=long,y=lat,group=group,fill=breaks),
               color='grey20', size=0.2)+
  scale_fill_manual(values=pal2, breaks=values,
                    labels = value_labels,
                    na.translate = F)+
  guides(fill = 
           guide_legend(title.position = "top",
                        label.position="bottom",
                        title.hjust =0.5,
                        nrow = 1))+
  coord_map(xlim=c(-180,180))+
  theme_void()+
  theme(
     text=element_text(family=chart_font),
      plot.title=element_text(family=bold_font),
    legend.position="bottom",
    legend.key = element_rect(colour = "black"),
    legend.key.width = unit(1, 'cm'),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")
  )+
  labs(
    title="Population Growth Rate: 2020",
    subtitle="Annual Population Growth Rate Percentage",
    caption="Data from OurWorldInData.org | Chart @tanya_shapiro",
    fill="Percentage Range"
  )



ggsave("pop_2020.jpeg", width = 24, height = 13.5, units='cm')

#decades map
years<-1990:2020
years_df<-merged_df%>% filter(Year%in%years)
years_map<-ggplot(years_df)+
  geom_polygon(data=years_df,
               aes(x=long,y=lat,group=group,fill=breaks),
               color='grey20', size=0.2)+
  scale_fill_manual(values=pal2, breaks=values,
                    labels = value_labels,
                    na.translate = F)+
  guides(fill = 
           guide_legend(title.position = "top",
                        label.position="bottom",
                        title.hjust =0.5,
                        nrow = 1))+
  coord_map(xlim=c(-180,180))+
  theme_void()+
  theme(
    text=element_text(family=chart_font),
    plot.title=element_text(family=bold_font),
    legend.position="bottom",
    legend.key = element_rect(colour = "black"),
    legend.key.width = unit(1, 'cm')
  )+
  transition_time(Year)+
  labs(
    title="Population Growth Rate: {frame_time}",
    subtitle="Annual Population Growth Rate Percentage",
    caption="Data from OurWorldInData.org | Chart @tanya_shapiro",
    fill="Percentage Range"
  )


animate(years_map, 
        width=1200, height=675,
        res=150,
        fps=3, nframes=31,renderer = gifski_renderer(loop=FALSE, "population_density.gif"))


