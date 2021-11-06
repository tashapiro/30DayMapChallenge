library(ggplot2)
library(maps)
library(usdata)
library(dplyr)
library(extrafont)
library(showtext)
library(ggtext)
library(ggrepel)
library(tibble)

#data for beef and buffalo consumption
df<-read.csv("beef-buffalo-consumption.csv")
#get map shape file
world_map<-map_data("world")
#remove antarctica
world_map<-world_map%>%filter(region!="Antarctica")

#scrub data from df to match entity to region names in map
df["Entity"][df["Entity"] == "United States"] <- "USA"
df["Entity"][df["Entity"] == "United Kingdom"] <- "UK"

#get most recent year of data
df_2013<-df%>%filter(Year==2013)


df_2013$bin<-
  ifelse(df_2013$kg_capita>=40,"+40",
         ifelse(df_2013$kg_capita>=30,"30-39",
                ifelse(df_2013$kg_capita>=20,"20-29",
                       ifelse(df_2013$kg_capita>=15,"15-19",
                              ifelse(df_2013$kg_capita>=10,"10-14",
                                     ifelse(df_2013$kg_capita>=5,"5-9",
                                            "<5"))))))


df_2013$bin<-factor(df_2013$bin, levels=c("<5","5-9","10-14","15-19","20-29","30-39","+40"))

#merge world map with data on meat consumpsion
world_map_df<-left_join(world_map,df_2013, by=c("region"="Entity"))
summary(df_2013$kg_capita)

#colors from R Color Brewer



#fonts
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path
chart_font<-"Gill Sans"
title_font<-"Gill Sans Bold"

#text for annotations
text_label<-c("Top beef and buffalo meat consumption in Argentina (55.5kg), followed by Brazil (39.3kg)",
              "United States ranked as 3rd highest conusmption (32.2kg)",
              "Liberia lowest consumption (0.78kg)",
              "India 2nd lowest consumption (0.81kg). Roughly 80% population practices Hinduism (prohibits beef).")
text_lat<-c(-50,30,-10,-20)
text_long<-c(-28,-150,-10,75)
text_desc<-c("C1","C2","C3","C4")
text_df<-data.frame(text_label,text_lat,text_long,text_desc)

#points
point_desc<-c("ARG","IND","USA","LIB","BRZ")
point_lat<-c(-36.416,20.593,40,7.4,-14.23)
point_long<-c(-63.616,78.96,-110,-9.4,-51.92)
point_df<-data.frame(point_desc,point_lat,point_long)

#x is long, #y is lat
#arrows to point between annotations and dots
arrows <- tibble(
  x2 = c(-63.616, #Long Arg Point
         78.96, #Long India Point
         -110, #Long USA Point
         -9.4, #Lont Liberia Point
         -51.92 #Long Brazil Point
  ),
  x1 = c(-28,  #Long Arg Comm
         75, #Long India Comm
         -150, #Long USA Comm
         -10, #Long Lib Comm
         -28 #Long Braz Comm
  ),
  y2 = c(-36.416,  #Lat Arg Point
         20.593,  #Lat Ind Point
         40, #Lat USA Point
         7.4, #Lat Lib Point
         -14.23 #Lat Brazil Point
  ),
  y1 = c(-50,  #Lat Arg Comm
         -20, #Lat India Comm
         30, #Lat US Comm
         -10, #Lat Lib Comm
         -50 #Lat Brazil Comm
  ) 
)

#setcolors
#fill colors
colors<-c(
  '#fee5d9',
  '#fcbba1',
  '#fc9272',
  '#fb6a4a',
  '#ef3b2c',
  '#cb181d',
  '#99000d')
background<-'#1C1D21'
font_color<-"white"
annotation_color<-"#FF9D00"

ggplot()+
  geom_polygon(
    data=world_map_df,
    aes(x=long,y=lat,group=group, fill=bin),
    color=background, size=0.2
  )+
  guides(fill = 
           guide_legend(title.position = "top",
                        title.hjust =0.5,
                        nrow = 1))+
  geom_polygon(
    data=world_map_df%>%filter(is.na(kg_capita)),
    aes(x=long,y=lat,group=group),
    fill="grey",
    color=background, size=0.2
  )+
  scale_fill_manual(values=colors, na.translate = F)+
  geom_point(
    data=point_df,
    inherit.aes=F,
    aes(x=point_long,y=point_lat),
    color='#2E1F27'
  )+
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
               color = annotation_color)+
  geom_textbox(
    data=text_df%>%filter(text_desc %in% c("C1","C4")),
    aes(x=text_long,y=text_lat,label=text_label),
    size=2.5,
    width = unit(0.1, "npc"),
    fill=background,
    color="white",
    box.color=annotation_color
  )+
  geom_textbox(
    data=text_df%>%filter(text_desc %in% c("C3","C2")),
    aes(x=text_long,y=text_lat,label=text_label),
    size=2.5,
    width = unit(0.07, "npc"),
    fill=background,
    color="white",
    box.color=annotation_color
  )+
  coord_map(xlim=c(-180,180))+
  theme_void()+
  theme(
    plot.background=element_rect(fill=background),
    legend.position="top",
    text=element_text(color=font_color, family=chart_font),
    plot.title=element_text(family=title_font),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )+
  labs(
    title="Beef & Buffalo Meat Consumption 2013",
    subtitle="Consumption based on kg per capita",
    fill="Kg Per Capita",
    caption="Data from OurWorldInData.org | Chart @tanya_shapiro"
  )


ggsave("red_beef_map.jpeg", width = 26.5, height = 18, units='cm')
