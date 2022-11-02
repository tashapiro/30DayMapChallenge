library(ggplot2)
library(raster)
library(tidyr)
library(rnaturalearth)
library(dplyr)
library(viridis)
library(countrycode) #get list of iso codees
library(maps) #get city lat longs
library(ggrepel)

country<-"Nepal"
country_codes<-codelist
country_codes<-country_codes%>%filter(country.name.en==country)
iso_code<-country_codes$iso3c

#Get Altitude Data
country_raster<-getData('alt',country=iso_code,mask=TRUE)
#Manipulate Raster File
country_raster_df<-as.data.frame(country_raster,xy=TRUE)%>%drop_na()
colnames(country_raster_df)[3] <- "altitude"


world_cities<-world.cities
country_cities<-world_cities%>%filter(country.etc==country)
top_cities<-top_n(country_cities,10,pop)



library(showtext)
library(extrafont)
loadfonts
font_import()


font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path

chart_font<-"Gill Sans"
background<-"grey75"
font_color<-"grey20"
city_font<-"white"
bold_font<-"Gill Sans Bold"

#Coords for Nepal Mountains
long<-c(86.925, 84.559, 88.1475, 86.9336, 87.0876, 86.6615)
lat<-c(27.9881, 28.5497, 27.7025, 27.9626, 27.8857, 28.0960)
feet<-c(29032,26781,28169,27940,27766,26864)
label<-c("Everest","Manaslu","Kangchenjunga","Lhotse","Makalu","Cho Oyu")
mountains<-data.frame(label,long,lat,feet)
mountains$meters<-round(mountains$feet/3.281,0)


ggplot()+
  geom_raster(data=country_raster_df,aes(x=x,y=y,fill=altitude))+
  scale_fill_viridis_c(name='Altitude (Meters)',option="magma",direction=1,
                       trans="log",
                       breaks=c(0,150,400,1100,3000,8000)
                       )+
  guides(fill = 
           guide_colourbar(
             barwidth = 20,
             barheight = 1,
             direction="horizontal",
             frame.colour="grey20",
             title.position="top"))+
  geom_point(data=top_cities,aes(x=long,y=lat), shape=21, fill="white",
            color="grey20")+
  geom_text_repel(data=top_cities,aes(x=long,y=lat,label=name),
                color=city_font, family=chart_font)+
  geom_point(data=top_n(mountains,3,meters),aes(x=long,y=lat),
             color="grey20", shape=9)+
  geom_label_repel(data=top_n(mountains,3,meters),aes(x=long,y=lat,label=label),
                  family=chart_font)+
  coord_sf()+
  theme_void()+
  theme(
    #legend.position="top",
    legend.position=c(1,0.85),
    legend.justification='right',
    legend.margin = margin(0, 50, 0, 0),
    plot.background = element_rect(fill=background),
    text=element_text(color=font_color,family=chart_font),
    plot.title=element_text(size=20, family=bold_font,hjust=0.05,margin=margin(20,0,0,0)),
    plot.subtitle=element_text(size=14,hjust=0.05),
    legend.text=element_text(color="grey20"),
    legend.title=element_text(family=chart_font),
    plot.caption = element_text(size=10,hjust=0.05, vjust=5)
  )+
  labs(title="SUCH GREAT HEIGHTS | NEPAL",
       subtitle="Altitude map of Nepal. Altitude expresesed in meters.",
       caption="Data from STRM | Chart @tanya_shapiro",
       fill="Altitude (meters)")



ggsave("nepal_map.jpeg", width = 23, height = 14.0625, units='cm')
