library(ggplot2)
library(rayshader)
library(maps)
library(tidyverse)
library(osmdata)
library(sf)
library(raster)
library(tigris)
#download data - https://www.ncdc.noaa.gov/cag/county/mapping/110/pcp/202110/12/value
df<-read.csv("us_rainfall.csv")
#get florida county map
washington <- counties("Washington", cb = TRUE)
washington$state<-"WA"
#remove "County" from Location
df<-df %>% mutate(Location = word(Location , 1  , -2))
df$state<-substr(df$Location.ID, 1, 2)
df<-df%>%filter(state=="WA")
#merge map dataframe with rainfall dataframe
merged_df <- left_join(washington, df, by=c("NAME"= "Location"))

library(showtext)
library(extrafont)
loadfonts
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path

#create ggplot map of washington rainfall
map<-ggplot()+ 
  geom_sf(data = merged_df%>%filter(state=="WA"), size = 0.3,
          color = "black" ,
             aes(fill=Value))+
  scale_fill_viridis_c()+
  guides(fill = 
           guide_colourbar(
             barwidth = 10,
             direction="horizontal",
             barheight = 1)
  )+
  theme( legend.justification='right',
        legend.position="top",
        panel.background=element_blank(),
        axis.line=element_blank(), 
        axis.text.x=element_blank(), axis.title.x=element_blank(),
        axis.text.y=element_blank(), axis.title.y=element_blank(),
        plot.title=element_text(family="Gill Sans Bold"),
        text=element_text(family="Gill Sans"),
        axis.ticks=element_blank())+
  coord_sf()+
  labs(fill="Inches",
       title="Precipitation Map: Washington State",
       subtitle="Data represents last 12 months from Oct 2021 ",
       caption="Data from NOAA | Chart @tanya_shapiro")


#render side by side image of 2D and 3D maps
par(mfrow = c(1, 2))
plot_gg(map, width = 5, height = 4, raytrace = FALSE, preview = TRUE)
plot_gg(map, 
        width = 5,
        height = 4, 
        scale = 300, 
        multicore = TRUE,
        fov = 70, zoom = 0.7, theta = 35, phi = 40
        )
Sys.sleep(0.2)
render_snapshot("Washington Precipitation.png",clear = TRUE)

