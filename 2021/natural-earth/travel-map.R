library(rnaturalearth)
#devtools::install_github("ropensci/rnaturalearthhires")
library(sf)
library(ggrepel)
library(emojifont)
library(extrafont)
library(showtext)
#install fonts for chart
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path

#download data from rnaturalearth
railroads <- ne_download(scale=10,type = 'railroads', category = 'cultural')
airports <- ne_download(scale=10,type = 'airports', category = 'cultural')
lakes <- ne_download(scale = 10, type = 'lakes', category = 'physical')
rivers <- ne_download(scale = 10, type = 'rivers_europe', category = 'physical')
roads <- ne_download(scale = 10, type = 'roads', category = 'cultural')
pop_places <- ne_download(scale = 10, type = 'populated_places', category = 'cultural')
ports <- ne_download(scale = 10, type = 'ports', category = 'cultural')

#preselect country
country<-"germany"
outline<-ne_countries(scale = 10, type = "countries", continent = NULL,
             country = country, geounit = NULL, sovereignty = NULL,
             returnclass = c("sp", "sf"))


#get intersections of country outline and rnaturalearth data
country_railroads <- raster::intersect(railroads, outline)
country_lakes <- raster::intersect(lakes, outline)
country_rivers <- raster::intersect(rivers, outline)
country_roads <- raster::intersect(roads, outline)
country_airports <- raster::intersect(airports, outline)
country_ports <- raster::intersect(ports, outline)
airport_coords<-data.frame(country_airports@coords)
airport_data<-data.frame(country_airports@data)
country_airports<-data.frame(airport_coords,airport_data)
country_pop_places <- raster::intersect(pop_places, outline)
pop_coords<-data.frame(country_pop_places@coords)
pop_data<-data.frame(country_pop_places@data)
country_pop_places<-data.frame(pop_coords,pop_data)
country_ports <- raster::intersect(ports, outline)
ports_coords<-data.frame(country_ports@coords)
ports_data<-data.frame(country_ports@data)
country_ports<-data.frame(ports_coords,ports_data)

col_water<-"#6DC0D5"
col_background<-'#2E294E'
col_rail<-"#EADEDA"
col_states<-"#D90368"
col_airport<-'#FFD400'
col_roads<-'#EADEDA'
col_font<-"white"

chart_font<-"Gill Sans"
title_font<-"Gill Sans Bold"

#add icons
country_airports$label<-fontawesome('fa-plane')
country_ports$label<-fontawesome('fa-ship')

ggplot(mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(data = outline, fill=col_background)+
  geom_polygon(data = country_lakes, fill=col_water,size=0.2)+
  geom_path(data = country_rivers, color=col_water, size=0.2)+
  geom_path(data = country_railroads, color=col_states,size=0.3)+
  geom_path(data = outline, color='grey80',size=0.3)+
  geom_text_repel(data = country_ports, inherit.aes=FALSE,
                  family='fontawesome-webfont', 
                  color=col_water,
                  aes(x = coords.x1, y = coords.x2, label=label), size=4)+
  geom_text(data = country_airports, inherit.aes=FALSE,
            family='fontawesome-webfont', 
            color=col_airport,
            aes(x = coords.x1, y = coords.x2, label=label), size=6)+
  geom_text_repel(data=country_pop_places%>%filter(FEATURECLA=='Admin-0 capital'),
            family="Gill Sans", inherit.aes=FALSE, color=col_font, size=5,
            nudge_y=0.05, nudge_x=-0.2,
           aes(x = coords.x1, y = coords.x2, label=NAME))+
  geom_text_repel(data=country_pop_places%>%filter(FEATURECLA!='Admin-0 capital' & MEGACITY==1),
                  family="Gill Sans", inherit.aes=FALSE, color=col_font, size=4,
                  direction = "both",
                  box.padding = 0.5,
                  nudge_y=-0.1,
                  max.overlaps=3,
                  aes(x = coords.x1, y = coords.x2, label=NAME))+
  coord_map()+
  theme_void()+
  theme(
        text=element_text(family=chart_font, color="white"),
        plot.title=element_text(family=title_font, face="bold", hjust=0.05, vjust=-1),
        plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
        plot.caption = element_text(margin=margin(10,0,0,0)),
        plot.background = element_rect(fill=col_background,color=NA))+
  labs(title=toupper(country))




