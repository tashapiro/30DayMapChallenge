library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(sp)

# Load shapefile
sp <- st_read('nyc_mappluto/MapPLUTO_UNCLIPPED.shp')


'%ni%' <- Negate('%in%')
#take a subset of NY data, focus on Queens Zips
sample<-sp%>%subset(ZipCode %in%c(11101,11102,11104,11103,11106,11378,11377,11373,11372) & ZoneMap %ni% c("10a","14d","9c","11a"))
#convert 0s for Year Built to NA
sample<-sample%>%mutate(YearBuilt = replace(YearBuilt, YearBuilt == 0, NA),
                            YearAlter1 = replace(YearAlter1, YearAlter1 == 0, NA),
                            YearAlter2 = replace(YearAlter2, YearAlter2 == 0, NA)
)

#create breaks for year built grouping
sample$YearGroup<-cut(sample$YearBuilt, breaks = c(0,1899,1919,1939,1959,1979,1999,2021),
                        labels=c("<1900","1900-1919","1920-1939","1940-1959","1960-1979","1980-2000","2000-2020")
)

#ggplot map
ggplot(data = sample) +
  geom_sf(aes(fill = YearGroup), color = "grey20", size = 0.15)+
  scale_fill_viridis_d(na.value="grey90")+
  coord_sf()+
  theme_void()+
  labs(
      title="QUEENS | Year of Construction",
       subtitle="Original year built of buildings in Queens, NY",
       caption="Data from MapPLUTO (NYC Planning) | Chart @tanya_shapiro",
       fill="Year Built")+
  theme(
    plot.title=element_text(family="Gill Sans Bold",size=14, hjust=0.89, vjust= - 35),
    plot.subtitle=element_text(family="Gill Sans",size=10,hjust=0.86, vjust=-50),
        text=element_text(family="Gill Sans"),
    plot.caption = element_text(margin=margin(0,0,10,0)),
        legend.position = c(0.15, 0.2)
        )

ggsave("nyc_urban.jpeg")