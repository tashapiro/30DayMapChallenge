library(ddplyr)
library(dplyr)
library(maps)
library(ggplot2)
library(stringr)
library(ggrepel)
library(extrafont)

#read data
df_2017 <- read.csv("governor_election_2017.csv")

#get county map data from maps library, filter for VA counties only
va_county_map<-map_data("county")%>%filter(region=='virginia')

#format county, remove "county" from name, convert to lowercase
df_2017<- df_2017 %>% mutate(County = tolower(word(County , 1  , -2)))

#get majority of votes value, e.g. Republican, Democratic
df_2017$Majority<-colnames(df_2017[1:4])[apply(df_2017[1:4],1,which.max)]

#create percentages for Democratic and Republican votes
df_2017$DemPerc<-df_2017$Democratic/df_2017$Total
df_2017$RepPerc<-df_2017$Republican/df_2017$Total

#get margin % points for majority party
df_2017$MajorityMargin<-
  ifelse(df_2017$Majority=="Republican" ,df_2017$RepPerc-df_2017$DemPerc,
       df_2017$DemPerc-df_2017$RepPerc)

#create categorical values based on margin % points
df_2017$MarginGroup<-
  ifelse(df_2017$MajorityMargin>=.50,"+50%",
  ifelse(df_2017$MajorityMargin>=.25,"25-49%",
  ifelse(df_2017$MajorityMargin>=.10,"10-24%",
  ifelse(df_2017$MajorityMargin>=.05,"5-9%",
  "<5%"))))


#abbreviation for majority party
df_2017$MajorityPartyAbbr<-
  ifelse(df_2017$Majority=="Republican","Rep",
  ifelse(df_2017$Majority=="Democratic","Dem",""
  ))

#categorical variable concatenating majority party and % margin group
df_2017$MajorityPartyMargin<-paste(df_2017$MajorityPartyAbbr,df_2017$MarginGroup)
  

  
#merge map data with election data
merged_data <- left_join(va_county_map, df_2017, by=c("subregion"= "County"))


#city data fro virtiniga
va_cities<-us.cities%>%filter(country.etc=='VA')
va_cities<- va_cities %>% mutate(city = word(name , 1  , -2))

top_va_cities<-top_n(va_cities, 10, pop) 
capitol<-va_cities%>%filter(city=='Richmond')

#get additional fonts for map
loadfonts()
font_import()
font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path

chart_font<-"Gill Sans"
ggplot()+
geom_polygon(data=merged_data, 
              aes(x=long, y=lat, group=group, fill = MajorityPartyMargin), 
                  color="white", size = 0.2)+
geom_label_repel(data=top_va_cities, family=chart_font,
                aes(long, lat, label = city),
                size=2.5)+
geom_point(data=capitol, aes(long,lat), shape=8)+
scale_fill_manual(
  values = 
    c("Dem +50%" = "#053061",
      "Dem 25-49%"="#2166ac",
      "Dem 10-24%"="#4393c3",
      "Dem 5-9%"="#92c5de",
      "Dem <5%"="#d1e5f0",
      "Rep <5%"="#fddbc7",
      "Rep 5-9%"="#f4a582",
      "Rep 10-24%"="#d6604d",
      "Rep 25-49%"="#b2182b",
      "Rep +50%"="#67001f"
      )
  )+
coord_map()+
theme_void()+
theme(
  plot.title=element_text(face='bold', family="Gill Sans Bold"),
  plot.subtitle =element_text(family=chart_font),
  plot.caption = element_text(family=chart_font),
  legend.title = element_text(family="Gill Sans Bold", face='bold', size=8),
  legend.text = element_text(family=chart_font, size=8),
  legend.position="right",
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
)+
labs(title="VIRGINIA GUBERNATORIAL ELECTION 2017",
     subtitle="County Election Results, majority margin based on % points",
     caption="Data from Virgina Department of Elections | Chart @tanya_shapiro",
     fill = "MARGIN % PTS")

ggsave("va_election.jpeg", width = 16, height = 9, units='cm')
