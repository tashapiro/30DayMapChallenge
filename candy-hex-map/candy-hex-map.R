# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(usdata)



spdf <- geojson_read("us_states_hexgrid.geojson.json",  what = "sp")

# Bit of reformating
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# I need to 'fortify' the data to be able to show it with ggplot2 (we need a data frame format)
library(broom)
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

df<-read.csv("candy_data.csv")

df$candy_category_factor<- factor(df$candy_category, 
                                  levels = c("Snickers", "M&M's", "TWIX",
                                             "Skittles","Starburst","Milky Way",
                                             "Skittles & Starburst", "Other"
                                  ))
#merge data
merged_data <- left_join(spdf_fortified, df, by=c("id"= "state"))

font_add("Gill Sans", "/Library/Fonts/Gill Sans.otf")  # Use the actual file path
font_add("Gill Sans Bold", "/Library/Fonts/Gill Sans Bold.otf")  # Use the actual file path

chart_font<-"Gill Sans"



colors<-c(
  '#F2CB05', #yellow
  '#04B2D9', #blue
  '#F2059F', #magenta
  '#F25C05', #orange,
  '#04D960', #green
  '#5D30BF', #purple
  '#16F4D0', #seagrey
  '#F2F2F2' #lightgrey
)




# Now I can plot this shape easily as described before:
ggplot() +
  geom_polygon(data = merged_data,
               aes(fill=candy_category_factor,
                   x = long,
                   y = lat, group = group), color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id), family=chart_font,
            color="#303030",size=3) +
  scale_fill_manual(values=colors)+
  theme_void() +
  theme(
    legend.position="right",
    plot.title=element_text(family="Gill Sans Bold"),
    legend.title=element_text(family="Gill Sans Bold"),
    text =element_text(family=chart_font),
    plot.margin = unit(c(0, 0.5, 0, 0.5), "cm")
  )+
  coord_map()+
  labs(
    title="United States of Mars Candy",
    subtitle="Favorite Mars Wrigley Candy Brandy by State",
    caption="Data from Mars Wrigley & Shipt (2021) | Chart @tanya_shapiro",
    fill="Brand"
  )

ggsave("favorite_mars_candy.jpeg", width = 16, height = 9, units='cm')
