#packages to collect data
library(gtrendsR)
library(nflfastR)
library(usmapdata)
#data wrangling + ggplot
library(tidyverse)
library(usdata)
#ggplot extensions
library(ggtext)
library(ggimage)
#font adjustments
library(sysfonts)
library(showtext)

#FONTS
#download font awesome locally first! https://fontawesome.com/download
sysfonts::font_add('fb', 'fonts/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Chivo","Chivo")
showtext_auto()
showtext::showtext_opts(dpi = 300)

#nfl team data - needed for images and colors
df_teams<-nflfastR::teams_colors_logos

#map data
map_state<-usmapdata::us_map(regions = "states")

#list of teams
teams<-c("Philadelphia Eagles","Kansas City Chiefs","Minnesota Vikings","Buffalo Bills")

#get google trends results
results<-gtrends(keyword=teams, geo="US", time="today 1-m")
region_results<-results$interest_by_region

#custom function to get color for team
get_color<-function(team, color=1){
  var = ifelse(color==1,"team_color",paste0("team_color",color))
  df_teams[[var]][df_teams$team_name==team]
}

top_searches<-region_results|>
  mutate(state_abbr = usdata::state2abbr(location))|>
  select(location, state_abbr, keyword, hits)|>
  group_by(location)|>
  arrange(location, -hits)|>
  mutate(total_hits=sum(hits),
         perc = hits/total_hits,
         rank = row_number())|>
  filter(rank==1)|>
  mutate(color = get_color(keyword,1),
         logo = df_teams$team_logo_espn[df_teams$team_name==keyword],
         #bin the alphas by percentage
         alpha = case_when(perc>.8~ 1, perc>.5 ~ .85, TRUE ~ .7))

#combine top searches with map data
map_data<-map_state|>left_join(top_searches, by=c("abbr"="state_abbr"))

#title+ caption html to use with ggtext::element_textboxsimple
title<-"<span style='font-family:Chivo;font-size:25px;padding-botto:10px;'>**Most Googled NFL Teams**</span><br><span style='font-family:Chivo;font-size:12px;color:#6F6F6F;'>Most popular keyword searches by state for top 4 NFL teams in the last 30 days. Top 4 based on Power Rankings (W10). Full team names used as keywords. Data as of Nov. 13th, 2023.</span>"

caption<-paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:Chivo;'> @tanya_shapiro   </span>",
  "<span style='font-family:fb;'>   &#xf09b;</span>",
  "<span style='font-family:Chivo;'> tashapiro </span>")

#create custom legend df
df_legend<-top_searches|>
  ungroup()|>
  distinct(keyword, color, logo)|>
  mutate(text_len = nchar(keyword), 
         text_space = text_len * unit,
         ymid = ymax+200000,
         lag = lag(text_space, default=xmin+200000),
         xmid = cumsum(lag))


#PLOT
ggplot()+
  geom_polygon(data=map_data,
               mapping=aes(x=x,y=y,group=group, fill=color, alpha=alpha),
               color="white", size=0.2)+
  #custom legend
  geom_point(data=df_legend, mapping=aes(x=xmid, y=ymid, color=color), alpha=0.9, size=11)+
  geom_image(data=df_legend, mapping=aes(x=xmid, y=ymid, image=logo), size=0.04)+
  geom_text(data=df_legend, mapping=aes(x=xmid+exp+80000, y=ymid, label=keyword), size=2.75, family="Chivo", hjust=0)+
  #annotation  for opacity
  geom_segment(mapping=aes(y=-1969029, yend =-1369029, x=1154654, xend=1254654 ), 
               size=0.2, arrow=arrow(length=unit(0.04,"inches")))+
  geom_textbox(mapping=aes(y = -1969029, x=1254654, label="Higher color opacity indicates higher % value for most popular search"), 
               box.color=NA, size=2.3, width=unit(1.25,'inches'), family="Chivo", color="grey20")+
  #set all scales to identity
  scale_fill_identity()+
  scale_color_identity()+
  scale_alpha_identity()+
  #title + plot labels
  labs(title=title, caption=caption)+
  #theme
  theme_void()+
  theme(legend.position="top", 
        plot.title=element_textbox_simple(halign=0.5, margin=margin(t=20, b=10), width=unit(5.5,"in")),
        plot.caption = element_textbox_simple(halign=1, color="#6F6F6F", margin=margin(r=15, b=10)))

ggsave("nfl-map.jpeg", height=6.9, width=6.9, units="in")
