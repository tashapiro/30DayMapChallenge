library(tigris)

#import data with all external causes of death (CDC Wonder Data)
county_df<-read.delim("county_heart.txt", header = TRUE, sep = "\t")
state_df<-read.delim("state_heart.txt", header = TRUE, sep = "\t")
#filter and clean data
state_df<-state_df%>%filter(Notes=="")
state_df$FIPS_Code<-state_df$State.Code
state_df$rate<-round((state_df$Deaths/state_df$Population)*100000,2)
county_df<-county_df%>%filter(Notes=="")
county_df$Population<-as.integer(county_df$Population)
county_df$Deaths<-as.integer(county_df$Deaths)
county_df$rate<-round((county_df$Deaths/county_df$Population)*100000,2)
county_df$FIPS_Code<-ifelse(nchar(county_df$County.Code)==5,county_df$County.Code,paste(0,county_df$County.Code,sep=""))

#json data
data("usgeojson") 
data("uscountygeojson")

#drilldown code taken from github issue forum: https://github.com/jbkunst/highcharter/issues/507
cn_st_index <- unlist(lapply(uscountygeojson$features, function (x){
  if(is.null(x$id))
    NA
  else
    str_to_lower(x$properties$fips) %>% str_sub(1, 2) %>% as.numeric()
}))


build_series <- function(stateCode) {
  #subset uscountygeojson
  uscountygeojson.subset <- uscountygeojson
  uscountygeojson.subset$features <- uscountygeojson$features[which(cn_st_index == stateCode)]
  #subset county data
  usa.cn <- filter(county_df, State == State) %>% 
    transmute(code = FIPS_Code, value = rate, cnlabel = County, stlabel = State) %>%
    list_parse()
  #build series
  list(
    id = stateCode,
    mapData = uscountygeojson.subset,
    data = usa.cn,
    joinBy = c('fips',"code"),
    dataLabels = list(enabled = TRUE, format = '{point.name}'),
    tooltip = list(
      useHTML = TRUE,
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.cnlabel}</b><br>",
                           "<b style=\"color:#1874CD\"> Death Rate:</b> {point.value:.2f}<br>"),
      footerFormat = "</p>")
  )
}

usa.ts<- state_df %>% 
  transmute(code = FIPS_Code, value= rate,stlabel = State, drilldown = FIPS_Code)
usa.st<- list_parse(usa.ts)

#create drilldown series
series.list <- lapply(usa.ts$drilldown, build_series)

#format color palette
n <- 5
stops <- data.frame(
  q = 0:n/n,
  c = c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725"),
  stringsAsFactors = FALSE
)
stops <- list_parse2(stops)

#build chart
map<-highchart(type = 'map') %>%
  hc_add_series(
    mapData = usgeojson, data = usa.st, joinBy = c("statefips", "code"),
    borderWidth = 0.8, dataLabels = list(enabled = TRUE, format = '{point.properties.postalcode}'),
    tooltip = list( 
      useHTML = TRUE,
      style = list(fontFamily= "Gill Sans"),
      headerFormat = "<p>",
      pointFormat = paste0("<b>{point.stlabel}</b><br>",
                           "<b style=\"color:#1874CD\"> Death Rate:</b> {point.value:.2f}<br>"),
      footerFormat = "</p>"
    )) %>%
  hc_plotOptions(map = list(states = list(hover = list(color = '#FFFFFF')))) %>%
  hc_colorAxis(stops=stops,max=100) %>% 
  hc_title(text = "Cause of Death: Coronary Disease (2019)") %>%
  hc_subtitle(text="Cause of Death related to Coronary Disease for people under 65.")%>%
  hc_caption(text="Data from CDC Wonder | Chart @tanya_shapiro")%>%
  hc_legend(enabled = TRUE, title=list(text="Rate Per 100K"))%>%
  hc_exporting(enabled = TRUE)%>%
  hc_drilldown(
    series = series.list,
    activeDataLabelStyle = list(
      color =  '#FFFFFF',
      textDecoration = 'none'
    )
  )

library(htmlwidgets)

saveWidget(map, file="highchart.html")
