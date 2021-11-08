library(countrycode)
library(maps)
library(dplyr)
library(ggplot2)
library(colorfindr)
library(RCurl)
library(png)

#country list
df<-codelist
df=df[,c("country.name.en","iso.name.en","cctld")]
df$country<-tolower(df$iso.name.en)
df$id<-substr(df$cctld,2,3)
df<-df %>% filter(!is.na(id))
#prepare url for countries to get flag links from flagpedia
url_prefix<-'https://flagpedia.net/data/flags/w1160/'
url_suffix<-'.png'
df$web_url<-paste(url_prefix,df$id,url_suffix,sep="")
#uk issues a 404 warning, gb is code
df[df$id=="uk","web_url"]<-'https://flagpedia.net/data/flags/w1160/gb.png'
#download list of flags

#can uncommenf if prefer to download images locally
#for (row in 1:nrow(df)) {
#  link <- as.character(df[row, "web_url"])
#  name <- as.character(df[row, "id"])
#  file_name=paste("flags/",name,".png",sep="")
#  download.file(link, destfile = file_name, mode = 'wb')
#}

#get color hex
for (row in 1:nrow(df)) {
    x <- df[row, "web_url"]
    print(x)
    df[row,"col_1"]<-get_colors(as.character(x),top_n=3)[1][[1]][1]
    df[row,"col_2"]<-get_colors(as.character(x),top_n=3)[1][[1]][2]
    df[row,"col_3"]<-get_colors(as.character(x),top_n=3)[1][[1]][3]
    df[row,"col_shar_1"]<-get_colors(as.character(x),top_n=3)[3][[1]][1]
    df[row,"col_shar_2"]<-get_colors(as.character(x),top_n=3)[3][[1]][2]
    df[row,"col_shar_3"]<-get_colors(as.character(x),top_n=3)[3][[1]][3]
}

#write.csv(df,"flag_data.csv")
