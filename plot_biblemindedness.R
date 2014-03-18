library(xlsx)
source('biblemindedness.R')

citydata<-read.xlsx('biblemindedness_data.xlsx', sheetName='Sheet1', header=TRUE)

clustering<-get_clusters(citydata$Biblemindedness)
color.codes<-c('#000000', '#000044', '#000088', '#0000CC', '#0000FF')
coordinates.set<-c()
for (i in 1:length(color.codes)) {
  coordinates<-mean_geocodes(citydata[ clustering[[1]]==i, ]$City)
  coordinates.set<-append(coordinates.set, coordinates)
}

bible_map<-ggmap(get_usmap())
bible_map<-bible_map+geom_point(aes(colour=1, color='Bible-Mindedness'), 
                                data=data.frame(lon=coordinates.set[1]$lon, lat=coordinates.set[2]$lat),
                                show_guide=TRUE)
bible_map<-bible_map+geom_point(aes(colour=2), 
                                data=data.frame(lon=coordinates.set[3]$lon, lat=coordinates.set[4]$lat))
bible_map<-bible_map+geom_point(aes(colour=3), 
                                data=data.frame(lon=coordinates.set[5]$lon, lat=coordinates.set[6]$lat))
bible_map<-bible_map+geom_point(aes(colour=4), 
                                data=data.frame(lon=coordinates.set[7]$lon, lat=coordinates.set[8]$lat))
bible_map<-bible_map+geom_point(aes(colour=5), 
                                data=data.frame(lon=coordinates.set[9]$lon, lat=coordinates.set[10]$lat))
bible_map+labs(colour='Bible-mindedness')
