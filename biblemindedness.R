library(ggmap)
library(stringr)

get_usmap<-function() {
  usmap<-get_map('United States', zoom=4)
}

parse.locations<-function(citystr, sep='/') {
  #regex.results<-gregexpr('([A-Z])([a-z]+)(, [A-Z]{2})?', citystr)[[1]]
  #locations<-mapply(function(start, length) {
  #  substr(citystr, start, start+length-1)
  #}, regex.results, attr(regex.results, 'match.length'))
  locations<-as.vector(mapply(str_trim, strsplit(citystr, sep)))
  statesuffix.results<-gregexpr(', [A-Z]{2}', locations)
  for (i in length(locations):1) {
    if (statesuffix.results[[i]][[1]]>0) {
      start<-statesuffix.results[[i]][[1]]+2
      length<-attr(statesuffix.results[[i]], 'match.length')[[1]]
      currentstate<-substr(locations[[i]], start, start+length-1)
    } else {
      locations[[i]]<-paste(locations[[i]], ', ', currentstate, sep='')
    }
  }
  locations
}

mean_geocode<-function(citystr) {
  locations<-parse.locations(citystr)
  geolocations<-geocode(locations)
  data.frame(lon=mean(geolocations$lon), lat=mean(geolocations$lat))
}

get_clusters<-function(biblemindednesses) {
  kmeans(biblemindednesses, c(10, 20, 30, 38, 45), iter.max=1000)
}

plot_cluster_biblemindedness<-function(citydata) {
  clustering<-get_clusters(citydata$Biblemindedness)
  color.codes<-c('#000000', '#000033', '#000066', '#000099', '#0000CC', '#0000FF')
  mapplot<-get_usmap()
  for (i in 1:nrow(citydata)) {
    cluster.type<-clustering[[1]][[i]]
    mapplot<-mapplot+geom_point(aes(colour=color.codes[[cluster.type]]),
                                data=mean_geocode(citydata$City[[i]]))
  }
  mapplot
}
