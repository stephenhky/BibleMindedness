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

mean_geocodes<-function(metros) {
  lons<-c()
  lats<-c()
  for (metro in metros) {
    coordinates<-mean_geocode(metro)
    lons<-append(lons, coordinates$lon)
    lats<-append(lats, coordinates$lat)
  }
  data.frame(lon=lons, lat=lats)
}

get_clusters<-function(biblemindednesses) {
  kmeans(biblemindednesses, c(10, 20, 30, 38, 45), iter.max=1000)
}
