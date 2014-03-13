library(ggmap)

usmap<-get_map('United States', zoom=4)

parse.locations<-function(citystr) {
  regex.results<-gregexpr('([A-Z])([a-z]+)(, [A-Z]{2})?', citystr)[[1]]
  locations<-mapply(function(start, length) {
    substr(citystr, start, start+length-1)
  }, regex.results, attr(regex.results, 'match.length'))
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
