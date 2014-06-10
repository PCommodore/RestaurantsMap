## @knitr loadLibraries
require(RJSONIO); require(rCharts); require(RColorBrewer); require(httr); require(jsonlite);require(googleVis)
options(stringsAsFactors = F)

#getData
restaurantslist = 'lesamisrestaurant,restaurantmajestic,bibigo.singapore,kisekirestaurant,restaurantcocotte,BacchanaliaSG'

getData <- function(rlist = restaurantslist){
  require(httr)
  url = sprintf('https://graph.facebook.com/?ids=%s', rlist)
  restaurants = fromJSON(url)
  lapply(restaurants, function(rest){within(rest, { 
    fillColor = cut(
      were_here_count, 
      breaks = c(0, 100, 500, 1000, 1100, 20000), 
      labels = brewer.pal(5, 'RdYlGn'),
      include.lowest = TRUE
    ) 
    popup = iconv(whisker::whisker.render(
      '<b>{{name}}</b><br>
      <b>Likes: </b> {{likes}} <br>
      <b>Talking About:</b> {{talking_about_count}}
      <p>Were Here: {{were_here_count}}</p>'
    ), from = 'latin1', to = 'UTF-8')
    latitude = as.numeric(location$latitude)
    longitude = as.numeric(location$longitude)
    #location$latitude <- location$longitude <- NULL
    })
  })
  
}

#Visualization

plotMap <- function(dataset = restaurantslist, width = 880, height = 550){
  data_ <- getData(dataset); 
  L1 <- Leaflet$new()
  #L1$tileLayer(provider = 'Stamen.TonerLite')
  L1$set(width = width, height = height)
  L1$setView(c(1.373607, 103.804476), 11)
  L1$geoJson(toGeoJSON(data_), 
             onEachFeature = '#! function(feature, layer){
      layer.bindPopup(feature.properties.popup)
    } !#',
             pointToLayer =  "#! function(feature, latlng){
      return L.circleMarker(latlng, {
        radius: 4,
        fillColor: feature.properties.fillColor || 'red',    
        color: '#000',
        weight: 1,
        fillOpacity: 0.8
      })
    } !#")
  L1$enablePopover(TRUE)
  L1$fullScreen(TRUE)
  return(L1)
}

#


restaurantdf <- function(){
  
  jsonlist = getData();
  
  numofvariables = 4
  data = matrix(0,length(names(jsonlist)), numofvariables)
  data = data.frame(data)
  names(data) = c("name","likes","talking","here")

#Convert restaurants to data frame for key variables of interest

  for (i in 1:length(names(jsonlist))) {
    data[i,1] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$name
    data[i,2] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$likes
    data[i,3] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$talking_about_count
    data[i,4] = eval(parse(text=sprintf('jsonlist$%s',names(jsonlist)[i])))$were_here_count
  }

return(data)
}


