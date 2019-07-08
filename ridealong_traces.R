rm(list=ls())

library(XML)
library(plotKML)
library(plyr)
library(leaflet)
library(sp)

# set directory here

### LOAD DATA

# stop locations (lat/lon of stop locations - NEED TO WRITE CODE to extract lat/lon from parking pics)
stop <- read.csv("stops_locations.csv", stringsAsFactors = F)

# gps traces
raw <- xmlTreeParse("nighttime_drive.gpx", useInternalNodes = TRUE)
rootNode <- xmlRoot(raw)
rawlist <- xmlToList(rootNode)$trk
ll <- unlist(rawlist[names(rawlist) == "trkseg"], recursive = FALSE)
gpx <- do.call(rbind.fill, lapply(ll, function(x) as.data.frame(t(unlist(x)), stringsAsFactors=F)))
names(gpx) <- c("elevation", "time", "speed", "course","hacc", "vacc", "steps", "lat", "lon") #check names reflect variables content


### CLEAN DATA
stop$lat <- gsub(" ", "", stop$lat)
stop$lon <- gsub(" ", "", stop$lon)
stop$lat <- as.numeric(char2dms(stop$lat,chd="d",chm="m",chs="s"))
stop$lon <- as.numeric(char2dms(stop$lon,chd="d",chm="m",chs="s"))

gpx$lon <- as.numeric(gpx$lon)
gpx$lat <- as.numeric(gpx$lat)
gpx$time <- gsub("T", " ", gpx$time)
gpx$time <- gsub("Z", "", gpx$time)
gpx$time <- as.POSIXct(gpx$time, format="%Y-%m-%d %H:%M:%S")
gpx$n <- 1:nrow(gpx)


### DATA ANALYSIS
# stop locations and routes
mm <- leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE) ) %>% 
  #setView(lng = -122.333279, lat = 47.601503, zoom = 15) %>%
  addCircleMarkers(~lon, ~lat,  data=gpx, stroke = FALSE, radius = 3, fillOpacity = 0.5)  #%>%
  #addMarkers(~lon, ~lat, popup = ~as.character(seq), data=stop) 

coll <- heat.colors(max(gpx$n)-1,alpha=NULL)

for (i in 1:(nrow(gpx)-1)) {
  mm <- mm %>% addPolylines(lat = ~lat, lng = ~lon, data=gpx[i:(i+1),], color = ~coll[i])
}

# analysis wishlist:
#- speed profile (walking & driving)
#- altitude analysis (walking up buildings)
#- tot distance (walking & driving)








