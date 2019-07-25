# 
# Created Jul 9, 2019
# 
# @ authors: Klaas Fiete Krutein
# @ authors: Giacomo Dalla Chiara
# 
# Urban Freight Lab @ University of Washington, Supply Chain Transportation and Logistics Center

rm(list=ls())

library(XML)
library(plotKML)
library(plyr)
library(leaflet)
library(sp)
library(exifr)
library(dplyr)
library(raster)
library(sp)
library(ggplot2)
library(gridExtra)
library(mapview)

# set directory here
setwd("~/Documents/University/University of Washington/RA positions/SCTL/DOE/Ridealong Data analysis")

### LOAD and TRANSFORM DATA

# transform stops data
transf_stops_data <- function(source_folder_name, data_name){
  stops <- read.csv(paste0(source_folder_name,"/",data_name, "_stops.csv"), stringsAsFactors=F) # read in data for stops
  stops$datetime <- as.POSIXct(sub(":","-",sub(":","-", stops$datetime)), format="%Y-%m-%d %H:%M:%S") # transformations
  # add parking information here and recode 
  stops$parking_type[stops$parking_type == "P"] <- "paid parking"
  stops$parking_type[stops$parking_type == "CP"] <- "customer parking"
  stops$parking_type[stops$parking_type == "N"] <- "no parking"
  stops$parking_type[stops$parking_type == "CVLZ"] <- "commercial vehicle loading zone"
  stops$parking_type[stops$parking_type == "3M"] <- "passenger loading zone"
  stops$parking_type[stops$parking_type == "U"] <- "unrestricted"
  stops$parking_type[stops$parking_type == "B"] <- "public transportation stop"
  stops$parking_type[stops$parking_type == "TL"] <- "travel lane"
  stops$parking_type[stops$parking_type == "A"] <- "alley"
  return(stops)
}

# transform route data
transf_route_data <- function(source_folder_name, data_name){
  gpx <- read.csv(paste0(source_folder_name,"/",data_name, "_route.csv"), stringsAsFactors=F) # read in data for route
  gpx$datetime <- gsub("T", " ", gpx$datetime)
  gpx$datetime <- gsub("Z", "", gpx$datetime)
  gpx$datetime <- as.POSIXct(gpx$datetime, format="%Y-%m-%d %H:%M:%S")
  return(gpx)
}

# transform segments data
transf_seg_data <- function(source_folder_name, data_name){
  segm <- read.csv(paste0(source_folder_name,"/",data_name, "_segs.csv"), stringsAsFactors=F) # read in data for segments
  segm$time <- gsub("T", " ", segm$time)
  segm$time <- gsub("Z", "", segm$time)
  segm$time <- as.POSIXct(segm$time, format="%Y-%m-%d %H:%M:%S")
  segm$Segment <- as.factor(segm$Segment)
  segm$segm_id <- as.numeric(segm$segm_id)
  return(segm)
}

### DATA VISUALIZATION

# stop locations and route visualization including speed profiles
visualize_routes <- function(source_folder_name, route_data, stops_data){
  coll <- heat.colors(max(route_data$speed),alpha=NULL) # add heat colors
  map <- leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% 
    setView(lng = -122.333279, lat = 47.601503, zoom = 10) %>%
    addCircleMarkers(~lon, ~lat,  data=route_data, stroke = FALSE, radius = 3, fillOpacity = 0.5)  %>%
    addMarkers(~lon, ~lat, popup = ~parking_type, data=stops_data)
  # add a dynamically changing line indicating the speed
  for (i in 1:(nrow(route_data)-1)) {
    map <- map %>% addPolylines(lat = ~lat, lng = ~lon, data=route_data[i:(i+1),], color = ~coll[round(route_data[i,3],0)])
  }
  #mapshot(map, file = paste0(getwd(), "/", source_folder_name, "/routes_map.html"))
  #mapshot(map, file = paste0(getwd(), "/", source_folder_name, "/routes_map.png"))
  return(map)
}

# stop locations and delivery segment visualizations incl. parking mode
visualize_segments <- function(source_folder_name, seg_data, stops_data){
  pal <- colorFactor(palette = 'Dark2', domain = seg_data$Segment)
  map <- leaflet() %>% addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap = TRUE)) %>% 
    setView(lng = -122.333279, lat = 47.601503, zoom = 10) %>%
    addCircleMarkers(~lon, ~lat,  data=seg_data, stroke = FALSE, radius = 3, fillOpacity = 0.5)  %>%
    addMarkers(~lon, ~lat, popup = ~c(parking_type, as.character(datetime)), data=stops_data)
  # add a dynamically changing line indicating the speed
  for (i in 1:(nrow(seg_data)-1)) {
    map <- map %>% addPolylines(lat = ~lat, lng = ~lon, data=seg_data[i:(i+1),], color = ~pal(Segment))
  }
  #mapshot(map, file = paste0(getwd(), "/", source_folder_name, "/segment_map.html"))
  #mapshot(map, file = paste0(getwd(), "/", source_folder_name, "/segment_map.png"))
  return(map)
}

# a shift vector function
shift.vec <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else {
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }

# visualize and calculate the time profile and distance covered of each process
calc_segments <- function(seg_data){
  # add new columns to seg_data to add next point in the journey for distance calculations
      seg_data$lat.p1 <- shift.vec(seg_data$lat, -1)
      seg_data$lon.p1 <- shift.vec(seg_data$lon, -1)
      seg_data$dist.to.prev <- apply(seg_data, 1, FUN = function (row) {
        pointDistance(c(as.numeric(row["lon.p1"]), as.numeric(row["lat.p1"])),
                      c(as.numeric(row["lon"]), as.numeric(row["lat"])),lonlat = T) # there is something wrong here
      })
  # add new columns to generate the same for speed profiles
      seg_data$time.p1 <- shift.vec(seg_data$time, -1)
      # Calculate the number of seconds between two positions.
      seg_data$time.diff.to.prev <- as.numeric(difftime(seg_data$time.p1, seg_data$time))
      # calculate speed profiles
      seg_data$speed.m.per.sec <- seg_data$dist.to.prev / seg_data$time.diff.to.prev
      seg_data$speed.km.per.h <- seg_data$speed.m.per.sec * 3.6
      seg_data$speed.km.per.h <- ifelse(is.na(seg_data$speed.km.per.h), 0, seg_data$speed.km.per.h)
      seg_data$speed.m.per.h <- seg_data$speed.km.per.h * 0.621371
      seg_data$speed.m.per.h <- ifelse(is.na(seg_data$speed.m.per.h), 0, seg_data$speed.m.per.h)
      seg_data$lowess.speed <- lowess(seg_data$speed.km.per.h, f = 0.1)$y
      seg_data$lowess.ele <- lowess(seg_data$ele, f = 0.2)$y
      seg_data$time.ref <- as.POSIXct(seg_data$time)
      seg_data$index <- seq(1,nrow(seg_data))
  return(seg_data)    
}
      
# calculate speed and distance profile
calc_profile_sd <- function(seg_data){
  seg_data <- calc_segments(seg_data)
  hold <- as.data.frame(matrix(NA, nrow = max(seg_data$segm_id), ncol = 3))
  colnames(hold) <- c("time_span", "distance_covered", "mode")
  for (i in (1:max(seg_data$segm_id))){
    hold[i, 1] <- difftime(max(seg_data$time[seg_data$segm_id == i]), min(seg_data$time[seg_data$segm_id == i]), units = "mins") # time in hours
    hold[i, 2] <- 1/1000*sum(abs(seg_data$dist.to.prev[seg_data$segm_id == i]), na.rm=TRUE)
    hold[i, 3] <- as.character(unique(seg_data$Segment[seg_data$segm_id == i]))
  }
  return(hold)
}
  
### EXECUTION

# Execute transformations of stops
stops_groc <- transf_stops_data("USPack_groceries", "USPack_Grocery_placards") 
stops_med <- transf_stops_data("USPack_medical", "USPack_medical")
stops_pick <- transf_stops_data("USPS_pick_large", "USPS_pick_large") 
stops_del_pick <- transf_stops_data("USPS_del_pick", "USPS_del_pick")
stops_peps <- transf_stops_data("PepsiCo", "PepsiCo")

# Execute transformations of routes
route_groc <- transf_route_data("USPack_groceries", "USPack_Grocery_placards")
route_med <- transf_route_data("USPack_medical", "USPack_medical")
route_pick <- transf_route_data("USPS_pick_large", "USPS_pick_large")
route_del_pick <- transf_route_data("USPS_del_pick", "USPS_del_pick")
route_peps <- transf_route_data("PepsiCo", "PepsiCo")

# Execute transformation of segments
segm_groc <- transf_seg_data("USPack_groceries", "USPack_Grocery_placards")
segm_med <- transf_seg_data("USPack_medical", "USPack_medical")
segm_pick <- transf_seg_data("USPS_pick_large", "USPS_pick_large")
segm_del_pick <- transf_seg_data("USPS_del_pick", "USPS_del_pick")
segm_peps <- transf_seg_data("PepsiCo", "PepsiCo")

### MAPS

# Execute visualizations for speed on route
map_route_groc <- visualize_routes("USPack_groceries", route_groc, stops_groc)
map_route_med <- visualize_routes("USPack_medical", route_med, stops_med)
map_route_pick <- visualize_routes("USPS_pick_large", route_pick, stops_pick)
map_route_del_pick <- visualize_routes("USPS_del_pick", route_del_pick, stops_del_pick)
map_route_peps <- visualize_routes("PepsiCo", route_peps, stops_peps)

# Execute visualizations for segments on route
map_segm_groc <- visualize_segments("USPack_groceries", segm_groc, stops_groc)
map_segm_med <- visualize_segments("USPack_medical", segm_med, stops_med)
map_segm_pick <- visualize_segments("USPS_pick_large", route_pick, stops_pick)
map_segm_del_pick <- visualize_segments("USPS_del_pick", segm_del_pick, stops_del_pick)
map_segm_peps <- visualize_segments("PepsiCo", segm_peps, stops_peps)

### PLOTTING

# function to get dataset for summary data
get_data <- function(seg_data){
  return(calc_profile_sd(seg_data))
}
# function to get datasets for delivery
get_del <- function(seg_data){
  summary.data <- calc_profile_sd(seg_data)
  summary.deliveries <- summary.data[which(summary.data$mode == "deliver"),]
  summary.deliveries$index <- seq(1, nrow(summary.deliveries))
  return(summary.deliveries)
}

# function to get datasets for driving
get_dri <- function(seg_data){
  summary.data <- calc_profile_sd(seg_data)
  summary.driving <- summary.data[which (summary.data$mode == "drive"),]
  summary.driving$index <- seq(1, nrow(summary.driving))
  return(summary.driving)
}

# MEDICAL ROUTES
summary.data <- get_data(segm_med)
summary.deliveries <- get_del(segm_med)
summary.driving <- get_dri(segm_med)

# plot comparisons
plot_comp_time <- ggplot(aes(x= mode, y = time_span, fill = mode), data = summary.data) + geom_boxplot() + theme_light() + ylab("time in minutes") + xlab("process step") + ggtitle("Time span distributions")
plot_comp_distance <- ggplot(aes(x= mode, y = distance_covered, fill = mode), data = summary.data) + geom_boxplot() + theme_light() + ylab("distance in km") + xlab("process step") + ggtitle("Distance distributions")
# plot deliveries
plot_deliver_time <- (ggplot(aes(x = index, y = time_span, fill = distance_covered), data = summary.deliveries)
                      + geom_bar(stat= "identity") + ylab("delivery time in mins") 
                      + theme_light() + ggtitle("Delivery time spans")) + scale_x_continuous(name = "Stop index", seq(1, nrow(summary.deliveries),1))
plot_deliver_dist <- (ggplot(aes(x = index, y = distance_covered, fill = time_span), data = summary.deliveries)
                      + geom_bar(stat= "identity") + ylab("covered distance in km") 
                      + theme_light() + ggtitle("Walking distances")) + scale_x_continuous(name = "Stop index", seq(1, nrow(summary.deliveries),1))

# plot routes (driving)
plot_drive_time <- (ggplot(aes(x = index, y = time_span, fill = distance_covered), data = summary.driving)
                    + geom_bar(stat= "identity") + ylab("drive time in mins")
                    + theme_light() + ggtitle("Delivery time spans")) + scale_x_continuous(name = "Drive segment index", seq(1, nrow(summary.driving),1))
plot_drive_dist <- (ggplot(aes(x = index, y = distance_covered, fill = time_span), data = summary.driving)
                    + geom_bar(stat= "identity") + ylab("covered distance in km") + scale_x_continuous(name = "Drive segment index", seq(1, nrow(summary.driving),1))
                    + theme_light() + ggtitle("Driving distances")) 

plot_meds <- grid.arrange(plot_comp_time, plot_deliver_time, plot_drive_time,
                          plot_comp_distance, plot_deliver_dist, plot_drive_dist,
                          nrow=2, ncol = 3, top = "Medical route")
#plot_meds
ggsave("med_routes_overview.png", plot = plot_meds, width = 14, height = 8)
#plot_speed_overall <- ggplot(data = segm_med) + geom_line(aes(x=index, y = speed.m.per.h), colour="black") + geom_line(aes(x = index, y = lowess.speed), colour = "red")

# GROCERY ROUTES
summary.data <- get_data(segm_groc)
summary.deliveries <- get_del(segm_groc)
summary.driving <- get_dri(segm_groc)

# plot comparisons
plot_comp_time <- ggplot(aes(x= mode, y = time_span, fill = mode), data = summary.data) + geom_boxplot() + theme_light() + ylab("time in minutes") + xlab("process step") + ggtitle("Time span distributions")
plot_comp_distance <- ggplot(aes(x= mode, y = distance_covered, fill = mode), data = summary.data) + geom_boxplot() + theme_light() + ylab("distance in km") + xlab("process step") + ggtitle("Distance distributions")
# plot deliveries
plot_deliver_time <- (ggplot(aes(x = index, y = time_span, fill = distance_covered), data = summary.deliveries)
                      + geom_bar(stat= "identity") + ylab("delivery time in mins") 
                      + theme_light() + ggtitle("Delivery time spans")) + scale_x_continuous(name = "Stop index", seq(1, nrow(summary.deliveries),1))
plot_deliver_dist <- (ggplot(aes(x = index, y = distance_covered, fill = time_span), data = summary.deliveries)
                      + geom_bar(stat= "identity") + ylab("covered distance in km") 
                      + theme_light() + ggtitle("Walking distances")) + scale_x_continuous(name = "Stop index", seq(1, nrow(summary.deliveries),1))

# plot routes (driving)
plot_drive_time <- (ggplot(aes(x = index, y = time_span, fill = distance_covered), data = summary.driving)
                    + geom_bar(stat= "identity") + ylab("drive time in mins")
                    + theme_light() + ggtitle("Delivery time spans")) + scale_x_continuous(name = "Drive segment index", seq(1, nrow(summary.driving),1))
plot_drive_dist <- (ggplot(aes(x = index, y = distance_covered, fill = time_span), data = summary.driving)
                    + geom_bar(stat= "identity") + ylab("covered distance in km") + scale_x_continuous(name = "Drive segment index", seq(1, nrow(summary.driving),1))
                    + theme_light() + ggtitle("Driving distances")) 

plot_meds <- grid.arrange(plot_comp_time, plot_deliver_time, plot_drive_time,
                          plot_comp_distance, plot_deliver_dist, plot_drive_dist,
                          nrow=2, ncol = 3, top = "Grocery route")
#plot_groc
ggsave("groc_routes_overview.png", plot = plot_groc, width = 14, height = 8)
#plot_speed_overall <- ggplot(data = segm_groc) + geom_line(aes(x=index, y = speed.m.per.h), colour="black") + geom_line(aes(x = index, y = lowess.speed), colour = "red")


