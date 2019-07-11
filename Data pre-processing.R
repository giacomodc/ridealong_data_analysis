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
library(readr)

# set directory here

### PRE-PROCESS DATA FUNCTIONS

# function to extract parking position information
extr_park_info <- function(source_folder_name, data_name){
  park <- read.csv(paste0(source_folder_name,"/",data_name, "_parking.csv"), stringsAsFactors=F, sep=';') # read in data for route
  parking.type <- select(park, Time, Parking.Type)
  # no smart way to mathch this yet read this yet, WIP
  # manually obtained parking types:
  parking.type <- c("CP", "N", "N", "N", "N", "N", "N", "N", "P","CP", "P","CP","N","CP", "N", "CP", "CP", "CP", "N")
  return(parking.type)
}

# function to localize the stops extracted from the photos
extr_loc_stops <- function(source_folder_name, data_name){
  photos <- paste0(source_folder_name, "/", list.files(path = paste0(source_folder_name, "/"), pattern = "*.jpg"))
  picsloc <- select(as.data.frame(read_exif(photos)), CreateDate, GPSAltitude, GPSLatitude, GPSLongitude)
  picsloc <- cbind(picsloc, extr_park_info(source_folder_name, data_name))
  names(picsloc) <- c("datetime", "alt", "lat", "lon", "parking_type")
  write.csv(picsloc, paste0(source_folder_name,"/",data_name, "_stops.csv"), row.names = F)
}

# function to extract GPS traces
extr_gps_trace <- function(source_folder_name, data_name){
  raw <- xmlTreeParse(paste0(source_folder_name,"/", data_name, ".gpx"), useInternalNodes = TRUE)
  rawlist <- xmlToList(xmlRoot(raw))$trk
  ll <- unlist(rawlist[names(rawlist) == "trkseg"], recursive = FALSE)
  gpx <- do.call(rbind.fill, lapply(ll, function(x) as.data.frame(t(unlist(x)), stringsAsFactors=F)))
  names(gpx) <- c("elevation", "datetime", "speed","hacc", "vacc", "steps", "lat", "lon", "course") #check names reflect variables content
  write.csv(gpx, paste0(source_folder_name, "/", data_name, "_route.csv"), row.names = F)
}

# function to extract route segments
extr_route_seg <- function(source_folder_name, data_name){
  wps <- readGPX(paste0(source_folder_name,"/", data_name, ".gpx"), way=T)
  if (source_folder_name == "USPack_groceries"){
    names_seg <- paste0(seq(1,length(wps$tracks[[1]]),1), "_", c(rep(c("drive", "deliver"), 10), rep(c("deliver", "drive"),2)))
  } else if (source_folder_name == "USPack_medical"){
    names_seg <- paste0(seq(1,length(wps$tracks[[1]]),1), "_", c(rep(c("drive", "deliver"))))
  }
  names(wps$tracks[[1]]) <- names_seg
  wps1 <- wps$tracks[[1]]
  for (i in (1:length(wps1))){
    wps1[i] <- lapply(wps1[i], cbind, Segment = c(substring(names(wps1[i]), regexpr("_", names(wps1[i])) + 1)), 
                      segm_id = parse_number(names(wps1[i])))
  }
  seg <- rbind.fill(wps1)
  write.csv(seg, paste0(source_folder_name, "/", data_name, "_segs.csv"), row.names = F)
}

### EXECUTION

# Execute location stop files
extr_loc_stops("USPack_groceries", "USPack_Grocery_placards")
extr_loc_stops("USPack_medical", "USPack_medical")

# Execute GPS trace files
extr_gps_trace("USPack_groceries", "USPack_Grocery_placards")
extr_gps_trace("USPack_medical", "USPack_medical")

# Execute GPS segment files
extr_route_seg("USPack_groceries", "USPack_Grocery_placards")
extr_route_seg("USPack_medical", "USPack_medical")
