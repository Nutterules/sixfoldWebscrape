library(httr)
library(jsonlite)
library(tidyr)
library(dplyr)
library(git2r)

portUrl <- "https://covid-19.sixfold.com/persistent/ports.json"
crossingUrl <- "https://covid-19.sixfold.com/persistent/data.json"
seaCrossingUrl <- "https://live.sixfold.com/persistent/port_crossings.json"
# setwd("C:/Users/hugha/OneDrive - York St John University/Apprenticeship/RTIs/Sixfold/sixfold_webscrape")
gap <- 25 # Wait for 'gap' minutes before next data request

if (!file.exists("delayData.RDS")) {
  data <- NULL
  saveRDS(data, file = "delayData.RDS") 
}
repeat {
  borderResponse <- GET(crossingUrl)
  borderRawData <- fromJSON(content(borderResponse, as = "text", encoding = "UTF-8"))
  
  
  euroTunnel<-borderRawData$eurotunnel_geojson$features
  euroTunnel <- cbind(euroTunnel$properties, euroTunnel$geometry)
  locs <- euroTunnel$coordinates %>% unlist()
  euroTunnel$latitude <- locs[c(TRUE, FALSE)]
  euroTunnel$longitude <- locs[c(FALSE, TRUE)]
  euroTunnel <- euroTunnel %>% select(Label=name, Median=median, Status=status, 
                                      Latitude=latitude, Longitude=longitude)
  euroTunnel$timeStamp <- borderRawData$updated_at
  euroTunnel$Direction[which(euroTunnel$Label %in% "Eurotunnel Calais")] <- "France->United Kingdom"
  euroTunnel$Direction[which(euroTunnel$Label %in% "Eurotunnel Folkestone")] <- "United Kingdom->France"
  
  crossings <- borderRawData$geojson$features$properties$crossings
  coords <- borderRawData$geojson$features$geometry
  locs <- coords$coordinates %>% unlist()
  coords$latitude <- locs[c(TRUE, FALSE)]
  coords$longitude <- locs[c(FALSE, TRUE)]
  coords$coordinates <- NULL
  coords$type <- NULL
  crossings <- Map(cbind, crossings, longitude = coords$longitude)
  crossings <- Map(cbind, crossings, latitude = coords$latitude)
  crossings <- crossings %>% bind_rows()
  crossings <- crossings %>% unite(Direction, from, to, sep="->") %>% 
    select(Label=name, Median=median, Status=status, 
           Latitude=latitude, Longitude=longitude, Direction = Direction)
  crossings$timeStamp <- borderRawData$updated_at
  
  
  portResponse <- GET(portUrl)
  portRawData <- fromJSON(content(portResponse, as = "text", encoding = "UTF-8"))
  portData <- cbind(portRawData$port_geojson$features$properties, portRawData$port_geojson$features$geometry)

  
  locs <- portData$coordinates %>% unlist()
  portData$latitude <- locs[c(TRUE, FALSE)]
  portData$longitude <- locs[c(FALSE, TRUE)]
  portData <- portData %>% select(Label=name, Median=median, Status=status, 
                                  Latitude=latitude, Longitude=longitude)
  portData$timeStamp <- portRawData$updated_at
  portData$Direction <- "Outbound"
  
  portCrossingResponse <- GET(seaCrossingUrl)
  portCRawData <- fromJSON(content(portCrossingResponse, as = "text", encoding = "UTF-8"))
  portCData <- cbind(portCRawData$features$properties, portCRawData$features$geometry)
  portCData <- portCData[sapply(portCData$crossings, function(x) dim(x)[1]) > 0, ]
  
  portCrossingResponse <- GET(seaCrossingUrl)
  portCRawData <- fromJSON(content(portCrossingResponse, as = "text", encoding = "UTF-8"))
  portCData <- cbind(portCRawData$geojson$features$properties, portCRawData$geojson$features$geometry)
  portCData <- portCData[sapply(portCData$crossings, function(x) dim(x)[1]) > 0, ]
  
  locs <- portCData$coordinates %>% unlist()
  portCData$latitude <- locs[c(TRUE, FALSE)]
  portCData$longitude <- locs[c(FALSE, TRUE)]
  portCData$Direction <- "Outbound"
  delayData <- bind_rows(portCData$crossings, .id = "id")
  delayData$Latitude <- portCData$latitude[as.integer(delayData$id)]
  delayData$Longitude <- portCData$longitude[as.integer(delayData$id)]
  delayData$timeStamp <- portCRawData$updated_at 
  delayData$name <- paste(portCData$name[as.integer(delayData$id)], delayData$name, sep = " - ")
  delayData <- delayData %>% select(Label=name, Median=time, Status=status, 
                                    Latitude, Longitude, timeStamp)
  delayData$Direction <- "Outbound"
  
  
  collatedData <- rbind(euroTunnel, crossings, portData, delayData)
  # ggplot(collatedData, aes(x=Latitude, y=Longitude, colour=Median)) + 
  #   geom_point() + theme_bw()
  
  data <- readRDS("delayData.RDS")
  data <- unique(rbind(data, collatedData))
  saveRDS(data, file = "delayData.RDS") 
  saveRDS(data, file = paste0("archive/delayData_", 
                              gsub("^.{2}", "", gsub(".{2}$", "", gsub(":", "", gsub("BST", "", gsub(" ", "", gsub("-","", Sys.time()))))))
                              , ".RDS")) # Back up
  
  
  #### send to git
  git2r::config(user.name = "Nutterules",user.email = "63188954+Nutterules@users.noreply.github.com")
  
  # Check git status.
  git2r::status()
  
  # Add and commit changes. 
  git2r::add(path = "*.RDS")
  git2r::commit(message = gsub(" ", "_", paste0("dataUpdate_", Sys.time())))
  
  # Push changes to github.
  password <- cred_user_pass(username = "Nutterules", password = "Jz570906C@Rumpole1!")
  git2r::push(credentials = password)
  
  
  ###
  Sys.sleep(gap * 60)
}



  # library("sf")
# world_points<- st_centroid(world)
# world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
# 
# library("rnaturalearth")
# library("rnaturalearthdata")
# library("ggspatial")
# world <- ne_countries(scale = "medium", returnclass = "sf")
# class(world)
# mid <- median(collatedData$Median, na.rm=TRUE)
# ggplot(data = world) + geom_sf(fill= "white") + 
#   geom_text(data= world_points, aes(x=X, y=Y, label=name), color = "black", size=2, fontface = "italic", check_overlap = TRUE) + 
#   annotation_scale(location = "bl", width_hint = 0.5) + 
#   annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"), style = north_arrow_orienteering) + 
#   coord_sf(xlim = c(-15, 35), ylim = c(35, 70), expand = FALSE) + 
#   xlab("Longitude") + ylab("Latitude") + 
#   ggtitle("Border Delays in Europe") + 
#   theme(panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5), panel.background = element_rect(fill = "skyblue")) + 
#   geom_point(data=collatedData, aes(x=Latitude, y=Longitude, colour=Median)) + 
#   scale_color_gradient2(midpoint = mid, low = "blue", mid = "green", high = "red")
# 
#              