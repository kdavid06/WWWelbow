#prep area
setwd("~/MSc project/data")
if (!require(googleway)) install.packages('googleway')
library(googleway)
set_key("insert my key")

key<- "insert my key"

#read in required data
GP_services <- read.csv("GP_services.csv", header=TRUE)
Hosp_services <- read.csv("Hosp_services.csv", header=TRUE)
Hosp_services$colour <- "red"
GP_services$colour <- "green"
GPs <- GP_services[,c(2,3,6,13,16,17,18)]
Hosps <- Hosp_services[,c(15,2,5,12,17,16,19)]
colnames(Hosps)[5] <- "Latitude"
colnames(Hosps)[6] <- "Longitude"
GPs$flag <- "GPs"
Hosps$flag <- "Hospitals"
GPs$ID <- paste(GPs$flag,GPs$GPID, sep="_")
Hosps$ID <- paste(Hosps$flag, Hosps$HospID, sep="_")
GPs <- GPs[,-1]
Hosps <- Hosps[,-1]
All <- rbind(GPs, Hosps)

#set services_data value for initial testing
services_data <- All

#set postcode value for initial testing
postc <- "W1B 5AH"
geo_reply <- google_geocode(postc, simplify=TRUE)
add_com <- unlist(geo_reply$results$address_components)
geo_reply <- geo_reply$results$geometry$location
geo_reply$Home_loc <- "Home Location"
geo_reply$colour <- "blue"
#data must be long lat format for the distance matrix api
address <- matrix(unlist(geo_reply[,2:1]), ncol=2, nrow=1)

#building distance matrix output as table, list?
#build a distance matrix in R  
#switch variable order so they are stored long, lat
services_data$distance <- abs(address[1]-services_data$Longitude) + abs(address[2]-services_data$Latitude)
serv_short <- services_data[order(services_data$distance),][1:50,c(4:5,1,6)]

#create static mode variable for now
transp_mode = "driving"

#takes data as 2 column df in order latitude-longitude
dist <- google_distance(origins=geo_reply[,1:2], destinations=serv_short[,1:2], mode=transp_mode)
#data structure is 5 items each with two levels, text and value, for duration text expresesses this in minutes, whereas value is in seconds.
#seconds are rounded such that 0-90secs is 1 minute, 91-150secs is 2 mins etc.
dist_out <- as.data.frame(dist$rows$elements)
dists <- cbind(dist_out, serv_short)
dist_t5 <- dists[order(dists$distance$value),][1:5,]
dist_t5$info <- paste0("<b>", dist_t5$Name, "</b>","<br><b>Mode of transport:</b>", transp_mode, "<br><b>Duration:</b>", dist_t5$duration$text, 
                       "<br><b>Distance:</b>", dist_t5$distance$text)

#building distance info into map
sbbox <- make_bbox(lon=dist_t5$Longitude, lat=dist_t5$Latitude, f=.01)
style <- '[{"featureType":"poi.medical", "stylers":[{"visibility":"off"}]}]'
google_map(location=sbbox, search_box = F, zoom=7, styles=style) %>%
  add_markers(data=geo_reply, colour="colour", info_window = "Home_loc") %>%
  add_markers(data=dist_t5, colour="colour", info_window = "info")



#building directions element on map

#fix destination for initial testing
destination <- Hosp5[5,]
res <- google_directions(origin=a, destination=destination, mode="driving")
df_polyline <- decode_pl(res$routes$overview_polyline$points)
google_map(key=key) %>%
  add_polylines(data=dir_t5, polyline="polyline")

#way for user to select one of the below and to change this into the type needed for function
"Car", "Public transport", "Bike", "Foot"
