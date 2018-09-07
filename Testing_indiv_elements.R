#running time tests

###all time tests are done using the simple code wrapped around the operations
start_time <- Sys.time()
end_time <- Sys.time()
end_time - start_time


#set up
if (!require(shiny)) install.packages('shiny')
library(shiny)
if (!require(shinydashboard)) install.packages('shinydashboard')
library(shinydashboard)
if (!require(shinyjs)) install.packages('shinyjs')
library(shinyjs)
if (!require(leaflet)) install.packages('leaflet')
library(leaflet)
if (!require(ggmap)) install.packages('ggmap')
library(ggmap)
if (!require(googleway)) install.packages('googleway')
library(googleway)
if (!require(mapsapi)) install.packages('mapsapi')
library(mapsapi)

setwd("~/MSc project/data")
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
services_data <- All

#geocoding user entered postcode
start_time <- Sys.time()
postc = "W14 8RU"
geo_reply <- google_geocode(postc, simplify=TRUE)
add_com <- unlist(geo_reply$results$address_components)
geo_reply <- geo_reply$results$geometry$location
geo_reply$Home_loc <- "Home Location"
geo_reply$colour <- "blue"
end_time <- Sys.time()
end_time - start_time

### 0.25 seconds when we exclude the validation checks.

##shortlist algorithm
start_time <- Sys.time()
services_data$distance <- abs(address[1]-services_data$Latitude) + abs(address[2]-services_data$Longitude)
serv_short <- services_data[order(services_data$distance),][1:50,c(4:5,1,6)]
end_time <- Sys.time()
end_time - start_time

## distance algorithm
stpo <- geo_reply
start_time <- Sys.time()
dist <- google_distance(origins=stpo[,1:2], destinations=serv_short[,1:2], mode=transp_mode)
home_address <<- dist$origin_addresses
dist_out <- as.data.frame(dist$rows$elements)
dists <- cbind(dist_out, serv_short)
dist_t5 <<- dists[order(dists$distance$value),][1:5,]
dist_t5$info <- paste0("<b>", dist_t5$Name, "</b>","<br><b>Mode of transport:</b>", transp_mode, "<br><b>Duration:</b>", dist_t5$duration$text, 
                       "<br><b>Distance:</b>", dist_t5$distance$text)
end_time <- Sys.time()
end_time - start_time

## distance algorithm if we didnt have the shortlist algorithm
stpo <- geo_reply
serv_short <- services_data[order(services_data$distance),][,c(4:5,1,6)]
start_time <- Sys.time()
lst_dis <- lapply(1:10, function(x) {
  if (x==1){
    i=1
  }else{
    i= (1000*(x-1))
  }
  if (x==10){
    j=9235
  }else{
    j=(1000*x)-1
  }
  dist <- google_distance(origins=stpo[,1:2], destinations=serv_short[1:500,1:2], mode=transp_mode)
})
dist1 <- data.table::rbindlist(lst_dis)
home_address <<- dist$origin_addresses
dist_out <- as.data.frame(dist$rows$elements)
dists <- cbind(dist_out, serv_short)
dist_t5 <<- dists[order(dists$distance$value),][1:5,]
dist_t5$info <- paste0("<b>", dist_t5$Name, "</b>","<br><b>Mode of transport:</b>", transp_mode, "<br><b>Duration:</b>", dist_t5$duration$text, 
                       "<br><b>Distance:</b>", dist_t5$distance$text)
end_time <- Sys.time()
end_time - start_time

## directions algorithm
t5_input <- dist_t5
stpo <- geo_reply
start_time <- Sys.time()
lst_dir <- lapply(1:nrow(t5_input), function(x) {
  google_directions(origin=stpo[1:2], destination=t5_input[x,5:6], mode=transp_mode)
})
lst_res <- lapply(lst_dir, function(x){
  data.frame(polyline=direction_polyline(x))
})
dir_t5 <- data.table::rbindlist(lst_res)
end_time <- Sys.time()
end_time - start_time

#directions algorithm on 1 point
start_time <- Sys.time()
destination <- dist_t5[5,5:6]
res <- google_directions(origin=geo_reply[,1:2], destination=destination, mode="driving", simplify =TRUE)
df_polyline <- data.frame(polyline=direction_polyline(res))
df_polyline <- decode_pl(res$routes$overview_polyline$points)
end_time <- Sys.time()
end_time - start_time

##time tests on proximity analysis
> time1
Time difference of 1.611573 mins
> time2
Time difference of 1.065272 secs
> time3
Time difference of 17.25208 mins
> time4
Time difference of 1.497348 secs
> time5
Time difference of 15.92083 mins
> sub_time1
Time difference of 5.104997 mins
> sub_time2
Time difference of 7.491289 mins
> time6
Time difference of 15.81771 mins