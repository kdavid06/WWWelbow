#function to load in all the required packages for the full library
setup_wrkspc <- function(){
  #below package contains functions to do the initial data cleaning and preprocessing
  if (!require(plotly)) install.packages('plotly')
  library(plotly)
  if (!require(plyr)) install.packages('plyr')
  library(plyr)
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
  if (!require(ggplot2)) install.packages('ggplot2')
  library(ggplot2)
  if (!require(reshape2)) install.packages('reshape2')
  library(reshape2)
  if (!require(readxl)) install.packages('readxl')
  library(readxl)
  
  #below package contains functions to perform the spatial transformation
  if (!require(rgdal)) install.packages('rgdal')
  library(rgdal)
  if (!require(rgeos)) install.packages('rgeos')
  library(rgeos)
  if (!require(yaImpute)) install.packages('yaImpute')
  library(yaImpute)
  if (!require(SDMTools)) install.packages('SDMTools')
  library(SDMTools)
  if (!require(googleway)) install.packages('googleway')
  library(googleway)
  if (!require(future.apply)) install.packages('future.apply')
  library(future.apply)
  
  #below packages are required for the findHealth application
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
  
  #Analysis for latest version of data including LSOA codes
  setwd("~/MSc project/data")
  #Load google maps API key
  key <- readLines("apikey.txt")
  return(key)
}

#function to load in the required historical hospital records data, this can be the raw data or processed data.
prepare_HES <- function(data_source){
  bins = c(0,1,2,3,4,5,6,7,8,9,10,15,30,50,100,250,500,700)
  if (data_source=="HD_home.csv"){
    HD_home <- read.csv(data_source, header=TRUE)
    HD_home <- HD_home[,-1]
  } else {
    HESdist1 <- read.csv(data_source, header=TRUE, sep=",")
    HESdist1$Arr_Month <- as.factor(HESdist1$Arr_Month)
    HESdist1$SITEDIST_FLAG <- as.factor(HESdist1$SITEDIST_FLAG)
    HESdist1$LSOA11 <- as.factor(HESdist1$LSOA11)
    HESdist1$AEINCLOCTYPE <- as.factor(HESdist1$AEINCLOCTYPE)
    HESdist1$UNNECESSARY <- as.factor(HESdist1$UNNECESSARY)
    levels(HESdist1$UNNECESSARY) <- c("Necessary", "Unnecessary")
    HESdis1 <- HESdist1[!is.na(HESdist1$SITEDIST),]
    HESdis1<- HESdis1[!grepl("^W", HESdis1$LSOA11),]
    
    #split SITEDIST into a bucket variable
    bin_names = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-15","15-30","30-50","50-100","100-250","250-500","500-700")
    HESdis1$DISTbucket <- 0
    HESdis1$DISTbucket <- cut(HESdis1$SITEDIST, breaks=bins, labels=bin_names)
    DIST1_bin <- HESdis1 %>% group_by(DISTbucket) %>% dplyr::summarize(count=n())
    #create graph here and save within function
    
    #create clean usable subset of data, only keep items where incident happenned at home and site location was used to calculate SITEDIST
    HD_home <- subset(HESdis1, AEINCLOCTYPE==10)
    HD_home <- subset(HD_home, SITEDIST_FLAG==5)
    HD_home$Attendances <- as.numeric(HD_home$Attendances)
    HD_home <- droplevels(HD_home)
    HD_home <- HD_home[,c(1:3,7,12:14)]
    HD_home <- HD_home %>% group_by(Arr_Month, LSOA11, SITETRET, SITEDIST, UNNECESSARY, Attendances, DISTbucket) %>% dplyr::summarize(count=n())
    HD_home <- HD_home %>% group_by(LSOA11, SITETRET, SITEDIST, UNNECESSARY, DISTbucket) %>% dplyr::summarise(Attendances = sum(Attendances))
    write.csv(HD_home, "HD_home.csv")
  }
  return(HD_home)
}

#function to create the LSOA location data, data_source can be the raw data or processed version
create_LSOAloc <- function(data_source){
  if (data_source=="LSOA_loc.csv"){
    LSOA_loc <- read.csv(data_source, header=TRUE)
    LSOA_loc <- LSOA_loc[,-1]
  } else {
    ##read in LSOA location data
    #code to convert east/nort to long/lat taken from http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf
    
    # Variables for holding the coordinate system types (see:
    # http://www.epsg.org/ for details)
    ukgrid = "+init=epsg:27700"
    latlong = "+init=epsg:4326"
    
    LSOA <- read.csv(data_source, header=TRUE)
    colnames(LSOA)[c(3,4)] <- c("Easting", "Northing")
    
    #check all LSOAs are in England
    LSOA[!grepl("E01", LSOA$code),]
    
    #Create spatial dataframe
    coords <- cbind(Easting = as.numeric(as.character(LSOA$Easting)), Northing = as.numeric(as.character(LSOA$Northing)))
    LSOA_sp <- SpatialPointsDataFrame(coords, data=data.frame(LSOA$name, LSOA$code), proj4string = CRS("+init=epsg:27700"))
    
    #convert to long/lats
    LSOA_ll <- spTransform(LSOA_sp, CRS(latlong))
    colnames(LSOA_ll@coords)[colnames(LSOA_ll@coords) == "Easting"] <- "Longtitude"
    colnames(LSOA_ll@coords)[colnames(LSOA_ll@coords) == "Northing"] <- "Latitude"
    LSOA_loc <- as.data.frame(LSOA_ll) 
    write.csv(LSOA_loc, "LSOA_loc.csv")
  }
  return(LSOA_loc)
}

read_allServices <- function(){
  if (!exists("all_services")){
    all_services <- read.csv("20_June_2018_CQC_directory.csv", skip=4, header = TRUE, sep=',')
  }
  return(all_services)
}

#function to create the dataset of hospitals with location data
create_hosp <- function(data_source){
  if (data_source=="Hosp_services.csv"){
    Hosp_services <- read.csv(data_source, header=TRUE)
    Hosp_services <- Hosp_services[,-1]
  } else {
    #create the hospitals data
    Hosp_services <- all_services[grep("*Hospital*", all_services$Service.types),]
    Hosp_services$HospID <- 1:nrow(Hosp_services)
    Hosp_services <- Hosp_services[,-c(5,6)]
    #there is no unique identifier in the CQC data to match on, so explore other options
    #given the number of rows we have we could create the long/lat data from the googleway geocoder
    #both options were explored, the batch geodcoder although very quick didnt identify 21 of the postcodes
    res <- future_apply(Hosp_services[1:10,],1, function(x){
      google_geocode(x[['Postcode']], key = key)
    })
    coords <- future_lapply(seq_along(res), function(x){
      coords <- res[[x]]$results$geometry$location
      Postcode <- Hosp_services[x, 'Postcode']
      res_df <- data.frame(postcode = Postcode,
                           lat=coords[,"lat"],
                           lon=coords[,"lng"])
    })
    Hosp_coords <- do.call(rbind, coords)
    Hosp_services$lon <- Hosp_coords$lon[match(Hosp_services$Postcode, Hosp_coords$postcode)]
    Hosp_services$lat <- Hosp_coords$lat[match(Hosp_services$Postcode, Hosp_coords$postcode)]
    
    #alter location data slightly so no 2 services exist at the same point
    Hosp_services$longlat <- paste(Hosp_services$lat, Hosp_services$lon, sep=" ")
    Hosp_services$longlat <- as.factor(Hosp_services$longlat)
    Hosp_services$noServices_flag <- with(Hosp_services, as.numeric(factor(longlat, levels=unique(longlat) )))
    Hosp_services$dup <- ave(seq_len(nrow(Hosp_services)), Hosp_services$longlat, FUN=seq_along)
    for (i in 1:max(Hosp_services$dup)){
      if (i%%2 == 0){
        Hosp_services$lat[Hosp_services$dup==i] <- Hosp_services$lat[Hosp_services$dup==i] + (i*0.0001)
      }else{
        Hosp_services$lon[Hosp_services$dup==i] <- Hosp_services$lon[Hosp_services$dup==i] - (i*0.0001)
      }
    }  

    #check this looks sensible on a map visual, selected an area with the maximum 6 overlapping markers
    test_map <- subset(Hosp_services, Hosp_services$noServices_flag=="412")
    google_map(key=key) %>% add_markers(data=test_map, lat="lat", lon="lon", info_window = "Name")
  
    Hosp_services$longlat <- paste(Hosp_services$lat, Hosp_services$lon, sep=" ")
    Hosp_services$longlat <- as.factor(Hosp_services$longlat)
    Hosp_services$dup <- ave(seq_len(nrow(Hosp_services)), Hosp_services$longlat, FUN=seq_along)
    max(Hosp_services$dup)
    #clean up the data to remove the columns for testing which are no longer required
    Hosp_services <- Hosp_services[,-c(18:20)]
  
    #store data containing lon/lat to save rerunning code
    write.csv(Hosp_services, "Hosp_services.csv")
  }
  return(Hosp_services)
}

#function to create GP data including location
create_GP <- function(data_source){
  if (data_source=="GP_services.csv"){
    GP_services <- read.csv(data_source, header=TRUE)
    GP_services <- GP_services[,-1]
  } else {
    GP_services <- all_services[grep("*GPs*", all_services$Service.types),]
    GP_services$GPID <- 1:nrow(GP_services)
    GP_services <- GP_services[,-c(5:6)]
    GP_services <- droplevels(GP_services)
    GP_ll <- read.csv("GP_services_ll.csv", header=TRUE, sep=",")
    GP_services <- merge(GP_services,GP_ll[,c(2,9,10)], by="GPID")
    GP_services <- GP_services[!is.na(GP_services$Longitude),]
    
    
    #clean out duplicate services at one location
    GP_services$longlat <- as.factor(paste(GP_services$Latitude, GP_services$Longitude, sep=" "))
    GP_services <- GP_services %>% group_by(longlat) %>% mutate(Date.of.latest.check=format(max(as.Date(Date.of.latest.check)), "%d/%m/%Y")) %>% distinct(longlat, .keep_all = TRUE)
    #check only 1 instance of every unique longlat
    GP_serv_check <- GP_services %>% group_by(longlat) %>% mutate(count=n())
    max(GP_serv_check$count)
    rm(GP_serv_check)
    GP_services <- GP_services[,-17]
    GP_services$GPID <- 1:nrow(GP_services)
    
    #store data containing lon/lat to save rerunning code
    write.csv(GP_services, "GP_services.csv")
  }
  return(GP_services)
}

#function to  calculate distance between LSOAs and hospitals and GPs
calc_dist <- function(rerun){
  if (rerun=="FALSE" & file.exists("min_hospDist.csv") & file.exists("min_GPDist.csv")){
    min_HospDist <- read.csv("min_hospDist.csv")[,-1]
    min_GPDist <- read.csv("min_GPDist.csv")[,-1]
    LS <- read.csv("LS.csv")[,-1]
  }else{
    items <- setup_wrkspc()
    wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    utm10n<-CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
    ###creating the distances data using the location data for LSOAs, hospitals and GPS.
    #create subset for testing distance algorithms
    LS_test <- LSOA_loc[grepl("Middlesbrough", LSOA_loc[["LSOA.name"]]),]
    GP_test <- GP_services[grepl("Middlesbrough", GP_services[["Local.Authority"]]),]
    H_test <- Hosp_services[grepl("Middlesbrough", Hosp_services[["Local.Authority"]]),]
    
    #LSOA data
    LS <- LS_test[,-1]
    colnames(LS) <- c("LSOA.code","lon","lat")
    lat <- LS[,3]
    lon <-LS[,2]
    LSOA.code <- LS[,1]
    LS_sp <- SpatialPointsDataFrame(data.frame(x=lon,y=lat),data=data.frame(LSOA.code),proj4string = wgs84)
    LS_proj <- spTransform(LS_sp, utm10n)
    
    #Hospital data
    H_lat <- H_test$lat
    H_lon <- H_test$lon
    H_code <- H_test$HospID
    Ho <- data.frame(H_lat,H_lon,H_code)
    Ho$ID <-1:nrow(Ho)
    H <- SpatialPointsDataFrame(data.frame(x=H_lon, y=H_lat),data=data.frame(ID=1:nrow(Ho)), proj4string = wgs84)
    H_proj <-spTransform(H, utm10n)
    
    #GP data
    GP_lon <- GP_test$Longitude
    GP_lat <- GP_test$Latitude
    GP_code <- GP_test$GPID
    GP <- data.frame(GP_lon, GP_lat, GP_code)
    GP_sp <- SpatialPointsDataFrame(data.frame(x=GP_lon, y=GP_lat), data=data.frame(ID=1:nrow(GP)), proj4string = wgs84)
    GP_proj <- spTransform(GP_sp, utm10n)
    
    
    #calculating distance
    #Hosp
    disth <- gDistance(LS_proj,H_proj,byid=T)
    min_thospDist <- future_apply(disth,2,min)
    LS_sp@data$Nearest_hosp <- min_thospDist
    LS_sp@data$Near_HID <- as.vector(future_apply(disth,2,function(x) which(x==min(x))))
    #GP
    distgp <- gDistance(LS_proj,GP_proj,byid=T)
    min_tGPDist <- future_apply(distgp,2,min)
    LS_sp@data$Nearest_GP <- min_tGPDist
    LS_sp@data$Near_GPID <- as.vector(future_apply(distgp,2,function(x) which(x==min(x))))
    
    pop1<-paste0("<b>DistanceHosp</b>: ",round(LS_sp$Nearest_hosp,2),"<br><b>DistanceGP</b>: ",round(LS_sp$Nearest_GP,2),"<br><b>Near Hosp ID</b>: ",LS_sp$Near_HID, "<br><b>Near GP ID</b>: ",LS_sp$Near_GPID)
    pop2<-paste0("<b>ID</b>: ",H$ID, "<br><b>Near ID</b>: ", Ho$H_code)
    pop3 <- paste0("<b>ID</b>: ",GP_sp$ID)
    services_Icons <- iconList(
      hosp_icon <- makeIcon("Hosp_icon.png"),
      GP_icon <- makeIcon("GP_icon.png")
    )
    
    m<-leaflet()%>%
      addTiles()%>%
      addCircleMarkers(data=LS_sp,radius=8,fillColor =   'red',fillOpacity=0.8,weight=1,color='black',popup=pop1)%>%
      addCircleMarkers(data=H,radius=8,fillColor =   'blue',fillOpacity=0.8,weight=1,color='black',popup=pop2)%>%
      addCircleMarkers(data=GP_sp,radius=8,fillColor =   'green',fillOpacity=0.8,weight=1,color='black',popup=pop3)
    m
    
    endsub <- Sys.time()
    subtime3 <- endsub - startsub 
    
    startsub <- Sys.time()
    #full     
    #LSOA data
    LS <- LSOA_loc[,-c(1)]
    colnames(LS) <- c("LSOA.code","lon","lat")
    lat <- LS[,3]
    lon <-LS[,2]
    LSOA.code <- LS[,1]
    LS_sp <- SpatialPointsDataFrame(data.frame(x=lon,y=lat),data=data.frame(LSOA.code),proj4string = wgs84)
    LS_proj <- spTransform(LS_sp, utm10n)
    
    #Hospital data
    H_lat <- Hosp_services$lat
    H_lon <- Hosp_services$lon
    H_code <- Hosp_services$HospID
    Ho <- data.frame(H_lat,H_lon,H_code)
    Ho$ID <-1:nrow(Ho)
    H <- SpatialPointsDataFrame(data.frame(x=H_lon, y=H_lat),data=data.frame(ID=1:nrow(Ho)), proj4string = wgs84)
    H_proj <-spTransform(H, utm10n)
    
    #GP data
    GP_lon <- GP_services$Longitude
    GP_lat <- GP_services$Latitude
    GP_code <- GP_services$GPID
    GP <- data.frame(GP_lon, GP_lat, GP_code)
    GP$GPID <- 1:nrow(GP)
    GP_sp <- SpatialPointsDataFrame(data.frame(x=GP_lon, y=GP_lat), data=data.frame(ID=1:nrow(GP)), proj4string = wgs84)
    GP_proj <- spTransform(GP_sp, utm10n)
    
    endsub <- Sys.time()
    subtime4 <- endsub - startsub 
    
    #calculating distance
    #Hosp
    substart <- Sys.time()
    disth <- gDistance(LS_proj,H_proj,byid=T)
    min_hospDist <- future_apply(disth,2,min)
    write.csv(min_hospDist, "min_hospDist.csv")
    subend <- Sys.time()
    sub_time1 <- subend-substart
    
    #GP
    substart <- Sys.time()
    distgp <- gDistance(LS_proj,GP_proj,byid=T)
    min_GPDist <- future_apply(distgp,2,min)
    write.csv(min_GPDist, "min_GPDist.csv")
    subend <- Sys.time()
    sub_time2 <- subend-substart
    
    #create distance columns in original DS
    startsub <- Sys.time()
    LS_sp@data$Nearest_hosp <- min_hospDist
    LS_sp@data$Near_HID <- as.vector(future_apply(disth,2,function(x) which(x==min(x))))
    LS_sp@data$Nearest_GP <- min_GPDist
    LS_sp@data$Near_GPID <- as.vector(future_apply(distgp,2,function(x) which(x==min(x))))
    
    pop1<-paste0("<b>DistanceHosp</b>: ",round(LS_sp$Nearest_hosp,2),"<br><b>DistanceGP</b>: ",round(LS_sp$Nearest_GP,2),"<br><b>Near Hosp ID</b>: ",LS_sp$Near_HID, "<br><b>Near GP ID</b>: ",LS_sp$Near_GPID)
    pop2<-paste0("<b>ID</b>: ",H$ID, "<br><b>Near ID</b>: ", Ho$H_code)
    pop3 <- paste0("<b>ID</b>: ",GP_sp$ID)
    services_Icons <- iconList(
      hosp_icon <- makeIcon("Hosp_icon.png"),
      GP_icon <- makeIcon("GP_icon.png")
    )
    
    m<-leaflet()%>%
      addTiles()%>%
      addCircleMarkers(data=LS_sp,radius=8,fillColor =   'red',fillOpacity=0.8,weight=1,color='black',popup=pop1)%>%
      addCircleMarkers(data=H,radius=8,fillColor =   'blue',fillOpacity=0.8,weight=1,color='black',popup=pop2)%>%
      addCircleMarkers(data=GP_sp,radius=8,fillColor =   'green',fillOpacity=0.8,weight=1,color='black',popup=pop3) %>%
      setView(-0.204365,51.493633, zoom=15)
    
    
    LS$dist_hosp <- LS_sp@data$Nearest_hosp
    LS$dist_gp <- LS_sp$Nearest_GP
    LS$near_hospID <- LS_sp$Near_HID
    LS$near_GPID <- LS_sp$Near_GPID
    LS$GPcloser <- 1
    LS$GPcloser[LS$dist_gp >= LS$dist_hosp] <- 0
    LS$GPcloser <- as.factor(LS$GPcloser)
    levels(LS$GPcloser) <- c("HospitalClosest", "GPClosest")
    write.csv(LS,"LS.csv")
    endsub <- Sys.time()
    subtime5 <- endsub - startsub 
  }
  return(LS)
}

#function to create location data for hospitals attended in historical hospital records data
create_siteLoc <- function(rerun){
  if (rerun==TRUE & !"SITE_lat" %in% colnames(HD_home)){
    #Reading in ODS data to support adding location element to sites in HES data
    ODS <- read.csv("ets.csv", header=FALSE)
    ODS <- ODS[,c(1,2,10,15)]
    colnames(ODS) <- c("Site.code", "Org.name", "Postcode", "Org.code")
    HES_sites <- as.data.frame(unique(HD_home$SITETRET))
    colnames(HES_sites) <- c("SITETRET")
    HES_sites$Postcode <- ODS$Postcode[match(HES_sites$SITETRET, ODS$Site.code)]
    #upon examination there are 23 sites which werent found in ODS data, however when using CQC data 106 arent found
    No_match <- HES_sites[is.na(HES_sites$Postcode),]
    #I can see these are valid sites but as no access to location data only option is to exclude
    HD_home$Postcode <- HES_sites$Postcode[match(HD_home$SITETRET, HES_sites$SITETRET)]
    HD_home <- HD_home[!is.na(HD_home$Postcode),]
    #add the long/lat information from Hosp_services where these sites exist.
    HD_home$SITE_lat <- Hosp_services$lat[match(HD_home$Postcode, Hosp_services$Postcode)]
    HD_home$SITE_lon <- Hosp_services$lon[match(HD_home$Postcode, Hosp_services$Postcode)]
    HD_unmatch <- HD_home[is.na(HD_home$SITE_lat),7]
    #By extracting the unmatched sites by postcode there are only 40 unique postcodes to geocode
    HD_unmatch <- as.data.frame(unique(HD_unmatch))
    
    #for the sites not found create long/lat data using googleway geocoder method used above.
    startsub <- Sys.time()
    res <- future_apply(HD_unmatch,1, function(x){
      google_geocode(x[['Postcode']], key = key)
    })
    coords <- future_lapply(seq_along(res), function(x){
      coords <- res[[x]]$results$geometry$location
      Postcode <- HD_unmatch[x, 'Postcode']
      res_df <- data.frame(postcode = Postcode,
                           lat=coords[,"lat"],
                           lon=coords[,"lng"])
    })
    Hosp_coords <- do.call(rbind, coords)
    HD_home$SITE_lon[is.na(HD_home$SITE_lon)] <- Hosp_coords$lon[match(HD_home$Postcode[is.na(HD_home$SITE_lon)], Hosp_coords$postcode)]
    HD_home$SITE_lat[is.na(HD_home$SITE_lat)] <- Hosp_coords$lat[match(HD_home$Postcode[is.na(HD_home$SITE_lat)], Hosp_coords$postcode)]
    rm(Hosp_coords, HD_unmatch)
  }
  return(HD_home)
}

#function to combine the historical records data with the distance data on hospitals and GPs
comb_data <- function(rerun){
  if (rerun==FALSE & file.exists("HES_comb.csv")){
    HES_comb <- read.csv("HES_comb.csv", header=TRUE)[,-1]
    HES_comb_w <- read.csv("HES_comb_w.csv", header=TRUE)[,-1]
  } else {   
    wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    utm10n<-CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
    ##Combining the different data sets
    HES_comb <- merge(x=HD_home, y=LS, by.x="LSOA11", by.y="LSOA.code", all.x=TRUE)
    
    #create clean usable subset of data
    HES_comb <- HES_comb[,-7] 
    colnames(HES_comb)[9] <- "LSOA_lon"
    colnames(HES_comb)[10] <- "LSOA_lat"
    
    
    #add distance from the LSOA to the hospital attended on to dataset
    HD_SL <- aggregate(Attendances~ LSOA11 + SITETRET + LSOA_lon + LSOA_lat + SITE_lon + SITE_lat, HES_comb, sum)
    HD_SL <- droplevels(HD_SL)
    
    #convert to spatial table
    H1 <- SpatialPointsDataFrame(data.frame(x=HD_SL$SITE_lon, y=HD_SL$SITE_lat),data=data.frame(ID=1:nrow(HD_SL)), proj4string = wgs84)
    H1_proj <-spTransform(H1, utm10n)
    H1_proj.split <- split(H1_proj, seq_len(length(H1_proj)))
    LS1 <- SpatialPointsDataFrame(data.frame(x=HD_SL$LSOA_lon, y=HD_SL$LSOA_lat),data=data.frame(ID=1:nrow(HD_SL)), proj4string = wgs84)
    LS1_proj <-spTransform(LS1, utm10n)
    LS1_proj.split <- split(LS1_proj, seq_len(length(LS1_proj)))
    LSH <- cbind(c(1:nrow(HD_SL)),c(1:nrow(HD_SL)))
    
    #find distance for each row in dataframe.
    HD_SL$act_dist_hosp <- 0
    dists <- apply(LSH, 1, function(x) 
      gDistance(LS1_proj.split[[x[1]]], H1_proj.split[[x[1]]], hausdorff=TRUE))
    HD_SL$act_dist_hosp <-dists
    
    HES_comb <- merge(HES_comb, HD_SL[,c("LSOA11", "SITETRET", "act_dist_hosp")], by=c("LSOA11", "SITETRET"), all.x=TRUE)
    #Is the nearest hospital the one attended flag? If not return distance from LSOA to that hospital.
    HES_comb$nearest_hosp_att <- HES_comb$dist_hosp == HES_comb$act_dist_hosp
    #Is hosp attended closer than nearest GP
    HES_comb$GP_closer_att <- 0
    HES_comb$GP_closer_att[HES_comb$act_dist_hosp >= HES_comb$dist_gp] <- 1
    
    #convert distances into km for ease
    HES_comb$dist_hosp <- HES_comb$dist_hosp/1000
    HES_comb$dist_gp <- HES_comb$dist_gp/1000
    HES_comb$act_dist_hosp <- HES_comb$act_dist_hosp/1000
    
    
    #convert the unneccessary from long to wide format
    HES_comb$LSOA_SITE <- paste(HES_comb$LSOA11, HES_comb$SITETRET, sep="_")
    HD_unnec <- dcast(HES_comb, LSOA_SITE ~ UNNECESSARY, value.var="Attendances")
    #convert dataframe to LSOA and site combination level with total number of attendances.
    HD_SL <- aggregate(Attendances~ LSOA11 + SITETRET + SITEDIST+ DISTbucket + SITE_lon + SITE_lat + LSOA_lon + LSOA_lat + dist_hosp +
                         dist_gp + near_hospID + near_GPID + GPcloser + act_dist_hosp + nearest_hosp_att + LSOA_SITE, HES_comb, sum)
    HD_SL <- droplevels(HD_SL)
    HES_comb_w <- merge(x=HD_SL, y=HD_unnec, by="LSOA_SITE", all.x=TRUE)
    HES_comb_w$Necessary[is.na(HES_comb_w$Necessary)] <- 0
    HES_comb_w$Unnecessary[is.na(HES_comb_w$Unnecessary)] <- 0
    HES_comb_w$prop_unnec <- HES_comb_w$Unnecessary/(HES_comb_w$Unnecessary+HES_comb_w$Necessary)
    
    write.csv(HES_comb, "HES_comb.csv")
    write.csv(HES_comb_w, "HES_comb_w.csv")
  }
  return(list(HES_comb, HES_comb_w))
}

#function to load the required data into the server side of the shiny application
load_data <- function(){
  if (file.exists("All.csv")){
    All <- read.csv(data_source, header=TRUE)
  }else{
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
    write.csv(All, "All.csv")
  }
  return(All)
}

#function to set up the server side of the shiny application
set_up_shiny <- function(){
  fieldsMandatory <- c("myLocation")
  home_address <<- ""
  dist_t5 <<- data.frame()
  `%then%` <- shiny:::`%OR%`
  key <- readChar("apikey.txt", file.info("apikey.txt")$size)
  set_key(key)
  return(key)
}

#prepare the HES data including the registered GP code.
prepare_HES_regGP <- function(data_source){
  if(data_source=="HDGP.csv"){
    HDGP <- read.csv(data_source, header=TRUE)
  } else {
    HES_regGP <- read.csv(data_source, header=TRUE)
    HES_regGP$Arr_Month <- as.factor(HES_regGP$Arr_Month)
    HES_regGP$SITEDIST_FLAG <- as.factor(HES_regGP$SITEDIST_FLAG)
    HES_regGP$LSOA11 <- as.factor(HES_regGP$LSOA11)
    HES_regGP$AEINCLOCTYPE <- as.factor(HES_regGP$AEINCLOCTYPE)
    HES_regGP$UNNECESSARY <- as.factor(HES_regGP$UNNECESSARY)
    levels(HES_regGP$UNNECESSARY) <- c("Necessary", "Unnecessary")
    HES_regGP$Unregistered <- 0
    HES_regGP$Unregistered[HES_regGP$GPPRAC=="V81997"] <- 1
    HES_regGP$Unregistered <- as.factor(HES_regGP$Unregistered)
    levels(HES_regGP$Unregistered) <- c("Registered", "Unregistered")
    HESdisGP <- HES_regGP[!is.na(HES_regGP$SITEDIST),]
    HESdisGP<- HESdisGP[!grepl("^W", HESdisGP$LSOA11),]
    
    #split SITEDIST into a bucket variable
    bins = c(0,1,2,3,4,5,6,7,8,9,10,15,30,50,100,250,500,700)
    bin_names = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-15","15-30","30-50","50-100","100-250","250-500","500-700")
    HESdisGP$DISTbucket <- cut(HESdisGP$SITEDIST, breaks=bins, labels=bin_names)
    
    #create clean usable subset of data, only keep items where incident happenned at home and site location was used to calculate SITEDIST
    HDGP <- subset(HESdisGP, AEINCLOCTYPE==10)
    HDGP <- subset(HDGP, SITEDIST_FLAG==5)
    HDGP$Attendances <- as.numeric(HDGP$Attendances)
    HDGP <- droplevels(HDGP)
    HDGP <- HDGP[,c(1:3,7,12:16)]
    HDGP <- HDGP %>% group_by(Arr_Month, LSOA11, SITETRET, SITEDIST, Attendances, UNNECESSARY, GPPRAC, Unregistered, DISTbucket) %>% dplyr::summarize(count=n())
    HDGP <- HDGP %>% group_by(LSOA11, SITETRET, SITEDIST, UNNECESSARY, GPPRAC, Unregistered, DISTbucket) %>% dplyr::summarise(Attendances = sum(Attendances))
    write.csv(HDGP, "HDGP.csv")
  }
  return(HDGP)
}

#create location data for the registered GPs.
create_GPLoc <- function(data_source){
  if (data_source=="HDGP_c.csv"){
    HDGP_c <- read.csv(data_source, header=TRUE)
  }else{
    ODS_GP <- read.csv (data_source, header=FALSE)
    ODS_GP <- ODS_GP[,c(1,2,10,15)]
    colnames(ODS_GP) <- c("Org.code", "Org.name", "Postcode", "Prac.code")
    HDGP<- HDGP[!grepl("^V8199", HDGP$GPPRAC),]
    HDGP<- HDGP[!grepl("&", HDGP$GPPRAC),]
    GP_sites <- as.data.frame(unique(HDGP$GPPRAC))
    colnames(GP_sites) <- c("GPPRAC")
    GP_sites$Postcode <- ODS_GP$Postcode[match(GP_sites$GPPRAC, ODS_GP$Prac.code)]
    #upon examination there are 359 sites which werent found in ODS data, however when using CQC data 106 arent found
    No_match <- GP_sites[is.na(GP_sites$Postcode),]
    #I can see these are valid sites but as no access to location data only option is to exclude
    HDGP$Postcode <- GP_sites$Postcode[match(HDGP$GPPRAC, GP_sites$GPPRAC)]
    HDGP <- HDGP[!is.na(HDGP$Postcode),]
    #a further 103 GPs were matched to a blank postcode
    HDGP <- HDGP[HDGP$Postcode!="",]
    #add the long/lat information from GP_services where these sites exist.
    GP_services <- create_GP("GP_services.csv")
    HDGP$GP_lat <- GP_services$Latitude[match(HDGP$Postcode, GP_services$Postcode)]
    HDGP$GP_lon <- GP_services$Longitude[match(HDGP$Postcode, GP_services$Postcode)]
    HD_unmatch <- HDGP[is.na(HDGP$GP_lon),]
    #By extracting the unmatched sites by postcode there are 2736 unique postcodes to geocode
    HD_unmatch <- HD_unmatch %>% group_by(Postcode) %>% dplyr::summarize(count=n())
    
    #for the GPs not found create long/lat data using googleway geocoder method used above.
    res <- future_apply(HD_unmatch,1, function(x){
      google_geocode(x[['Postcode']], key = key)
    })
    #coords keeps returning error Error in data.frame(Postcode, coords) : arguments imply differing number of rows: 1, 0
    #suspect this is due to some postcode geocdoing not being successful
    #below code establishes this is the case that 138 postcodes get no result
    status <- future_lapply(seq_along(res), function(x){
      check <- res[[x]]$status
      Postcode <- HD_unmatch[x, 'Postcode']
      check_df <- data.frame(status= check, Postcode=Postcode)
    })
    #code used in earlier geocoding for sites, is amended to pick up postcodes that dont match
    coords <- future_lapply(seq_along(res), function(x){
      Postcode <- HD_unmatch[x, 'Postcode']
      if(res[[x]]$status=="OK"){
        coords <- res[[x]]$results$geometry$location
        res_df <- data.frame(postcode = Postcode,
                             lat=coords[,"lat"],
                             lon=coords[,"lng"])
      }else{
        res_df <- data.frame(postcode=Postcode, lat="NA", lon="NA") 
      }
    })
    GP_coords <- do.call(rbind, coords)
    HDGP$GP_lon[is.na(HDGP$GP_lon)] <- GP_coords$lon[match(HDGP$Postcode[is.na(HDGP$GP_lon)], GP_coords$Postcode)]
    HDGP$GP_lat[is.na(HDGP$GP_lat)] <- GP_coords$lat[match(HDGP$Postcode[is.na(HDGP$GP_lat)], GP_coords$Postcode)]
    #138 postcodes have not been found by the geocoder
    #investigation shows some have been terminated by the royal mail
    HDGP$GP_lat <- as.numeric(HDGP$GP_lat)
    HDGP$GP_lon <- as.numeric(HDGP$GP_lon)
    HD_nomatch <- HDGP[is.na(HDGP$GP_lon),]
    rm(HD_nomatch, HD_unmatch)
    HDGP_c <- HDGP[!is.na(HDGP$GP_lon),]
    HDGP_c$LSOA_SITE <- paste(HDGP_c$LSOA11, HDGP_c$SITETRET, sep="_")
    write.csv(HDGP_c, "HDGP_c.csv")
  }
  #Reading in ODS data to support adding location element to sites in HES data
  return(HDGP_c)
}

#function to add GP data to historic data and create distane
create_dist_comb <- function(data_source){
  if(data_source=="HDGP_comb_g.csv"){
    HDGP_comb_g <- read.csv(data_source, header=TRUE)
    HDGP_comb_g <- HDGP_comb_g[,-1]
  } else{
    wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
    utm10n<-CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
    
    #combine with the HES_comb data
    HDGP_c$all <- paste(HDGP_c$LSOA_SITE, HDGP_c$UNNECESSARY)
    HES_comb$all <- paste(HES_comb$LSOA_SITE, HES_comb$UNNECESSARY)
    
    HDGP_comb <- merge(x=HDGP_c, y=HES_comb, by="all", all.x=TRUE)
    #first remove NAs where rows have been removed f
    HDGP_comb <- HDGP_comb[!is.na(HDGP_comb$SITETRET.y),]
    #remove duplicated columns
    HDGP_comb <- HDGP_comb[,-c(2,8,11,14:20,33)]
    write.csv(HDGP_comb, "HDGP_comb.csv")
    
    #add distance from the LSOA to the patients registered GP on to dataset
    HDGP_SL <- aggregate(Attendances.x~ LSOA11.x + GP_lon + GP_lat + LSOA_lon + LSOA_lat + GPPRAC, HDGP_comb, sum)
    HDGP_SL <- droplevels(HDGP_SL)
    
    #convert to spatial table
    GP1 <- SpatialPointsDataFrame(data.frame(x=HDGP_SL$GP_lon, y=HDGP_SL$GP_lat),data=data.frame(ID=1:nrow(HDGP_SL)), proj4string = wgs84)
    GP1_proj <-spTransform(GP1, utm10n)
    GP1_proj.split <- split(GP1_proj, seq_len(length(GP1_proj)))
    LS1 <- SpatialPointsDataFrame(data.frame(x=HDGP_SL$LSOA_lon, y=HDGP_SL$LSOA_lat),data=data.frame(ID=1:nrow(HDGP_SL)), proj4string = wgs84)
    LS1_proj <-spTransform(LS1, utm10n)
    LS1_proj.split <- split(LS1_proj, seq_len(length(LS1_proj)))
    LSH <- cbind(c(1:nrow(HDGP_SL)),c(1:nrow(HDGP_SL)))
    
    #find distance for each row in dataframe.
    HDGP_SL$reg_GP_dist <- 0
    dists <- apply(LSH, 1, function(x) 
      gDistance(LS1_proj.split[[x[1]]], GP1_proj.split[[x[1]]], hausdorff=TRUE))
    HDGP_SL$reg_GP_dist <-dists
    
    HDGP_comb_g <- merge(HDGP_comb, HDGP_SL[,c("LSOA11.x", "GPPRAC", "reg_GP_dist")], by=c("LSOA11.x", "GPPRAC"), all.x=TRUE)
    #convert distance into km for comparison
    HDGP_comb_g$reg_GP_dist <- HDGP_comb_g$reg_GP_dist/1000
    
    #Is the nearest GP the one registered at? 
    HDGP_comb_g$nearest_GP_reg <- HDGP_comb_g$dist_gp == HDGP_comb_g$reg_GP_dist
    #Is hosp attended closer than registered GP
    HDGP_comb_g$reg_GP_closer <- 0
    HDGP_comb_g$reg_GP_closer[HDGP_comb_g$act_dist_hosp >= HDGP_comb_g$reg_GP_dist] <- 1
    
    write.csv(HDGP_comb_g, "HDGP_comb_g.csv")
  }
  return(HDGP_comb_g)
}   

#add prop unnecessary and prop unregistered to dataframe
create_wide_elements <- function(data_source){
  if(data_source=="HDGP_comb_g_w.csv"){
    HDGP_comb_g_w <- read.csv(data_source, header=TRUE)
  } else {
    HDGP_comb_g <- read.csv(data_source, header=TRUE)
    if (file.exists("HD_comb.csv")){
      HD_comb <- read.csv("HD_comb.csv", header=TRUE)
    }else{
      #convert the unneccessary from long to wide format
      HDGP$LSOA_SITE_GP <- paste(HDGP$LSOA11, HDGP$SITETRET, HDGP$GPPRAC, sep="_")
      HD_unnec <- dcast(HDGP, LSOA_SITE_GP ~ UNNECESSARY, value.var="Attendances")
      HD_unreg <- dcast(HDGP, LSOA_SITE_GP ~ Unregistered, value.var = "Attendances")
      #convert dataframe to LSOA and site combination level with total number of attendances.
      HD_SL <- aggregate(Attendances~ LSOA11 + SITETRET + SITEDIST+ GPPRAC + DISTbucket + LSOA_SITE_GP, HDGP, sum)
      HD_SL <- droplevels(HD_SL)
      HD_comb <- merge(x=HD_SL, y=HD_unnec, by="LSOA_SITE_GP", all.x=TRUE)
      HD_comb <- merge(x=HD_comb, y=HD_unreg, by="LSOA_SITE_GP", all.x=TRUE)
      
      HD_comb$Necessary[is.na(HD_comb$Necessary)] <- 0
      HD_comb$Unnecessary[is.na(HD_comb$Unnecessary)] <- 0
      HD_comb$Registered[is.na(HD_comb$Registered)] <- 0
      HD_comb$Unregistered[is.na(HD_comb$Unregistered)] <- 0
      write.csv(HD_comb, "HD_comb.csv")
    }
    #combine new data with full data
    HDGP_comb_g_w <- HDGP_comb_g %>% group_by(LSOA11.x, GPPRAC, SITETRET.x, SITEDIST.x, DISTbucket.x, GP_lat, GP_lon, 
                                              SITE_lat, SITE_lon, LSOA_lon, LSOA_lat, dist_hosp, dist_gp, near_hospID, 
                                              near_GPID, GPcloser, act_dist_hosp, nearest_hosp_att, GP_closer_att, reg_GP_dist,
                                              nearest_GP_reg, reg_GP_closer) %>% dplyr::summarise(Attendances=sum(Attendances.x))
    HDGP_comb_g_w$LSOA_SITE_GP <- paste(HDGP_comb_g_w$LSOA11.x, HDGP_comb_g_w$SITETRET.x, HDGP_comb_g_w$GPPRAC, sep="_")
    HDGP_comb_g_w$Unnecessary <- HD_comb$Unnecessary[match(HDGP_comb_g_w$LSOA_SITE_GP, HD_comb$LSOA_SITE_GP)]                                      
    HDGP_comb_g_w$Necessary <- HD_comb$Necessary[match(HDGP_comb_g_w$LSOA_SITE_GP, HD_comb$LSOA_SITE_GP)]                                      
    HDGP_comb_g_w <- HDGP_comb_g_w[,c(1:3,12:26)]
    HDGP_comb_g_w$prop_unnec <- HDGP_comb_g_w$Unnecessary/(HDGP_comb_g_w$Unnecessary+HDGP_comb_g_w$Necessary)
    write.csv(HDGP_comb_g_w, "HDGP_comb_g_w.csv")
  }
  return(HDGP_comb_g_w)
}

#Create LSOA level data where unnecessary hasnt ben converted to wide.
Agg_data <- function(rerun){
  if (rerun==FALSE & file.exists("LSOA.csv")){
    LSOA <- read.csv("LSOA.csv", header=TRUE)
    LSOA_w <- read.csv("LSOA_w.csv", header=TRUE)
  } else {
    HD_comb <- read.csv("HD_comb.csv", header=TRUE)
    LSOA_SITE <- HDGP_comb_g %>% group_by(LSOA11.x, SITETRET.x, dist_hosp, dist_gp, 
                                          GPcloser, act_dist_hosp, nearest_hosp_att, 
                                          GP_closer_att, UNNECESSARY.x) %>% 
      dplyr::summarise(Attendances=sum(Attendances.x), 
                       reg_GP_dist = median(reg_GP_dist), 
                       reg_GP_closer=sum(reg_GP_closer),
                       nearest_GP_reg = sum(nearest_GP_reg))
    
    HD_comb$LSOA_SITE <- paste(HD_comb$LSOA11, HD_comb$SITETRET, sep="_")
    HD_comb_LS <- HD_comb %>% group_by(LSOA_SITE) %>% dplyr::summarise(Attendances=sum(Attendances), Registered=sum(Registered), Unregistered=sum(Unregistered))
    
    LSOA_SITE$LSOA_SITE <- paste(LSOA_SITE$LSOA11.x, LSOA_SITE$SITETRET.x, sep="_")
    LSOA_SITE$Registered <- HD_comb_LS$Registered[match(LSOA_SITE$LSOA_SITE, HD_comb_LS$LSOA_SITE)]                                      
    LSOA_SITE$Unregistered <- HD_comb_LS$Unregistered[match(LSOA_SITE$LSOA_SITE, HD_comb_LS$LSOA_SITE)]                                      
    LSOA_SITE$prop_unreg <- LSOA_SITE$Unregistered/(LSOA_SITE$Unregistered+LSOA_SITE$Registered)
    
    
    LSOA <- LSOA_SITE %>% group_by(LSOA11.x, round(dist_hosp,2), round(dist_gp,2), 
                                   GPcloser, UNNECESSARY.x) %>% 
      dplyr::summarise(Attendances=sum(Attendances), 
                       reg_GP_dist = median(reg_GP_dist), reg_GP_closer=sum(reg_GP_closer),
                       nearest_GP_reg = sum(nearest_GP_reg), act_dist_hosp = median(act_dist_hosp),
                       nearest_hosp_att = sum(nearest_hosp_att), GP_closer_att = sum(GP_closer_att), 
                       Registered = sum(Registered), Unregistered=sum(Unregistered))
    
    LSOA$prop_unreg <- LSOA$Unregistered/(LSOA$Unregistered+LSOA$Registered)
    colnames(LSOA)[2] <- "dist_hosp"
    colnames(LSOA)[3] <- "dist_gp"
    #creating aggregate data with unnecessary in wide format
    LSOA_SITE_w <- HDGP_comb_g_w %>% group_by(LSOA11.x, SITETRET.x, dist_hosp, dist_gp, 
                                              GPcloser, act_dist_hosp, nearest_hosp_att, 
                                              GP_closer_att, Unnecessary, Necessary, prop_unnec) %>% 
      dplyr::summarise(Attendances=sum(Attendances), 
                       reg_GP_dist = median(reg_GP_dist), 
                       reg_GP_closer=sum(reg_GP_closer),
                       nearest_GP_reg = sum(nearest_GP_reg))
    
    LSOA_SITE_w$LSOA_SITE <- paste(LSOA_SITE_w$LSOA11.x, LSOA_SITE_w$SITETRET.x, sep="_")
    LSOA_SITE_w$Registered <- HD_comb_LS$Registered[match(LSOA_SITE_w$LSOA_SITE, HD_comb_LS$LSOA_SITE)]                                      
    LSOA_SITE_w$Unregistered <- HD_comb_LS$Unregistered[match(LSOA_SITE_w$LSOA_SITE, HD_comb_LS$LSOA_SITE)]                                      
    LSOA_SITE_w$prop_unreg <- LSOA_SITE_w$Unregistered/(LSOA_SITE_w$Unregistered+LSOA_SITE_w$Registered)
    
    
    LSOA_w <- LSOA_SITE_w %>% group_by(LSOA11.x, round(dist_hosp,2), round(dist_gp,2), 
                                       GPcloser) %>% 
      dplyr::summarise(Attendances=sum(Attendances), 
                       reg_GP_dist = median(reg_GP_dist), reg_GP_closer=sum(reg_GP_closer),
                       nearest_GP_reg = sum(nearest_GP_reg), act_dist_hosp = median(act_dist_hosp),
                       nearest_hosp_att = sum(nearest_hosp_att), GP_closer_att = sum(GP_closer_att),
                       Unnecessary = sum(Unnecessary), Necessary = sum(Necessary), 
                       Registered = sum(Registered), Unregistered=sum(Unregistered))
    
    LSOA_w$prop_unnec <- LSOA_w$Unnecessary/(LSOA_w$Unnecessary+LSOA_w$Necessary)
    LSOA_w$prop_unreg <- LSOA_w$Unregistered/(LSOA_w$Unregistered+LSOA_w$Registered)
    colnames(LSOA_w)[2] <- "dist_hosp"
    colnames(LSOA_w)[3] <- "dist_gp"
    
    write.csv(LSOA_w, "LSOA_w.csv")
    write.csv(LSOA, "LSOA.csv")
  }
  return(list(LSOA, LSOA_w))
}





