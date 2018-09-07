#below package contains functions to do the initial data cleaning and preprocessing
if (!require(plotly)) install.packages('plotly')
library(plotly)
if (!require(plyr)) install.packages('plyr')
library(plyr)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)


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

#below packages contain functions to undertake the analysis and produce the visualisations
if (!require(leaflet)) install.packages('leaflet')
library(leaflet)

#Analysis for latest version of data including LSOA codes
setwd("~/MSc project/data")

if (file.exists("HD_home.csv")){
  HD_home <- read.csv("HD_home.csv", header=TRUE)
} else {
  HESdist1 <- read.csv("DISTANCE_171814_08.csv", header=TRUE, sep=",")
  summary(HESdist1)
  str(HESdist1)
  
  HESdist1$Arr_Month <- as.factor(HESdist1$Arr_Month)
  HESdist1$SITEDIST_FLAG <- as.factor(HESdist1$SITEDIST_FLAG)
  HESdist1$AEATTENDDISP <- as.factor(HESdist1$AEATTENDDISP)
  HESdist1$INVEST2_01 <- as.factor(HESdist1$INVEST2_01)
  HESdist1$TREAT3_01 <- as.factor(HESdist1$TREAT3_01)
  HESdist1$LSOA11 <- as.factor(HESdist1$LSOA11)
  HESdist1$AEDEPTTYPE <- as.factor(HESdist1$AEDEPTTYPE)
  HESdist1$AEARRIVALMODE <- as.factor(HESdist1$AEARRIVALMODE)
  HESdist1$AEINCLOCTYPE <- as.factor(HESdist1$AEINCLOCTYPE)
  HESdist1$UNNECESSARY <- as.factor(HESdist1$UNNECESSARY)
  levels(HESdist1$UNNECESSARY) <- c("Necessary", "Unnecessary")
  HESdis1 <- HESdist1[!is.na(HESdist1$SITEDIST),]
  HESdis1<- HESdis1[!grepl("^W", HESdis1$LSOA11),]
  
  #split SITEDIST into a bucket variable
  bins = c(0,1,2,3,4,5,6,7,8,9,10,15,30,50,100,250,500,700)
  bin_names = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-15","15-30","30-50","50-100","100-250","250-500","500-700")
  HESdis1$DISTbucket <- cut(HESdis1$SITEDIST, breaks=bins, labels=bin_names)
  DIST1_bin <- HESdis1 %>% group_by(DISTbucket) %>% dplyr::summarize(count=n())
  
  #create clean usable subset of data, only keep items where incident happenned at home and site location was used to calculate SITEDIST
  HD_home <- subset(HESdis1, AEINCLOCTYPE==10)
  HD_home <- subset(HD_home, SITEDIST_FLAG==5)
  HD_home$Attendances <- as.numeric(HD_home$Attendances)
  HD_home <- droplevels(HD_home)
  write.csv(HD_home, "HD_home.csv")
}

if (file.exists("LSOA_loc.csv")){
  LSOA_ll <- read.csv("LSOA_loc.csv", header=TRUE)
} else {
  ##read in LSOA location data
  #code to convert east/nort to long/lat taken from http://www.alex-singleton.com/R-Tutorial-Materials/7-converting-coordinates.pdf
  
  # Variables for holding the coordinate system types (see:
  # http://www.epsg.org/ for details)
  ukgrid = "+init=epsg:27700"
  latlong = "+init=epsg:4326"
  
  LSOA <- read.csv("england_lsoa_2011_centroids.csv", header=TRUE)
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

if (!exists("all_services")){
  all_services <- read.csv("20_June_2018_CQC_directory.csv", skip=4, header = TRUE, sep=',')
}

if (file.exists("Hosp_services.csv")){
  Hosp_services <- read.csv("Hosp_services.csv", header=TRUE)
} else {
  #create the hospitals data
  Hosp_services <- all_services[grep("*Hospital*", all_services$Service.types),]
  Hosp_services$HospID <- 1:nrow(Hosp_services)
  Hosp_services <- Hosp_services[,-c(5,6)]
  #there is no unique identifier in the CQC data to match on, so explore other options
  #given the number of rows we have we could create the long/lat data from the googleway geocoder
  #both options were explored, the batch geodcoder although very quick didnt identify 21 of the postcodes
  key = "insert my key"
  res <- future_apply(Hosp_services,1, function(x){
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

if (file.exists("GP_services.csv")){
  GP_services <- read.csv("GP_services.csv", header=TRUE)
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

if (file.exists("min_hospDist.csv") & file.exists("min_GPDist.csv")){
  min_HospDist <- read.csv("min_hospDist.csv")
  min_GPDist <- read.csv("min_GPDist.csv")
  LS <- read.csv("LS.csv")
}else{
  ###creating the distances data using the location data for LSOAs, hospitals and GPS.
  #create subset for testing distance algorithms
  LS_test <- LSOA_loc[grepl("Middlesbrough", LSOA_loc[["LSOA.name"]]),]
  GP_test <- GP_services[grepl("Middlesbrough", GP_services[["Local.Authority"]]),]
  H_test <- Hosp_services[grepl("Middlesbrough", Hosp_services[["Local.Authority"]]),]
  
  #Define Projections
  wgs84<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  utm10n<-CRS("+proj=utm +zone=10 +ellps=GRS80 +datum=NAD83 +units=m +no_defs +towgs84=0,0,0")
  
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
  
  #calculating distance
  #Hosp
  disth <- gDistance(LS_proj,H_proj,byid=T)
  min_hospDist <- future_apply(disth,2,min)
  write.csv(min_hospDist, "min_hospDist.csv")
  
  #GP
  distgp <- gDistance(LS_proj,GP_proj,byid=T)
  min_GPDist <- future_apply(distgp,2,min)
  write.csv(min_GPDist, "min_GPDist.csv")
  
  #create distance columns in original DS
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
  
  }

if (file.exists("HES_comb.csv")){
  GP_services <- read.csv("HES_comb.csv", header=TRUE)
} else {
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
  HD_unmatch <- HD_home[is.na(HD_home$SITE_lat),15]
  #By extracting the unmatched sites by postcode there are only 40 unique postcodes to geocode
  HD_unmatch <- as.data.frame(unique(HD_unmatch))
  colnames(HD_unmatch) <- c("Postcode")
  
  #for the sites not found create long/lat data using googleway geocoder method used above.
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
  
  ##Combining the different data sets
  HES_comb <- merge(x=HD_home, y=LS, by.x="LSOA11", by.y="LSOA.code", all.x=TRUE)
  
  #create clean usable subset of data
  HES_comb <- HES_comb[,-c(5:11)]
  colnames(HES_comb)[11] <- "LSOA_lon"
  colnames(HES_comb)[12] <- "LSOA_lat"
  
  #add distance from the LSOA to the hospital attended on to dataset
  HD_SL <- aggregate(Attendances~ LSOA11 + SITETRET +LSOA_lon + LSOA_lat + SITE_lon + SITE_lat, HES_comb, sum)
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
  
  write.csv(HES_comb, "HES_comb.csv")
}

##### code above all reviewed ####

###code below needs finalising to complete the analysis ###


#subsetting the combined data without keeping the location data
HD1 <- HESd1 %>% group_by(Arr_Month, SITETRET, LSOA11, UNNECESSARY, SITEDIST, Attendances) %>% dplyr::summarize(count=n())
HD2 <- HD1 %>% group_by(SITETRET, LSOA11, UNNECESSARY, SITEDIST, Attendances) %>% dplyr::summarize(sum(count))

#subsetting the data keeping the location data
HDa <- HESd1 %>% group_by(Arr_Month, SITETRET, LSOA11, UNNECESSARY, SITEDIST, Attendances, near_gp_code, near_hosp_code, GPcloser) %>% dplyr::summarize(count=n())

#subsetting the data into distance buckets, keeping LSOA, hosp, nearestGP
Comb_buk <- HESd1 %>% group_by(DISTbucket, SITETRET, Arr_Month, UNNECESSARY, LSOA11, near_gp_code, GPcloser, near_hosp_code) %>% dplyr::summarize(n())

#split SITEDIST into a bucket variable
bins = c(0,1,2,3,4,5,6,7,8,9,10,15,30,50,100,250,500,700)
bin_names = c("0-1","1-2","2-3","3-4","4-5","5-6","6-7","7-8","8-9","9-10","10-15","15-30","30-50","50-100","100-250","250-500","500-700")
HESd1$DISTbucket <- cut(HESd1$SITEDIST, breaks=bins, labels=bin_names)
DIST1_bin <- HESd1 %>% group_by(DISTbucket) %>% dplyr::summarize(count=n())

HESd1$count <- 1
hist_dist1 <- ggplot(HESd1) + geom_histogram(aes(x=SITEDIST, weight=count), breaks=bins, fill="steelblue")+ theme_classic() + labs(x="Distance travelled in KM", y="Frequency")
ggsave("hist_dist.png")
Dist1_buckets <- plot_ly(DIST1_bin, x=~DISTbucket, y=~count, type="bar") %>% layout(xaxis=list(title="Distance travelled in km"), yaxis=list(title="frequency"))
htmlwidgets::saveWidget(Dist1_buckets, "DIST_buckets.html")

#Number of sites visited by people in an LSOA
LSOA_HES <- HESd1 %>% group_by(LSOA11, UNNECESSARY) %>% summarize(n_distinct(SITETRET))
LSOA_SITE <- table(LSOA_HES$`n_distinct(SITETRET)`)
plot(LSOA_SITE, xlab="The number of different hospital sites attended by patients in the LSOA", ylab="Number of LSOAs", main="The number of different hospital sites \npatients from the same LSOA visit.", col="steelblue3")
dev.copy(png,"Number of different sites per LSOAs.png")
dev.off()  

#chart to compare how far people travel split out by the unnecessary classifier.
Un_dist_comp <- ggplot(HESd1, aes(SITEDIST)) + geom_histogram(breaks=bins, fill="steelblue") + facet_wrap(~UNNECESSARY) + theme_classic()
ggsave("hist_comp_unnecessary.png")

GP_dist_comp <- ggplot(HESd1, aes(SITEDIST)) + geom_histogram(breaks=bins, fill="steelblue") + facet_wrap(~GPcloser) + theme_classic()
ggsave("hist_comp_GPcloser.png")

#look at whether there is a relationship between % attendances which are unecessary and distance from LSOA.
#create smaller dataset to explore this
HESd1nec <-aggregate(Attendances~ UNNECESSARY + LSOA11 + SITETRET + GPcloser + dist_hosp + dist_gp,HESd1,sum)
HESd1u <- HESd1nec[HESd1nec$UNNECESSARY == "Unnecessary",]
HESd1prop <-aggregate(Attendances~ LSOA11 + SITETRET + GPcloser + dist_hosp + dist_gp,HESd1,sum)
HESd1uprop <- merge(HESd1prop, HESd1u[,c("LSOA11", "SITETRET", "Attendances")], by=c("LSOA11", "SITETRET"), all.x=TRUE)
HESd1uprop$Attendances.y[is.na(HESd1uprop$Attendances.y)] <- 0
HESd1uprop$near_hosp_code <- HESd1$near_hosp_code[match(HESd1uprop$LSOA11, HESd1$LSOA11)]
HESd1uprop$near_gp_code <- HESd1$near_gp_code[match(HESd1uprop$LSOA11, HESd1$LSOA11)]


HESfin <- HESd1uprop %>% left_join(HESdis, by=c("LSOA11","SITETRET"))
colnames(HESfin)[6] <- "Total_atts"
colnames(HESfin)[7] <- "Unnecessary_atts"
HESfin$prop_un <- HESfin$Unnecessary_atts/HESfin$Total_atts
ggplot(HESfin, aes(x=act_dist_hosp , y=prop_un)) + geom_point(colour="steelblue") + theme_classic()
ggplot(HESfin, aes(x=dist_hosp , y=dist_gp)) + geom_point(colour="steelblue") + theme_classic() + labs(x="Distance from nearest hospital in meters", y="Distance from nearest GP in meters")


fit <- glm(prop_un~act_dist_hosp+GPcloser+dist_hosp, weights=Total_atts, HESfin, family="binomial")
