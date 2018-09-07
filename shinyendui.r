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

fieldsMandatory <- c("myLocation")



ui <- dashboardPage(skin="blue",
                    title="Health service locator",
                    dashboardHeader(title = "Health service locator", titleWidth = 300),
                    dashboardSidebar(width = 300,
                                     tags$div(
                                       tags$blockquote("Use this app to identify your nearest health services."),
                                        tags$h4(" Enter your current location:")
                                     ),
                                     
                                     tags$hr(),
                                     
                                     textInput ("myLocation", "Enter your postcode below:"),
                                     actionButton("Runpc", "Submit postcode"),
                                     
                                     #uiOutput("SelectDir"),
                                     uiOutput("SelectMode"),
                                     
                                     ## render dynamic checkboxes
                                     radioButtons("show_serv", "Select service types:",
                                                          choices =  c("All", unique(as.character(All$flag))))
                                                      
                    ),
                    dashboardBody(
                      useShinyjs(),
                      conditionalPanel(
                        condition = "input.Runpc == 0",
                        tags$head(tags$style("#holdMap{height:90vh !important;}")),
                        google_mapOutput("holdMap")),
                      
                      conditionalPanel(
                        condition = "input.Runpc > 0",
                        tags$head(tags$style("#myMap{height:90vh !important;}")),
                        textOutput("postcodeEntered"),
                        #textOutput("transportSelected"),
                        #tags$hr("are shown on the map below, further information can be found by selecting a marker."),
                        google_mapOutput("myMap")
                      )

                    )
)

server <- function(input, output) {
  
  set_key("AIzaSyANJWDkNERnmxYO3K4wWjHTue9H3uCBlMQ")
  
  observe({
    mandatoryFilled <- 
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] !=""},
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState("Runpc", mandatoryFilled)
  })
  
  #need to make the below line reactive
  setstart <- eventReactive (input$Runpc, {
    postc <- input$myLocation
    geo_reply <- google_geocode(postc, simplify=TRUE)
    add_com <- unlist(geo_reply$results$address_components)
    validate(
      need(geo_reply$status =="OK", "Postcode entered is invalid, please enter a valid postcode.")
    )
    validate(
      need("England" %in% add_com, "Postcode entered is from outside of England, this tool is just for England area")
    )
    geo_reply <- geo_reply$results$geometry$location
    geo_reply$Home_loc <- "Home Location"
    geo_reply$colour <- "blue"
    return(geo_reply)
  })

  Service_Types <- eventReactive(input$show_serv, {
    validate(
      need(input$show_serv !="", "Please choose a service type.")
    )
    sd <- All
    if (input$show_serv != "All") {
      sd <- filter(All, flag %in% input$show_serv) 
    }
    return(sd)
  })
  
  output$myMap <- renderGoogle_map({
    stpo <- setstart()
    address <- matrix(unlist(stpo[1:2]), ncol=2, nrow=1)
    services_data <- Service_Types()
    services_data$distance <- abs(address[1]-services_data$Latitude) + abs(address[2]-services_data$Longitude)
    nearestLoc <- services_data[order(services_data$distance),][1:5,c(4,5,1,6)]
    sbbox <- make_bbox(lon=nearestLoc$Longitude, lat=nearestLoc$Latitude, f=.01)
    style <- '[{"featureType":"poi.medical", "stylers":[{"visibility":"off"}]}]'
    google_map(location=sbbox, search_box = F, zoom=7, styles=style) %>%
      add_markers(data=stpo, colour="colour", info_window = "Home_loc") %>%
      add_markers(data=nearestLoc, colour="colour", info_window = "Name")
  })
  
  output$holdMap <- renderGoogle_map({
    google_map(location= c(53.118755, -1.448822), zoom=7)
  })
  
  
  displayText <- observeEvent(input$Runpc, {
    output$postcodeEntered <- renderText(paste("The 3 closest health services from your current location, postcode:", input$myLocation, "when travelling by:", input$ModeTravel, "are shown on the map below, further information can be found by selecting a marker."))
    output$SelectMode <- renderUI({
      selectInput("ModeTravel", "Travelling by:",
          choices=c("Car", "Public transport", "Bike", "Foot"))
    })
  })
  

  



  
  #build a distance matrix in R  
  #Hosps$distance <- abs(address[1]-Hosps$Latitude) + abs(address[2]-Hosps$Longitude)
  #Hosp_llshort <- data.matrix(Hosps[order(Hosps$distance),][1:100,15:16])
  #Hosp_llshort_name <- data.matrix(Hosps[order(Hosps$distance),][1:100,c(15,16,8)])
  #Hosp_shorttest_name <- data.matrix(Hosps[order(Hosps$distance),][1:5,c(15,16,8)])
  
  #address <- data.matrix(address)
  #Hosp_shorttest <- Hosp_llshort[1:5,c(2,1)]
  #doc <- mp_matrix(Hosp_shorttest,address, region="UK")
  #mode=c("driving","transit","walking","bicycling"), arrival_time=NULL, departure_time = NULL, avoid=NULL, region = NULL, key=NULL)
  #distance5 <- mp_get_matrix(doc, value="distance_text")
  #mp_get_matrix(doc, value="distance_m")
  #duration5 <- mp_get_matrix(doc, value="duration_text")
  #mp_get_matrix(doc, value="duration_s")
  
  
#need to work out how to do the below. Creates a drop down of shortlist places to then get directions to.
    #setstart <- eventReactive (input$Runpc, {
#    output$SelectDir <- renderUI({
#      selectInput("DirectLoc", "Get directions to:",
#                  choices= as.character(droplevels(nearestLoc$OrganisationName)))
#    })
 # })
  
}

shinyApp(ui, server)