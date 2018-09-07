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
home_address <<- ""
dist_t5 <<- data.frame()

`%then%` <- shiny:::`%OR%`

ui <- dashboardPage(skin="blue",
                    title="Health service locator",
                    dashboardHeader(title = "Health service locator", titleWidth = 300),
                    dashboardSidebar(width = 300,
                                     tags$div(
                                       tags$blockquote("Use this app to identify your nearest health services."),
                                        tags$h4(" Enter your current location:")
                                     ),
                                     tags$style(HTML(".shiny-output-error-validation {
                                                     color: red;
                                                     }
                                                     ")),
                                     tags$hr(),
  
                                     textInput ("myLocation", "Enter your postcode below:"),

                                     a(id="toggleAdvanced", "Show/hide advanced options", href="#"),
                                       div(id="advancedSettings",
                                         selectInput("ModeTravel", "Travelling by:",
                                                     choices=c("driving", "transit", "bicycling", "walking")),
                                         radioButtons("show_serv", "Select service types:",
                                                      choices =  c("All", unique(as.character(All$flag))))
                                     ),
                                     actionButton("Runpc", "Submit postcode"),
                                     actionButton("Direct", "Get directions")
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
                        google_mapOutput("myMap"))
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
    shinyjs::toggleState("Direct", mandatoryFilled)
  })
  
  observe({
    shinyjs::onclick("toggleAdvanced", 
                     shinyjs::toggle(id="advancedSettings", anim=TRUE))
  })
  
  
  #need to make the below line reactive
  setstart <- eventReactive (input$Runpc, {
    postc <- input$myLocation
    geo_reply <- google_geocode(postc, simplify=TRUE)
    add_com <- unlist(geo_reply$results$address_components)
    validate(
        need(geo_reply$status =="OK", "Postcode entered is invalid, please enter a valid postcode.") %then%
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
      sd <- subset(All, flag == input$show_serv) 
    }
    return(sd)
  })
  
  #create distance and directions data
  dis_data <- eventReactive(input$Runpc, {
    stpo <- setstart()
    address <- matrix(unlist(stpo[1:2]), ncol=2, nrow=1)
    services_data <- Service_Types()
    transp_mode <- input$ModeTravel
    services_data$distance <- abs(address[1]-services_data$Latitude) + abs(address[2]-services_data$Longitude)
    serv_short <- services_data[order(services_data$distance),][1:50,c(4:5,1,6)]
    dist <- google_distance(origins=stpo[,1:2], destinations=serv_short[,1:2], mode=transp_mode)
    home_address <<- dist$origin_addresses
    dist_out <- as.data.frame(dist$rows$elements)
    dists <- cbind(dist_out, serv_short)
    dist_t5 <<- dists[order(dists$distance$value),][1:5,]
    dist_t5$info <- paste0("<b>", dist_t5$Name, "</b>","<br><b>Mode of transport:</b>", transp_mode, "<br><b>Duration:</b>", dist_t5$duration$text, 
                           "<br><b>Distance:</b>", dist_t5$distance$text)
    return(dist_t5)
  })
  
  dir_data <- eventReactive(input$Direct, {
    t5_input <- dis_data()
    stpo <- setstart()
    transp_mode <- input$ModeTravel
    lst_dir <- lapply(1:nrow(t5_input), function(x) {
      google_directions(origin=stpo[1:2], destination=t5_input[x,5:6], mode=transp_mode)
    })
    lst_res <- lapply(lst_dir, function(x){
      data.frame(polyline=direction_polyline(x))
    })
    dir_t5 <- data.table::rbindlist(lst_res)
    return(dir_t5)
  })
  
  output$myMap <- renderGoogle_map({
    t5_dist <- dis_data()
    stpo <- setstart()
    sbbox <- make_bbox(lon=t5_dist$Longitude, lat=t5_dist$Latitude, f=.01)
    style <- '[{"featureType":"poi.medical", "stylers":[{"visibility":"off"}]}]'
    google_map(location=sbbox, search_box = F, zoom=7, styles=style, event_return_type = "list") %>%
      add_markers(data=stpo, colour="colour", info_window = "Home_loc") %>%
      add_markers(data=t5_dist, colour="colour", info_window = "info", close_info_window = T)
  })
  
  output$holdMap <- renderGoogle_map({
    google_map(location= c(53.118755, -1.448822), zoom=7)
  })
  
  observeEvent(input$Direct, {
    directs <- dir_data()
    google_map_update(map_id="myMap") %>%
      add_polylines(data=directs, polyline="polyline")
  })
  
  
  displayText <- observeEvent(input$Runpc, {
    if(input$show_serv == "All"){
      Service <-  "GPs & Surgerys"
    }else{
      Service <- input$show_serv
    }
    output$postcodeEntered <- renderText(paste("The 5 closest", Service,  "from your current approximate location ", home_address, "when ", input$ModeTravel, "are shown on the map below.<br> further information can be found by selecting a marker."))
  })
  
}

shinyApp(ui, server)