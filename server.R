source("function_library.R")


server <- function(input, output) {

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
  

  dis_data <- eventReactive(input$Runpc, {
    key <- set_up_shiny()
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
      google_directions(origin=stpo[1:2], destination=t5_input[x,4:5], mode=transp_mode)
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
      clear_polylines() %>%
      add_polylines(data=directs, polyline="polyline")
  })
  
  
  displayText <- observeEvent(input$Runpc, {
    if(input$show_serv == "All"){
      Service <-  "GPs & Hospitals"
    }else{
      Service <- input$show_serv
    }
    output$postcodeEntered <- renderText(paste("The 5 closest", Service,  "from your current approximate location ", home_address, "when ", input$ModeTravel, "are shown on the map below. Further information can be found by selecting a marker."))
  })
  
}
