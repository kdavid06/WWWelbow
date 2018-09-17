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
