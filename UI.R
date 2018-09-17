ui <- dashboardPage(skin="blue",
                    title="Health service locator",
                    #generate the dashboard header section for the application
                    dashboardHeader(title = "Health service locator", titleWidth = 300),
                    #create the side panel to hold the user interactivity
                    dashboardSidebar(width = 300,
                                     tags$div(
                                       tags$blockquote("Use this app to identify your nearest health services."),
                                       tags$h4(" Enter your current location:")
                                     ),
                                     #format the presentation of the error messages, within the server function
                                     #make them appear red so the user recognises them as an error message
                                     tags$style(HTML(".shiny-output-error-validation {
                                                     color: red;
                                                     }
                                                     ")),
                                     tags$hr(),
                                     #create text input field for the postcode entry
                                     textInput ("myLocation", "Enter your postcode below:"),
                                     #create ability to show and hide the advanced options
                                     #linked to the toggle function in server.
                                     a(id="toggleAdvanced", "Show/hide advanced options", href="#"),
                                     div(id="advancedSettings",
                                         selectInput("ModeTravel", "Travelling by:",
                                                     choices=c("driving", "transit", "bicycling", "walking")),
                                         radioButtons("show_serv", "Select service types:",
                                                      choices =  c("All", unique(as.character(All$flag))))
                                     ),
                                     #create the buttons to trigger all reactive events within the server.
                                     actionButton("Runpc", "Submit postcode"),
                                     actionButton("Direct", "Get directions")
                                     ),
                    #create the main body as a dashboard element.
                    dashboardBody(
                      useShinyjs(),
                      #create conditional panels to enable the holding map to be displayed until the submit postcode button is triggered
                      #each click of the button increases the number assigned to it
                      conditionalPanel(
                        condition = "input.Runpc == 0",
                        tags$head(tags$style("#holdMap{height:90vh !important;}")),
                        google_mapOutput("holdMap")),
                      #load the main map once the button value is greater than 1 following the button being pressed
                      conditionalPanel(
                        condition = "input.Runpc > 0",
                        tags$head(tags$style("#myMap{height:90vh !important;}")),
                        textOutput("postcodeEntered"),
                        google_mapOutput("myMap"))
                    )
)
