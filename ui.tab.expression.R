  ############################################################
              # Expression Profiles
              ############################################################
              ### add a contiional panel that only show this once loaded
tab.expression = tabPanel("Expression Profiles", icon =icon("area-chart"),
                       conditionalPanel(condition = "!input.platform == ''",
                       h4("Determine if these samples are fair to compare by observing the graphical appearance & profiles values."), 
                       radioButtons("radio", label = "Select a method of log transformation to apply to the data", 
                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3), 
                                    selected = 1, inline = TRUE), actionButton("exprAdd", "Save R Code"),
                       plotOutput("exProfiles") 
                    ) 
             
             )

