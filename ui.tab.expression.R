  ############################################################
              # Expression Profiles
              ############################################################
              ### add a contiional panel that only show this once loaded
tab.expression = tabPanel("Expression Profiles", icon =icon("area-chart"),
                       conditionalPanel(condition = "!input.platform == ''",
                     #  h4("Determine if these samples are fair to compare by observing the graphical appearance & profiles values."),

		div(stye = "display: inline-block;width:100%;", 
			h4("Expression Profiles have been downloaded successfully. Please choose an analysis.")
		),
		div(stye = "display: inline-block;", 
			actionButton("DEbutton", "Differential Expression Analysis"),
			actionButton("KMbutton", "Survival Analysis"),
 			actionButton("exprAdd", "Save R Code"),
			a(id = "normLink", "(options)")
		),
                       plotOutput("exProfiles"), 
		bsModal("normalalizationModal", "Normalization Options", "normLink", size = "small",
                       radioButtons("radio", label = "Select a method of log transformation to apply to the data", 
                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3), 
                                    selected = 1, inline = TRUE)
		)
                    ) 
             
             )

