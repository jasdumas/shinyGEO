  ############################################################
              # Expression Profiles
              ############################################################
              ### add a contiional panel that only show this once loaded
tab.expression = tabPanel("Home", icon =icon("area-chart"),
                       conditionalPanel(condition = "!input.platform == ''",
                     #  h4("Determine if these samples are fair to compare by observing the graphical appearance & profiles values."),

		div(stye = "display: inline-block;width:100%;", 
			h4("Expression Profiles have been downloaded successfully. Please choose an analysis.")
		),
		div(stye = "display: inline-block;", 
                        actionButton("GSEButton", "Change Series/Platform", class = "btn-danger"),
			actionButton("DEbutton", "Differential Expression Analysis", class = "btn-info"),
			actionButton("KMbutton", "Survival Analysis", class = "btn-info"),
 			actionButton("exprAdd", "Save R Code", class = "btn-info"),
			#HTML("<button id='exprAdd' type='button' class='btn btn-info action-button'>Save R Code</button>"),
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

