  ############################################################
              # Expression Profiles
              ############################################################
              ### add a contional panel that only show this once loaded
tab.expression = tabItem(tabName = "Home",
		 conditionalPanel(condition = "output.sidebarDisplay=='ALL'",
                     #  h4("Determine if these samples are fair to compare by observing the graphical appearance & profiles values."),
		div(stye = "display: inline-block;", 
		#      actionButton("GSEButton", "Change Series/Platform", class = "btn-danger"),
		#	actionButton("DEbutton", "Differential Expression Analysis", class = "btn-info"),
		#	actionButton("KMbutton", "Survival Analysis", class = "btn-info"),
 		#	actionButton("exprAdd", "Save R Code", class = "btn-info"),
		#	#HTML("<button id='exprAdd' type='button' class='btn btn-info action-button'>Save R Code</button>"),
			uiOutput("summary"),
			a(id = "normLink", "(View expression profiles)",style="cursor:pointer"),
  			a(id = "platLink", "(View platform data)",style="cursor:pointer") 
		)

		),
		bsModal("normalalizationModal", "Expression Profiles", "normLink", size = "large",
                       radioButtons("radio", label = "Select a method of log transformation to apply to the data", 
                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3), 
                                    selected = 1, inline = TRUE),
			bsAlert("expAlert"),
                       plotOutput("exProfiles") 
		),

		bsModal("platformModal", "Platform annotation", "platLink", size = "large",
			DT::dataTableOutput("platformData")
                ), 
		p(), p(), HTML("<hr style = \"width:30px\">"),
		DT::dataTableOutput("clinicalDataSummarySummary")
             
             )

