navbar.header = list(

    ############################################################
    # GSE selection
    ############################################################
    tags$style(type = "text/css", ".well{color: gray; background-color: black}"),
              
    
    bsAlert("alert1"),
    fluidRow(
	column(6,

	  div(style = "display:inline-block; width: 55%",
            selectizeInput('GSE', label = NULL, choices = NULL, 
              options = list(placeholder = "Please enter a GSE accession number",
                          maxOptions = 100)
            )
	  ),
	
                  
	  div(style = "display:inline-block; width: 20%",
		actionButton("submitButton", "Submit")
	  ),
	  div(style = "display:inline-block; width: 20%; position: relative; left:-40px;",
     		conditionalPanel(condition = "output.displayPlatform=='TRUE'",      
     			actionButton("dataSeries", "Gene Series Information"),
 			shinyBS::bsModal("dataSeriesDisplay", "Gene Series", "dataSeries", size = "large",
                                        verbatimTextOutput("dataInputPrint"))
		)
	  )
	),   
 
    ## Hidden text box to indicate whether platform has been selected ##
#    conditionalPanel(condition = "input.GSE == 'GSE13'",
#                    textOutput("displayPlatform")
#     ), 

	column(1), 
 
	column(3, 
        	conditionalPanel(condition = "output.displayPlatform=='TRUE'",      
            	#uiOutput('platform', style = "display:inline-block; width:50%"),
            		selectizeInput('platform', label = NULL, choices = NULL, 
              			options = list(placeholder = "Please select a platform",
                  		maxOptions = 10)
            		)
		)
	)
        
   
       #actionButton("PlatformInfoButton", "Platform Information"),

       # shinyBS::bsModal("PlatformLinks", HTML("Available Platforms<br>(More Information)"), "PlatformInfoButton", size = "small",
       #   uiOutput("PlatformLinks")
       # )
    ),
 
    bsAlert("alert2"),
    #uiOutput("test"),
    #uiOutput("test2"),
    shiny::hr(), # new ecsu.css file with defaults
    #div(style = "position: relative; top: -20px", HTML("<hr style = \"background-color: black; height:3px;\">")),
                
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",  
        div(style = "position:center; width:100%; height:100; text-align:center",
            img(src="PleaseWait.gif", style = "width:50%")
        )
    )                
)


