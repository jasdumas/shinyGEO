
# bsModal version where data-backdrop is 'static' and data-keyboard is 'false' 
# to prevent closing from clicking outside the modal and pressing escape
# The header and footer are also removed
bsModalNoClose <-function(...) {
	b = bsModal(...)
	b[[2]]$`data-backdrop` = "static"
#	b[[2]]$`data-keyboard` = "false"
	a = b[[3]][[1]]$children[[1]]

	a[[3]][[3]] = NULL   ## remove footer (includes close button)
	a[[3]][[1]] = NULL   ## remove header (includes x button)


	a = gsub("\"modal\"", "\"none\"", a)
	b[[3]][[1]]$children[[1]] = a


	return (b)
}



gse.input = div(style = "display:inline-block; width: 60%",
            selectizeInput('GSE', label = NULL, choices = NULL, 
              options = list(placeholder = "Please enter a GSE accession number",
                          maxOptions = 100)
            )
	  )

gse.button = div(style = "display:inline-block; width: 20%",
		actionButton("submitButton", "Submit"), uiOutput("processing")
	  )

series.info = div(style = "display:inline-block; width: 20%; position: relative; left:-40px;",
     		conditionalPanel(condition = "output.displayPlatform=='TRUE'",      
     			actionButton("dataSeries", "Gene Series Information"),
  			shinyBS::bsModal("dataSeriesDisplay", "Gene Series", "dataSeries", size = "large",
                                        verbatimTextOutput("dataInputPrint"))
 		)
	  )
navbar.header = list(

    ############################################################
    # GSE selection
    ############################################################
    tags$style(type = "text/css", ".well{color: gray; background-color: black}"),
              
   
   DT::dataTableOutput("irisData"),

   bsModalNoClose("welcomeModal", NULL, "GSEButton", size = "large",
	#h4("Dataset selection", style = "color: darkred;"),
    bsAlert("alert0"), 
    bsAlert("alert1"),

    fluidRow(
	column(7,
	 gse.input, gse.button#, series.info
	),   

    ## Hidden text box to indicate whether platform has been selected ##
#    conditionalPanel(condition = "input.GSE == 'GSE13'",
#                    textOutput("displayPlatform")
#     ), 

	column(5, 
        	conditionalPanel(condition = "output.displayPlatform=='TRUE'",     

 	  	  div(style = "display:inline-block; width: 60%",
            		selectizeInput('platform', label = NULL, choices = NULL, 
              			options = list(placeholder = "Please select a platform",
                  		maxOptions = 10)
            		)
		  ),
 	  	  div(style = "display:inline-block; width: 30%",
       			actionButton("submitPlatform", "Go!")
		  )
		)
	)
        
   
       #actionButton("PlatformInfoButton", "Platform Information"),

       # shinyBS::bsModal("PlatformLinks", HTML("Available Platforms<br>(More Information)"), "PlatformInfoButton", size = "small",
       #   uiOutput("PlatformLinks")
       # )
    ),

    conditionalPanel(condition="$('html').hasClass('shiny-busy')",  
        div(style = "position:center; width:100%; height:100; text-align:center",
            img(src="PleaseWait.gif", style = "width:50%")
        )
    )                
 
   ),

conditionalPanel(condition = "output.displayPlatform=='adfd'",      
 div(style = "display:inline-block; width: 55%",
            selectizeInput('GSE2', label = NULL, choices = NULL, 
              options = list(placeholder = "Please enter a GSE accession number",
                          maxOptions = 100)
            )
 )
),

    bsAlert("alert2")

)


