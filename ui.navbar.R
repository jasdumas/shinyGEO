navbar.header = list(

    ############################################################
    # GSE selection
    ############################################################
    tags$style(type = "text/css", ".well{color: gray; background-color: black}"),
              
    
    bsAlert("alert1"),
    div(style = "display:inline-block; width: 30%",
        # uiOutput("GSE")
        selectizeInput('GSE', label = NULL, choices = NULL, 
            options = list(placeholder = "Please enter a GSE accession number",
                          maxOptions = 1000)
        )                    
        # textInput("GSE", 
        # HTML("<span style = \"color:red;font-weight:bold\">GEO Accession Number </span>"), "")
    ),
    
    div(style = "display:inline-block; width: 30%",
        actionButton("submitButton", "Submit")
    ),
    
    ## Hidden text box to indicate whether platform has been selected ##
#    conditionalPanel(condition = "input.GSE == 'GSE13'",
#                    textOutput("displayPlatform")
#     ), 
    
    div(style = "display:inline-block; width: 30%",
        conditionalPanel(condition = "output.displayPlatform=='TRUE'",      
            div(style = "display:inline-block; width: 83%",
            #uiOutput('platform', style = "display:inline-block; width:50%"),
            selectizeInput('platform', label = NULL, choices = NULL, 
              options = list(placeholder = "Please select a platform",
                  maxOptions = 10)
            )
        ),
        
        div(style = "display:inline-block; width: 15%",
            actionButton("PlatformInfoButton", "Platform Information")
        ),
        shinyBS::bsModal("PlatformLinks", HTML("Available Platforms<br>(More Information)"), "PlatformInfoButton", size = "small",
          uiOutput("PlatformLinks")
        )
      )
    ), 
    bsAlert("alert2"),
    #uiOutput("test"),
    #uiOutput("test2"),
    hr(), # new ecsu.css file with defaults
    #div(style = "position: relative; top: -20px", HTML("<hr style = \"background-color: black; height:3px;\">")),
                
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",  
        div(style = "position:center; width:100%; height:100; text-align:center",
            img(src="PleaseWait.gif", style = "width:50%")
        )
    )                
)


