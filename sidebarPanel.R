sidebarPanel(
    textInput("GSE", 
          HTML("<span style = \"color:red;font-weight:bold\">Enter GEO Accession Number </span>"), "") ,
    actionButton("submitButton", "Submit"),
    
    ## Hidden text box to indicate whether platform has been selected ##
    conditionalPanel(condition = "input.GSE == 'HEY'",
        textOutput("displayPlatform")
    ),
    
    #######################################################################
    ### ONLY DISPLAY REST AFTER PLATFORM IS SELECTED 
    #######################################################################
    conditionalPanel(condition = "output.displayPlatform == 'TRUE'",
        uiOutput('platform'),  
        tags$div(HTML("<hr style = \"background-color: blue; height:5px\">")), 
                 
        uiOutput('selectGenes'),
        uiOutput('selectProbes'),
                 
        uiOutput('selectedColumn'),
        uiOutput('selectedGroups') 
    ) #end Analyze Conditional Panel

) ## end sidebarPanel 