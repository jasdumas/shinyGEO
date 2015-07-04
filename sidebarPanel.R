sidebarPanel(
  
  div(style = "display:inline-block; width: 60%",
    textInput("GSE", 
          HTML("<span style = \"color:red;font-weight:bold\">GEO Accession Number </span>"), "")
  ),
  div(style = "display:inline-block; width: 30%",
    actionButton("submitButton", "Submit")
  ),
    ## Hidden text box to indicate whether platform has been selected ##
    conditionalPanel(condition = "input.GSE == 'HEY'",
        textOutput("displayPlatform")
    ),
    
    #######################################################################
    ### ONLY DISPLAY REST AFTER PLATFORM IS SELECTED 
    #######################################################################
    conditionalPanel(condition = "output.displayPlatform == 'TRUE'",
        uiOutput('platform'),  

        conditionalPanel(condition = "input.tabs == 'Differential Expression Analysis'",
          #uiOutput('selectGenes'),
          #uiOutput('selectProbes'),
          tags$div(HTML("<hr style = \"background-color: red; height:4px\">")),                  
          uiOutput('selectedColumn'),
          uiOutput('selectedGroups') 
      ), # end Conditional Panel
      
      conditionalPanel(condition = "input.tabs == 'Survival Analysis'",
                       tags$div(HTML("<hr style = \"background-color: red; height:4px\">")),   
                       uiOutput("survTime"),
                       uiOutput("survOutcome"),
                       uiOutput("survX"), 
                       actionButton("parseButton", "Parse Data"), 
                       
                       bsModal("parseModal", "Selected Survival Analysis Parameters", "parseButton", size = "large",
                               actionButton("parseEnter", label = "Submit"))
                       #uiOutput('selectedColumn'),
                       #uiOutput('selectedGroups') 
      ) # end Conditional Panel
      
    ) #end Analyze Conditional Panel

) ## end sidebarPanel 