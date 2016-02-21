##########################################################################
# Analyses Tab - This file contains 4 parts:
#   tab.DE.analysis - tab panel for Differential Expression analysis
#   tab.survival.analysis - tab panel for survival analysis
###########################################################################

####################
# DE tab
####################
tab.DE.analysis = tabItem(tabName = "DifferentialExpressionAnalysis",
    bsAlert("alertDE"), 

conditionalPanel(condition = "input.selectGenes!=''",

   uiOutput("selectedColumn", container = div, style = "display:inline-block; width: 15%"),
    uiOutput("selectedGroups", container = div, style = "display:inline-block; width: 70%"),
    br(),                                                
                                                   
    actionButton("mergeGroupsButton", "Merge Groups"), 
    actionButton("formatDEButton", "Format Graph"),
    HTML("<button id='DEadd' type='button' class='btn btn-info action-button'>Save R Code</button>"),                                   

    bsModal("addCodeDEModal", "R Code Added", "DEadd", size = "small",
		bsAlert("addCodeDEAlert")
    ),
             
    plotOutput("plot"),
    formatBSModal("MergeGroupsModal", "Merge Groups", "mergeGroupsButton", size = "large", applyID = "applyMergeGroups",

        bsAlert("mergeGroupsAlert"),
        textInput("newColumnForMerge", "New Column Name", "MergeColumn"),
    fluidRow(
      column(6, uiOutput("mergeGroup1")), 
      column(6, textInput("group1Label", "New Group Name"))
    ),
    fluidRow(
      column(6, uiOutput("mergeGroup2")), 
      column(6, textInput("group2Label", "New Group Name"))
    ),
    fluidRow(
      column(6, uiOutput("mergeGroup3")), 
      column(6, textInput("group3Label", "New Group Name"))
    )
   ),
    formatBSModal("Format", "Format", "formatDEButton", applyID = "applyFormatDE", size = "large",
        htmlOutput("formatDE")#, hr(),               
    )
)
)


####################
# Survival tab
####################
tab.survival.analysis = tabItem("SurvivalAnalysis", 
        summaryBSModal("summaryBSModal","Clinical Data Summary",""),
        bsButton("autoAnalysis","Select Time/Outcome", style="success",disabled = TRUE),
        actionButton("formatDEButton2", "Format Graph",disabled=TRUE), # add on
        HTML("<button id='Survadd' type='button' class='btn btn-info action-button'>Save R Code</button>"),                                   
    formatBSModal("Format2", "Format", "formatDEButton2", applyID = "applyFormatDE2", size = "large", 
	hr = radioButtons("hr.format", label = "HR (expression)",
                    choices = list("high/low", "low/high"),
                    inline = TRUE),
        textInput("km.xlab", "x-axis label: ", "Time"),
        textInput("km.ylab", "y-axis label: ", "Survival"),
	htmlOutput("formatDE2")
    ),    
    bsModal("addCodeSurvModal", "R Code Added", "Survadd", size = "small",
		bsAlert("addCodeSurvAlert")
        ),
         plotOutput("kmSurvival")
) # end tabPanel: Survival Analysis

