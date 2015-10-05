
###########################################################################
# Analyses Tab - This file contains 4 parts:
#   formatBSModal - used to display BS Modal with Save Changes button
#   analyses.common - common component (gene/probe) for DE and survival analyses
#   tab.DE.analysis - tab panel for Differential Expression analysis
#   tab.survival.analysis - tab panel for survival analysis
###########################################################################


##########################################################################
## formatBSModal - equivalent to shinyBS::bsModal except we add a Save 
## Changes button with id = applyID next to the Close button
## shinyBSDep is not found, so last line is commented out. This does
## not appear to have an effect
##########################################################################
formatBSModal<-function (id, title, trigger, applyID, ..., size) 
{
  if (!missing(size)) {
    if (size == "large") {
      size = "modal-lg"
    }
    else if (size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  }
  else {
    size <- "modal-dialog"
  }
  bsTag <- shiny::tags$div(class = "modal sbs-modal fade", 
              id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
              shiny::tags$div(class = size, 
                shiny::tags$div(class = "modal-content", 
                  shiny::tags$div(class = "modal-header", 
                    shiny::tags$button(type = "button",  class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
                      shiny::tags$h4(class = "modal-title", title)
                  ), 
                  shiny::tags$div(class = "modal-body", list(...)), 
                  shiny::tags$div(class = "modal-footer", 
                    shiny::tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Close"),
                      actionButton(applyID, "Save Changes", class = "btn-primary")    
                  )      
                )
              )
  )
  #htmltools::attachDependencies(bsTag, shinyBSDep)
}


####################
# DE tab
####################
tab.DE.analysis = tabPanel("Differential Expression Analysis", icon = icon("flask"), 
    uiOutput("selectedColumn", container = div, style = "display:inline-block; width: 20%"),
    uiOutput("selectedGroups", container = div, style = "display:inline-block; width: 75%"),
                                                    
    bsCollapse(id = "DiffExpDataTable",
        bsCollapsePanel("View clinical data table (click on a column to select)",
            DT::dataTableOutput("clinicalDataForDiffExp")
        )
    ),
                                                    
    actionButton("formatDEButton", "Format Graph"),
    #actionButton("DEadd", "Save R Code"),
    HTML("<button id='DEadd' type='button' class='btn btn-info action-button'>Save R Code</button>"),                                                
    uiOutput("selectGroupsMessage"), 
    plotOutput("plot"),
    formatBSModal("Format", "Format", "formatDEButton", applyID = "applyFormatDE", size = "large",
        htmlOutput("formatDE")#, hr(),               
        #actionButton("applyFormatDE", "Apply Changes")
    )
)


####################
# Survival tab
####################
tab.survival.analysis = tabPanel("Survival Analysis", icon = icon("life-ring"), 
    bsCollapse(id = "SurvDataTable", open = "SurvDataTable",
        bsCollapsePanel("Select Time and Outcome From Clinical Data",
            div(style = "display:inline-block; width:30%",
              uiOutput('survTime')
            ),
            div(style = "display:inline-block; width:30%",
                uiOutput('survOutcome')
            ),
            actionButton("parseButton", "Format Columns"), 
                DT::dataTableOutput("clinicalDataForSurvival")
            )
    ),
         
    shiny::hr(),
    #tags$div(HTML("<hr style = \"background-color: red; height:4px\">")), 
    uiOutput("SurvMessage"),
    actionButton("formatDEButton2", "Format Graph"), # add on
    formatBSModal("Format2", "Format", "formatDEButton2", applyID = "applyFormatDE2", size = "large",
    htmlOutput("formatDE2")),
        #actionButton("Survadd", "Save R Code"),
        HTML("<button id='Survadd' type='button' class='btn btn-info action-button'>Save R Code</button>"),
        shinyBS::bsModal("parseModal", "Selected Survival Analysis Parameters", "parseButton", size = "large",
            fluidRow(
                column(4, textInput("survfind", label = "Find", value = "")),
                column(4, textInput("survreplace", label = "Replace", value = "")),
                column(4, actionButton("parseEnter", label = "Submit"), 
                actionButton("undo", label="Revert Changes")) # not valid as a revert yet
            ),
            DT::dataTableOutput("selectedCols")
            #print("in parseModal window...")
        ),
         plotOutput("kmSurvival")
) # end tabPanel: Survival Analysis




analyses.common = conditionalPanel(condition = "input.tabs == 'Differential Expression Analysis' | input.tabs == 'Survival Analysis'", 
    bsCollapse(id = "GeneSelection", open = "Select a Gene and Probe",
        bsCollapsePanel("Select a Gene and Probe",
            div(style = "display:inline-block; width:30%",
                uiOutput('selectGenes')
            ),
            div(style = "display:inline-block; width:30%",
                uiOutput('selectProbes')
            )
        )
    )
) 

#####################################################################
# The complete analyses tab
#####################################################################
tab.analyses =  navbarMenu("Analyses", icon = icon("bar-chart-o"),
                        analyses.common,
                         tab.DE.analysis,
                         tab.survival.analysis    
)


