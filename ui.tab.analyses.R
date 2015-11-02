
###########################################################################
# Analyses Tab - This file contains 4 parts:
#   formatBSModal - used to display BS Modal with Save Changes button
#   analyses.common - common component (gene/probe) for DE and survival analyses
#   tab.DE.analysis - tab panel for Differential Expression analysis
#   tab.survival.analysis - tab panel for survival analysis
###########################################################################


##########################################################################
## MODAL SECTION
##
## formatBSModal - equivalent to shinyBS::bsModal except we add a Save 
## Changes button with id = applyID next to the Close button
## shinyBSDep is not found, so last line is commented out. This does
## not appear to have an effect
##
## genBSModal - equivalent to shinyBS::bsModal except custom buttons for 
## generation of survival analysis
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
genBSModal<-function (id, title, trigger, ..., size) 
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
               bsButton("manuBtn", "Manual Selection", icon = NULL, style = "info",
                        size = "default", type = "action", block = FALSE, disabled = FALSE,
                        value = FALSE),
 
               bsButton("genBtn", "Generate Analysis", icon = NULL, style = "success",
                        size = "default", type = "action", block = FALSE, disabled = FALSE,
                        value = FALSE),
               shiny::tags$h4(class = "modal-title", title)
), 
shiny::tags$div(class = "modal-body",
              fluidRow(
                 column(12,
                        tags$h4(class="intro","shinyGEO has automatically detected and formatted columns within your data for you. Please confirm these are correct and then generate the analysis."),
                        bsAlert("warningAlert")
                 )
               ),
               tags$hr(class="hr"),
               fluidRow(
                 column(7,
                        tags$h4(class="col-time-head","Time Column Selection"),
                        selectizeInput('autoColumn.time','Column Time',choices=NULL),
                        tags$br(),
                        dataTableOutput("timetable")
                 ),
                 column(1,""),
                 column(3,
                        tags$h4(class="col-time-head","Outcome Column Selection"),
                        selectizeInput('autoColumn.outcome','Column Outcome',choices=NULL),
                        tags$br(),
                        tags$div(class="columnSelect",
                                 selectizeInput('columnEvent1',label ="Event: Yes",choices = NULL,multiple = TRUE)       
                        ),
                        tags$div(class="columnSelect",
                                 selectizeInput('columnEvent0',label="Event: No",choices = NULL, multiple = TRUE)
                        ))
                    )
                 )
              )
           )
       )
   
}
summaryBSModal<-function (id, title, trigger,..., size) { 
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

cat("SIZE = ", size, "\n\n\n")
bsTag <- shiny::tags$div(class = "modal sbs-modal fade", id = id, tabindex = "-1", `data-sbs-trigger` = trigger, shiny::tags$div(class = size, shiny::tags$div(class = "modal-content", shiny::tags$div(class = "modal-header", shiny::tags$button(type = "button", class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), shiny::tags$h4(class = "modal-title", title) ), shiny::tags$div(class = "modal-body", DT::dataTableOutput("summaryModalTable") ), shiny::tags$div(class = "modal-footer", bsButton("gBack","Go Back") ) ) ) ) }


####################
# DE tab
####################
tab.DE.analysis = tabItem(tabName = "DifferentialExpressionAnalysis",
    bsAlert("alertDE"), 

conditionalPanel(condition = "input.selectGenes!=''",

   # actionButton("ClinicalDataBtn", "View Clinical Data", style = "display:inline-block; width:20%"),
#    bsButton("ClinicalDataBtn", "View Clinical Data", icon = NULL, style = "success",
#                        size = "default", type = "action", block = FALSE, disabled = FALSE,
#                        value = FALSE),
   uiOutput("selectedColumn", container = div, style = "display:inline-block; width: 20%"),
    uiOutput("selectedGroups", container = div, style = "display:inline-block; width: 55%"),
                                                    
                                                    
    actionButton("formatDEButton", "Format Graph"),
    #actionButton("DEadd", "Save R Code"),
    HTML("<button id='DEadd' type='button' class='btn btn-info action-button'>Save R Code</button>"),                                                
    plotOutput("plot"),
    formatBSModal("Format", "Format", "formatDEButton", applyID = "applyFormatDE", size = "large",
        htmlOutput("formatDE")#, hr(),               
        #actionButton("applyFormatDE", "Apply Changes")
    )
)
)


####################
# Survival tab
####################
tab.survival.analysis = tabItem("SurvivalAnalysis", 
    #bsCollapse(id = "SurvDataTable", open = "SurvDataTable",
        #bsCollapsePanel("Select Time and Outcome From Clinical Data",
            #div(style = "display:inline-block; width:30%",
             # uiOutput('survTime')
            #),
            #div(style = "display:inline-block; width:30%",
            #    uiOutput('survOutcome')
            #),
            #actionButton("parseButton", "Format Columns"), 
            #   DT::dataTableOutput("clinicalDataForSurvival")
            #)
    #),
         
    #shiny::hr(),
    #tags$div(HTML("<hr style = \"background-color: red; height:4px\">")), 
    #uiOutput("SurvMessage"),
#        summaryBSModal("summaryBSModal","Clinical Data Summary","manuBtn"),
        actionButton("formatDEButton2", "Format Graph"), # add on
        formatBSModal("Format2", "Format", "formatDEButton2", applyID = "applyFormatDE2", size = "large", htmlOutput("formatDE2")),
        actionButton("Survadd", "Save R Code"),
       # shinyBS::bsModal("parseModal", "Selected Survival Analysis Parameters", "parseButton", size = "large",
        #    fluidRow(

         #       column(4, textInput("survfind", label = "Find", value = ""),
          #      	  textInput("survreplace", label = "Replace", 
				#value = ""),
         #       	  actionButton("parseEnter", label = "Submit"), 
			  # not valid as a revert yet
          #      	  actionButton("undo", label="Revert Changes")),

           #   		column(3),
            #  		column(4, 
             # 			uiOutput("eventYes"), uiOutput("eventNo")
              #		)
	         # ), 
             #    DT::dataTableOutput("selectedCols")
      #  ),
         plotOutput("kmSurvival")
        
) # end tabPanel: Survival Analysis




analyses.common = conditionalPanel(condition = "input.tabs == 'Differential Expression Analysis' | input.tabs == 'Survival Analysis'",
    
        selectizeInput('selectGenes', "Select Gene", choices = NULL),
        conditionalPanel(condition = "input.tabs =='Survival Analysis'",
            #bsButton("autoAnalysis","Time/Outcome Selection", style="success"),
            genBSModal("autogenModal","Survival Analyses","",size="large"),
            
            style = "display:inline-block"
        ),
         
        #uiOutput('selectGenes'),
	    hr()
   
) 


#####################################################################
# The complete analyses tab
#####################################################################
tab.analyses =  navbarMenu("Analyses", icon = icon("bar-chart-o"),
                         analyses.common,
                         tab.DE.analysis,
                         tab.survival.analysis    
)


