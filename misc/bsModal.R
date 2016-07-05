#####################################################################
# Variations on bsModal
#####################################################################

##########################################################################
## MODAL SECTION
##
## formatBSModal - equivalent to shinyBS::bsModal except we add a Save 
## 	Changes button with id = applyID next to the Close button
## 	shinyBSDep is not found, so last line is commented out. This does
## 	not appear to have an effect
##
## genBSModal - equivalent to shinyBS::bsModal except custom buttons for 
## 	generation of survival analysis
##
## summaryBSModal - 
## bsModalNoClose - No close button 
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
               bsButton("manuBtn", "View Data Table", icon = NULL, style = "info",
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
                        selectizeInput('autoColumnTime','Time Column',choices=NULL),
                        tags$br(),
                        DT::dataTableOutput("timetable")
                 ),
                 column(1,""),
                 column(3,
                        tags$h4(class="col-time-head","Outcome Column Selection"),
                        selectizeInput('autoColumnOutcome','Outcome Column',choices=NULL),
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
summaryBSModal<-function (id, title, trigger,size, ...) { 
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


bsTag <- shiny::tags$div(class = "modal sbs-modal fade", id = id, tabindex = "-1", `data-sbs-trigger` = trigger, shiny::tags$div(class = size, shiny::tags$div(class = "modal-content", shiny::tags$div(class = "modal-header", shiny::tags$button(type = "button", class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), shiny::tags$h4(class = "modal-title", title) ), shiny::tags$div(class = "modal-body", ... ), shiny::tags$div(class = "modal-footer", bsButton("gBack","Go Back") ) ) ) ) 
}



###############################################################################
# bsModal version where data-backdrop is 'static' and data-keyboard is 'false' 
# to prevent closing from clicking outside the modal and pressing escape
# The header and footer are also removed
###############################################################################
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


