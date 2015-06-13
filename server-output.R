##########################################################
# display functions for conditional panels              ##
##########################################################
# when platform info is availabe the other drop-down boxes are shown in the sidebar panel
displayPlatform <-function() {
  if (is.null(Platforms())) return(FALSE)
  return(TRUE)
}
output$displayPlatform <- renderText(displayPlatform())
outputOptions(output, 'displayPlatform', suspendWhenHidden=FALSE)

output$selectGenes <- renderUI({
  validate(
    need(!is.null(geneNames()), 'Processing.')
  )
  selectInput("selectGenes", label = "Select Gene",
              choice = geneNames(), multiple = F,
              selected = 0)
})

output$selectProbes <- renderUI({
  validate(
    need(!is.null(probeNames()), 'Processing.')
  )
  selectInput("selectProbes", label = "Select Probe", 
              choices = probeNames(), multiple = F,
              selected = 0)
})


output$platform <- renderUI({
  validate(
    need(!is.null(Platforms()), 'Processing.')
  )
  selectInput('platform', 'Platform', Platforms(), multiple = F, selectize = FALSE)        
})

##################################################
## function to select most recent item selected
##################################################
mostRecent <- function() { 
  return (ColumnNames()[input$rows+1]) # This doesn't seem relevant and maybe removed soon
}

################################################
### Renders drop-down menu for variables/columns 
################################################  
output$selectedColumn <- renderUI({
  a=mostRecent() # This doesn't seem relevant and maybe removed soon (if/else statement setup)
  
  if ( length(a) == 0 ) {
    # if nothing has been selected it shows all possible choices
    selectInput('selectedColumn', 'Selected Column', 
                choices = input$clinicalDataSummary_row_last_clicked, multiple = F, selectize = FALSE
    )
  }
  else {
    selectInput('selectedColumn', 'Selected Column', 
                choices = input$clinicalDataSummary_row_last_clicked, multiple = F, selectize = FALSE
    )
  }
})


####################################################################
## renders drop-down menus (server-side) for clinical group selection
####################################################################
output$selectedGroups <- renderUI({
  selectInput('Group1Values','Select Groups for Comparison', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = defaultGroupsForSelectedColumn(),
              selectize = TRUE
              
              
  )
})

############################################
## displays the Clinical Summary Data Table
###########################################

observe({  # observe needed since data object is a reactive function
  validate(
    need(!is.null(clinicalDataSummary()), 'Processing.')
  )
  output$clinicalDataSummary <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()), rownames = TRUE,  
                                                                extensions = 'ColReorder',
                                                                options = list(dom = 'Rlfrtip', ajax = list(url = action)),
                                                                filter = 'top', 
                                                                selection = 'single') 
    
  })
  
  dd = clinicalDataSummary()
  
  action = dataTableAjax(session, data=dd, rownames = TRUE) # for the row_output as characters
  
})


output$DTtest <- renderPrint({
  s = input$clinicalDataSummary_row_last_clicked # explose the index of the last clicked row to shiny, per request in #78
  if (length(s)) {
    cat('These rows were selected:\n\n')
    cat(s, sep = '\n')
  }
  
})

######################################################################
## displays the full Clinical Data Table - currently with multi-select
#####################################################################

observe({
  validate(
    need(!is.null(clinicalInput()), 'Processing.')
  )
  output$clinicalData <- DT::renderDataTable({ datatable(as.data.frame(clinicalInput()), rownames = TRUE,
                                                         extensions = 'ColReorder',
                                                         options = list(dom = 'Rlfrtip', ajax = list(url = action1)),
                                                         filter = 'top',
                                                         selection = 'multiple') # this needs to be multiple selection soon
  })
  
  
  di = clinicalInput()
  
  action1 = dataTableAjax(session, data=di, rownames = TRUE)
  
})

output$DTtestFull <- renderPrint({
  j = input$clinicalData_rows_selected
  if (length(j)) {
    cat('These rows were selected:\n\n')
    cat(j, sep = '\n')
  }
})
