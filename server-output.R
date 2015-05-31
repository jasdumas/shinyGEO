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
  selectInput("selectGenes", label = "Select Gene",
              choice = geneNames(), multiple = F,
              selected = 0)
})

output$selectProbes <- renderUI({
  selectInput("selectProbes", label = "Select Probe", 
              choices = probeNames(), multiple = F,
              selected = 0)
})


output$platform <- renderUI({
  selectInput('platform', 'Platform', Platforms(), multiple = F, selectize = FALSE)        
})

##################################################
## function to select most recent item selected
##################################################
mostRecent <- function() { 
  return (ColumnNames()[input$rows+1])
}

################################################
### Renders drop-down menu for variables/columns 
################################################  
output$selectedColumn <- renderUI({
  a=mostRecent()
  
  if ( length(a) == 0 ) {
    # if nothing has been selected it shows all possible choices
    selectInput('selectedColumn', 'Selected Column', 
                ColumnNames(), multiple = F, selectize = FALSE
    )
  }
  else {
    selectInput('selectedColumn', 'Selected Column', 
                mostRecent(), multiple = F, selectize = FALSE
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
#action = dataTableAjax(session, clinicalDataSummary(), rownames = TRUE) # for the row_output as characters

output$clinicalDataSummary <- renderDataTable({(datatable(as.data.frame(clinicalDataSummary()), 
                                                          selection = 'single', 
                                                          rownames = TRUE
                                                          #options = list(ajax = list(url = action)), server = TRUE
#                                                           callback = JS("function(table) {
#                                                           table.on('click.dt', 'tr', function() {
#                                                           $(this).closest('table').find('.selected').each(function(){  
#                                                           $(this).removeClass('selected');                          
#                                                           });
#                                                           $(this).toggleClass('selected');
#                                                           Shiny.onInputChange('rows',
#                                                           table.rows('.selected').indexes().toArray());
#                                                           });
#                                                           }")
                                                          ))})

output$DTtest <- renderPrint({
  s = input$clinicalDataSummary_rows_selected
  if (length(s)) {
    cat('These rows were selected:\n\n')
    cat(s, sep = '\n')
  }
  
})

######################################################################
## displays the full Clinical Data Table - currently with multi-select
#####################################################################
output$clinicalData <- renderDataTable({ datatable(clinicalInput(), 
                                                   filter = 'top', extensions = c('ColReorder', 'FixedColumns', 'KeyTable'),
                                       options = list(paging=F, searchable=T, info=T, autowidth=T, 
                                                      scrollX=T, scrollY="500px", scrollCollapse = T, 
                                                      dom = 'Rlfrtip') 
                                       )})
