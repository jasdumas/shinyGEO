##########################################################
# display functions for conditional panels              ##
##########################################################

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



################################################
### Renders drop-down menu for variables in ui 
################################################  
output$selectedColumn <- renderUI({
  a=mostRecent()
  
  if ( length(a) == 0 ) {
    
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

##################################################
## function to select most recent item selected
##################################################
mostRecent <- function() { 
  return (ColumnNames()[input$rows+1])
}


##################################################################################

### Renders drop-down menu for levels in ui ######################################
output$selectedGroups <- renderUI({
  selectInput('Group1Values','Select Groups for Comparison', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = defaultGroupsForSelectedColumn(),
              selectize = TRUE
              #      , options = list(maxOptions = 10)
              
  )
})




### Clinical Summary #############################################################

output$clinicalDataSummary <- renderDataTable(clinicalDataSummary(),
                                              callback = "function(table) {
        table.on('click.dt', 'tr', function() {
          $(this).closest('table').find('.selected').each(function(){
              $(this).removeClass('selected');
          });
          $(this).toggleClass('selected');
          Shiny.onInputChange('rows',
          table.rows('.selected').indexes().toArray());
        });
      }")
#output$dataSummary <- renderDataTable(NULL) 

##################################################################################

##################################################################################






#######################################
## displays the Clinical Data Table
#######################################
output$clinicalData <- renderDataTable(clinicalInput(), 
                  options = list(paging=F, searchable=T, info=T, autowidth=T, 
                      scrollX=T, scrollY="400px", scrollCollapse = T,
                                              "sDom" = 'RMD<"cvclear"C><"clear"T>lfrtip',
                                              "oTableTools" = list(
                                                "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                                                "aButtons" = list(
                                                  "copy",
                                                  "print",
                                                  list("sExtends" = "collection",
                                                       "sButtonText" = "Save",
                                                       "aButtons" = c("csv","xls"))))), 
                               callback = "function(table) {
                               table.on('click.dt', 'tr', function() {
                               $(this).toggleClass('selected');
                               Shiny.onInputChange('rows',
                               table.rows('.selected').indexes().toArray());
                               });
  }")
  