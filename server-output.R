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

################################################
### Renders drop-down menu for variables/columns 
################################################  
output$selectedColumn <- renderUI({
  
# show possible choices (column names)
selectInput('selectedColumn', 'Selected Column', 
            choices = ColumnNames(),
            selected = input$clinicalDataSummary_row_last_clicked, multiple = F, selectize = FALSE
    )
  
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
  
  output$clinicalDataSummary <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()), rownames = TRUE,  
                                                                 extensions = 'ColReorder',
                                                                 options = list(dom = 'Rlfrtip', ajax = list(url = action)),
                                                                 filter = 'top', 
                                                                 selection = 'single') 
    
  })
  
  dd = clinicalDataSummary()
  action = dataTableAjax(session, data=dd, rownames = TRUE) # for the row_output as characters
  
})

######################################################################
## displays the full Clinical Data Table - currently with multi-select
#####################################################################
observe({
output$clinicalData <- DT::renderDataTable({ datatable(editClinicalTable(), rownames = TRUE,
                                                   extensions = 'ColReorder',
                                                   options = list(dom = 'Rlfrtip', ajax = list(url = action1)),
                                                   filter = 'top',
                                                   selection = 'multiple')
  })

di = clinicalInput()
action1 = dataTableAjax(session, data=di, rownames = TRUE)

})

##############################
## Expression Profiles plot 
##############################
observeEvent(input$submitButton,
output$exProfiles <- renderPlot({ 
  
  # set parameters and draw the plot
  #palette(c("#99d5db", "#d399db"))                       # global palette choices for strip chart too
  dev.new(width=4+dim(dataInput())[[2]]/5, height=6)
  par(mar=c(2+round(max(nchar(sampleNames(dataInput())))/2),4,2,1))
  title <- paste (input$GSE, '/', input$platform, " selected samples", sep ='')
  # Changes y-zxis label if radio choices change
  if (input$radio == 1 | input$radio == 2) {
     y.label = "log2 Expression"       
  } else {
    y.label = "Expression"
  }
  
  boxplot(x = profiles(), boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, ylab= y.label, col = colors())
  
})
)

####################
# Survival Analysis - place holder
####################
output$survTime <- renderUI({
     selectInput("servTimeUI", "Time", choices = ColumnNames(), selected = "")
   })

output$survOutcome <- renderUI({
    selectInput("servOutcomeUI", "Outcome", choices = ColumnNames(), selected = "")
})

output$survX <- renderUI({
  selectInput("servXUI", "x", choices = ColumnNames(), selected = "")
})

###### Survival reactives ########
time <- reactive({
  input$servTimeUI
})

outcome <- reactive ({
  input$servOutcomeUI
})

x <- reactive ({
  input$servXUI
})

#### Plots
output$kmSurvival <- renderPlot(
  a = 1:25,
  boxplot(a)
  # plot.shiny.km(time = time(), death = outcome(), x = x())
  ## create options for title/labels/colors etc!
  
)

#####

   