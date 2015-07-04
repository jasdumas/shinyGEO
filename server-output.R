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


#### Survival Analysis duplicates of Gene and Probe selection
output$selectGenesSurv <- renderUI({
  selectInput("selectGenesSurv", label = "Select Gene",
              choice = geneNames(), multiple = F,
              selected = 0)
})

output$selectProbesSurv <- renderUI({
  selectInput("selectProbesSurv", label = "Select Probe", 
              choices = probeNames(), multiple = F,
              selected = 0)
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
                                                                 options = list(dom = 'Rlrtip', ajax = list(url = action), paging = F),
                                                                 filter = 'none', 
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
                                                   options = list(dom = 'Rlfrtip', ajax = list(url = action1), paging = F),
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
  # Return max 30 exp. samples if there is alot of samples to make the determination easier = unclutterd graphics
  x = profiles()
  if (is.null(x)) return(NULL)
  n = ncol(x)
  if (n > 30) {
    s = sample(1:n, 30)
    x = x[,s]
  }
  
  # if more than 30 samples change the title to include " selected samples" since they are randomly selected, else " samples"
  if (n > 30) {
    title.detail = " selected samples"
  } else {
    title.detail = " samples"
  }
  
  # Changes y-zxis label if radio choices change
  if (input$radio == 1 | input$radio == 2) {
     y.label = "log2 Expression"       
  } else {
    y.label = "Expression"
  }
  
  # set parameters and draw the plot
  #palette(c("#99d5db", "#d399db"))                       # global palette choices for strip chart too
  #dev.new(width=4+dim(dataInput())[[2]]/5, height=6)
  par(mar=c(2+round(max(nchar(sampleNames(dataInput())))/2),4,2,1))
  title <- paste(input$GSE, '/', input$platform, title.detail, sep ='') # need 
  boxplot(x = x, boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, ylab= y.label, col = colors())
  
})
)

####################
# Survival Analysis 
####################
output$survTime <- renderUI({
     selectInput("survTimeUI", "Time", choices = ColumnNames(), selected = "")
   })

output$survOutcome <- renderUI({
    selectInput("survOutcomeUI", "Outcome", choices = ColumnNames(), selected = "")
})

    ##############################################
    # serThis choice parameter depends on whether the user 
    # would select groups by sample name [GSM...] (rownames(editClinicalTable()))
    # or if they need to choose a column that has been edited 
    # ie. stages in a ColumnNames() or groupsForSelectedColumn() [Immature B Cells..]
    ##############################################
    output$survX <- renderUI({
      selectInput("survXUI", "x", choices = ColumnNames(), 
              #selected = input$editClinicalTable_rows_selected, 
              multiple = F, selectize = TRUE)  
    })

#### Plots
output$kmSurvival <- renderPlot({
  a = 1:25
  boxplot(a)
  # plot.shiny.km(time = time(), death = outcome(), x = x())
  ## create options for title/labels/colors etc!
  
})

output$selectedCols <- DT::renderDataTable( datatable(iris)
  
)
#####

   
