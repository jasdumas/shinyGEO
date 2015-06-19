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
#output$clinicalData <- DT::renderDataTable({ datatable(as.data.frame(clinicalInput()), rownames = TRUE,
output$clinicalData <- DT::renderDataTable({ datatable(data = switchTable(), rownames = TRUE,
                                                   extensions = 'ColReorder',
                                                   options = list(dom = 'Rlfrtip', ajax = list(url = action1)),
                                                   filter = 'top',
                                                   selection = 'multiple') # this needs to be multiple selection soon
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
  palette(c("#99d5db", "#d399db")) # i don't think this pallete works right now
  dev.new(width=4+dim(dataInput())[[2]]/5, height=6)
  par(mar=c(2+round(max(nchar(sampleNames(dataInput())))/2),4,2,1))
  title <- paste (input$GSE, '/', input$platform, " selected samples", sep ='')
  #if (input$radio == 1 | input$radio == 2) return (y.label = "log2 Expression")        # doesnt work yet
  #else return(y.label = "Expression")
  boxplot(x = profiles(), boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, ylab="log2 Expression")
  
})
)
######## This reactive function changes the value of a reactive function into a variable
######## to be used as a reactive expression in the plot but I could also call profiles() in the 
######## in the Diff. Expr. Analysis to truely reflect the data transformation

   profiles <- reactive({

    if (input$radio == 1) return (ex <- log2(exprInput()))  # this is the auto-detect for now
    if (input$radio == 2) return (ex <- log2(exprInput()))
    else return (ex <- exprInput())
 })

   #######################################################
   ## Find & Replace Method 
   ######################################################
   output$dropModal <- renderUI({
     selectInput("drop2", "Column Names", choices = ColumnNames(), selected = "")
   })
   
   ######################################################### 
   # Editable tables function + temporary observe functions
   ########################################################
   
   find.str <- reactive({input$find})       # reactives for textboxes in modal window
   replace.str <- reactive({input$replace})
   column.num <- reactive({input$drop2})
   
   #observeEvent(input$Enter,
   editClinicalTable <- reactive({
     input$Enter    
     
     cat(" in Full Clinical Table\n")  # trouble shooting print output
     
     exactMatch = isolate(input$checkbox) # exact match condition
     
     if (exactMatch) {    # while default is false
       find.str() = paste("^", find.str, "$", sep = "")
     }
     
     newClinical <- values$table
     #newClinical <- iris
     
     ### if factor, change to character.  Otherwise we can't replace it. ##
     if (is.factor(newClinical[,column.num()])) {
       newClinical[,column.num()] = as.character(newClinical[,column.num()])
     }
     
     g = isolate(grep(find.str(), newClinical[,column.num()])) 
     cat("g = ", g, "\n")
     newClinical[g,column.num()] = replace.str() 
     cat("replacing ", find.str(), "with ", replace.str())
     return (newClinical)
     #if (!is.null(find.str()) & !is.null(replace.str())) return(newClinical)
     #else return(clinicalInput())
     
   })
   
   switchTable <- reactive({
     if (is.null(find.str()) & is.null(replace.str())) return(newClinical)
     else return(clinicalInput())
   })
  # )
   