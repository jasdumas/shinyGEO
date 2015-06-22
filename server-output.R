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
output$clinicalData <- DT::renderDataTable({ datatable(as.data.frame(x = editClinicalTable()), #rownames = TRUE,
                                                   extensions = 'ColReorder',
                                                   #options = list(dom = 'Rlfrtip', ajax = list(url = action1)),
                                                   filter = 'top',
                                                   selection = 'multiple') 
  })

#di = clinicalInput()
#action1 = dataTableAjax(session, data=di, rownames = TRUE)

## removed rownames to eliminate datatables error: https://www.datatables.net/manual/tech-notes/4 
## I can edit the table now

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
  #if (input$radio == 1 | input$radio == 2) return (y.label = "log2 Expression")        # doesnt work yet
  #else return(y.label = "Expression")
  boxplot(x = profiles(), boxwex=0.6, notch=T, main=title, outline=FALSE, las=2, ylab="log2 Expression", col = colors())
  
})
)
##############################################################################
# This reactive function changes the value of a reactive function into a variable
# to be used as a reactive expression in the plot but I could also call profiles() in the 
# in the Diff. Expr. Analysis to truely reflect the data transformation
##############################################################################
profiles <- reactive({
  ### log2 transform (auto-detect) citation ###
  # Edgar R, Domrachev M, Lash AE.
  # Gene Expression Omnibus: NCBI gene expression and hybridization array data repository
  # Nucleic Acids Res. 2002 Jan 1;30(1):207-10
  
  ex <- exprInput()
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC | input$radio == 1) { ex[which(ex <= 0)] <- NaN
  return (ex <- log2(ex)) }    ## next step is to include a function that displays a sample() of the results to prevent over-crowding
  
  if (input$radio == 2) return (ex <- log2(exprInput()))   # forced Yes
  else return (ex <- exprInput())  # No
  
 })

#######################################################
## Find & Replace Method for editing tables 
######################################################
 
# output$dropModal <- renderUI({
#      selectInput("drop2", "Column Names", choices = rownames(p), selected = "")
#    })

#observe({  
## reactives for textboxes/drop-downs in modal window
   find.str <- reactive({input$find})       
   replace.str <- reactive({input$replace})
   column.num <- reactive({input$dropModal})  # changed drop-downs back to numbers for subset

## editClinicalTable uses grep as a all-or-nothing replacement -> soon to include gsub for changing parts of cells!   
editClinicalTable <- reactive({
     input$Enter    
     
     #cat(" in Full Clinical Table\n")  # trouble-shooting print output
  find.str = isolate(find.str())
  column.num = isolate(column.num())
  replace.str = isolate(replace.str())
     
     if (find.str == "" & replace.str == "") {   # if there is nothing entered it returns the original table
       return(values.edit$table)
     }
     exactMatch = isolate(input$checkbox) # exact match condition
     
     if (exactMatch) {    # while default is false
       find.str = paste("^", find.str, "$", sep = "")
     }
     
     newClinical <- values.edit$table
     
     ### if factor, change to character.  Otherwise we can't replace it. ##
     if (is.factor(newClinical[,column.num])) {
       newClinical[,column.num] = as.character(newClinical[,column.num])
     }
     
     g.total = grep(find.str, newClinical[,column.num])  # regular g is used in selectGene() reactive
     cat("g.total = ", g.total, "\n")
     newClinical[g.total,column.num] = replace.str 
     cat("replacing ", find.str, "with ", replace.str)
     values.edit$table = newClinical
     return (values.edit$table)
    
     
   }) # end of editClinicalTable() reactive

#observe({if(!is.na(input$find)) print(input$find) })

#})  # end of observe for editClinicalTable()
  
   