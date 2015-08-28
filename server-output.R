##########################################################
# display functions for conditional panels              ##
##########################################################

load("series/series.RData")
load("platforms/platforms.RData")

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
              choice = probeNames(), multiple = F,
              selected = "")
})

PlatformLinks <- reactive({
  pl = Platforms()
  if (is.null(pl)) return(NULL)
  pl = paste0("<a target = \"_blank\" href = \"http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", 
         pl, "\">", pl, "</a>")
  pl = paste0("<p>",pl, "</p>")
  pl = paste0(pl, collapse = "")
  beg ="<p>Click on the links below for more information about the availalbe platforms:</p>"
  paste0(beg, pl)
})

output$PlatformLinks <-renderUI( {
  HTML(PlatformLinks())
})

observe ({
  ## only show plaforms for selected series ##
  pl = Platforms()
  cat("upate for pl = ", pl, "\n")
  selected = NULL
  
  if (!is.null(pl)) {
    cat("updating pl...\n")
    keep = platforms.accession %in% pl
    pl.accession = platforms.accession[keep]
    pl.description = platforms.description[keep]
    if (length(pl.accession) == 1) {
      selected = NULL #pl.accession
    }
  } else {
    pl.accession = platforms.accession
    pl.description = platforms.description
  }
  
updateSelectizeInput(session, inputId='platform', server = TRUE,
                     
#output$platform <- renderUI({  
#  selectInput('platform', 'Platform', Platforms(), multiple = F, selectize = FALSE)        
               choices = data.frame(label = pl.accession, value = pl.accession, name = pl.description),
               selected = selected,
               options = list(
                 #create = TRUE, persist = FALSE,
                 render = I(
                   "{
                      option: function(item, escape) {
                          return '<div> <strong>' + item.label + '</strong> - ' +
                          escape(item.name) + '</div>';
                      }
                    }"
                 ))
               )
#}


if (!is.null(pl)) {
createAlert(session, "alert", alertId = "GPL-alert", title = "Attention Needed", style = "success",
            content = "Please select a platform to continue", append = TRUE) 
}

})




#output$GSE <- renderUI({
#  selectizeInput('GSE', label = 'Accession Number', 
#                 choice = c("",series.accession), 
#                 multiple = F, selected = NULL           
#                 )        
#})

###############################################################
# drop down options are in form of GSE number - description
# when a selection is made only the GSE number (label)
# is stored in the textbox. However, only the
# GSE number (label) can be searched.
# Ideally, we want to search both the number and description
# but only display the number when selected
# 'value' is what gets returned to server (GSE number)
###############################################################
updateSelectizeInput(session, inputId='GSE', label = "Accession Number", server = TRUE,
    choices =  data.frame(label = series.accession, value = series.accession, name = series.description),
    options = list(
      #create = TRUE, persist = FALSE,
      render = I(
      "{
          option: function(item, escape) {
      return '<div> <strong>' + item.label + '</strong> - ' +
      escape(item.name) + '</div>';
      }
      }"
    ))
)

##########################################
# Only include GSE number               ##
##########################################
#observe({
#  cat("change input$GSE...\n")
#  GSE = input$GSE
#  if (!is.null(GSE)) {
#    cat("GSE = ", GSE, "\n")
#    GSE = strsplit(GSE, "-")[[1]][1]
#    choices = c(series)
#    updateSelectizeInput(session, "GSE", label = "Accession", choices = choices,
#                         selected = GSE, options = list(create = TRUE), server = FALSE)
#  }
#})



################################################
### Renders drop-down menu for variables/columns 
################################################  
observe({
  colNames = colnames(editClinicalTable())
  val = colNames[input$clinicalDataForDiffExp_columns_selected]
  
  cat("selected column  = ", val, "\n")
  output$selectedColumn <- renderUI({  
      # show possible choices (column names)
      selectInput('selectedColumn', 'Selected Column', 
            choices = ColumnNames(), #width='20%',
            selected = val, multiple = F, selectize = FALSE
    )
  })
})

#output$test2 <- renderText(paste0("row = ", input$clinicalData_rows_selected))

####################################################################
## renders drop-down menus (server-side) for clinical group selection
####################################################################
output$selectedGroups <- renderUI({
  selectInput('Group1Values','Select Groups for Comparison', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = defaultGroupsForSelectedColumn(),
              width='80%',
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

###########################################################################################
## Reactive for displaying the dataTable, since same display will be used multiple times
##########################################################################################
displayDataTable <-reactive({
  
  DT::renderDataTable({ datatable(editClinicalTable(), rownames = TRUE,
                                                      extensions = 'ColReorder',
                                                      options = list(dom = 'Rlfrtip', #ajax = list(url = action1), 
                                                                    #scrollX = "auto",
                                                                     scrollY = "400px",
                                                                     paging = F, 
                                                                     searchHighlight = TRUE,
                                                                     columnDefs = list(list(
                                                                       targets = 1: ncol(editClinicalTable()), # applies to the entire table
                                                                       render = JS(
                                                                         "function(data, type, row, meta) {",
                                                                         "return type == 'display' && data.length > 20 ?",
                                                                         "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                                                                         "}")
                                                                     ))), 
                                                      select = list(target = "column"),
                                                      filter = 'none')
  })
})


##############################################
# set output variables to display the table
##############################################
observe({

output$clinicalDataFAKE <- 
  DT::renderDataTable({ datatable(iris, rownames = TRUE,
                                  options = list(searchHighlight=TRUE),
                                  select = list(target = "column"),
                                  
                                  filter = "top"
                                 )
                    })

output$clinicalData <- displayDataTable()
output$clinicalDataForSurvival <- displayDataTable()
output$clinicalDataForDiffExp <- displayDataTable()

#output$test <- renderText(paste0("col = ", colnames(editClinicalTable())[input$clinicalDataForDiffExp_columns_selected]))
                          
di = clinicalInput()
#action1 = dataTableAjax(session, data=di, rownames = TRUE)

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
  
  closeAlert(session, "GPL-alert")

  cat("create expression alert\n")
  createAlert(session, "alert", alertId = "Expression-alert", title = "Current Status", style = "info",
              content = "Generating boxplot of expression data", append = TRUE) 
  
  par(mar=c(2+round(max(nchar(sampleNames(dataInput())))/2),4,2,1))
  title <- paste(isolate(input$GSE), '/', isolate(input$platform), title.detail, sep ='') # need 
  
  x1 = melt(x)
  #View(x1)  # to get aes(); X2 column header for GSMXXX values
  new <- ggplot(x1, aes(as.factor(Var2), value)) + geom_boxplot(outlier.colour = "green")
  r = (new + labs(title = title, y = y.label, x = "")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))) 
  print(r)
  
  closeAlert(session, "Expression-alert")
  
}

) 
) 

####################
####################
observe({

  val1 = input$survTimeUI
  val2 = input$survOutcomeUI
    
colNames = colnames(editClinicalTable())

if (!is.null(colNames) & !is.null(val1) & !is.null(val2)) {
  vals = colNames[input$clinicalDataForSurvival_columns_selected]
  cat("vals = ", vals, "\n")
  if (length(vals) == 1) {
    val1 = vals[1]
    val2 = NULL
  } else if (length(vals) == 2) {
    val1 = vals[1]
    val2 = vals[2]
  }
}

output$survTime <- renderUI({
     selectInput("survTimeUI", "Time", choice = ColumnNames(), selected = val1, multiple = F)
   })

output$survOutcome <- renderUI({
    selectInput("survOutcomeUI", "Outcome", choice = ColumnNames(), val2, multiple = F)
})

# this prevents the resetting of the drop-down columns after the action is pressed on the surv modal
#updateSelectInput(session, "survTimeUI", label = "Time", choice = ColumnNames(), selected = input$survTimeUI )
#updateSelectInput(session, "survOutcomeUI", label = "Outcome", choice = ColumnNames(), selected = input$survOutcomeUI )

}) # end of observe for time/outcome


output$selectedCols <- DT::renderDataTable({ 
  datatable(data = parse.modal(), rownames = F,
		options = list(dom = "Rlrtip", paging = F),
		filter = 'none')
}) 


###################
# Knitr Report
###################
output$knitDoc <- renderUI({
  input$DEadd
  input$Survadd
  return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))))
  #isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))) # trial without return 
})  
   
### Download knitr report ###
output$downloadData <- downloadHandler(
  filename = function() { 
    paste(date(), '.pdf', sep='') 
  },
  
  content = function(file) {
    src <- normalizePath('report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd')
  
    library(rmarkdown)
    
    out <- render('report.Rmd')
    file.rename(out, file)
  
  }
)

