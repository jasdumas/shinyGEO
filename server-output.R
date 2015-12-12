##########################################################
# display functions for conditional panels              ##
##########################################################

cat("begin server-output.R\n")

load("series/series.RData")
load("platforms/platforms.RData")

m = matrix(rnorm(1000), ncol=20)
rownames(m) = paste0("row", 1:nrow(m))

opp = list(dom = 'Rlfrtip', #ajax = list(url = action1), 
                       #scrollX = "auto",
                       #scrollY = "400px",
                       paging = T, 
                       searchHighlight = TRUE,
                       columnDefs = list(list(
                         targets = 1: ncol(m), # applies to the entire table
                         render = JS(
                           "function(data, type, row, meta) {",
                           "return type == 'display' && data.length > 20 ?",
                           "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                           "}")
                       ))
) 


## drop down boxes for event = yes and event = no
output$eventYes <- renderUI({  
  selectInput('eventYes', 'event = yes', 
              choices = KM$eventNames, #width='20%',
              selected = 0, multiple = TRUE, selectize = TRUE
  )
})

output$eventNo <- renderUI({  
  selectInput('eventNo', 'event = no', 
              choices = KM$eventNames, #width='20%',
              selected = 0, multiple = TRUE, selectize = TRUE
  )
})


#############################################
# dynamically change shinyTitle
#############################################
shinyTitle <-reactive({
  gse = isolate(input$GSE)
  platform = isolate(input$platform)
  if (is.null(gse) | gse == "") return("shinyGEO")
  paste0("shinyGEO - ", gse, "/", platform, sep = "")
})

output$shinyTitle = renderText(shinyTitle())

######################################################
# Hidden text boxes for conditional output
######################################################

# when platform info is availabe the other drop-down boxes are shown in the sidebar panel
sidebarDisplay <-reactive({
  if (is.null(dataInput())) return ("GSE-ONLY")
  if (is.null(platInfo())) return("PLATFORM")
  return("ALL")
})

observe({
cat("display = ", sidebarDisplay(), "\n")
})


output$sidebarDisplay <- renderText(sidebarDisplay())
outputOptions(output, 'sidebarDisplay', suspendWhenHidden=FALSE)


observe({
  add.tab()
  cat("observing for selectizeInput\n")
  options=  list(
      render = I(
        "{
            option: function(item, escape) {
                return '<div> <strong>' + item.genes + '</strong> - ' +  escape(item.probes) + '</div>';
            }
        }"
      )
    )

  updateSelectizeInput(session, "selectGenes", 
	label = "Select Gene", server = TRUE, 
 	choices = geneNames(), options = options 
  )
  cat("done observing for selectizeInput\n")
  subtract.tab()

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
  cat("update for pl = ", pl, "\n")
 
  pl.selected = NULL
  choices = NULL
  pl.options = NULL 
  if (!is.null(pl)) {
    cat("updating pl...\n")
    keep = platforms.accession %in% pl
    pl.accession = platforms.accession[keep]
    pl.description = platforms.description[keep]
    if (length(pl.accession) == 1) {
      pl.selected = pl.accession 
      choices = pl.selected
    } else {
      cat("multiple accessions..\n")
      pl.selected = NULL
      choices = data.frame(label = pl.accession, value = pl.accession, 
		name = pl.description)
      pl.options = list(
          render = I(
             "{
                option: function(item, escape) {
                     return '<div> <strong>' + item.label + '</strong> - ' +
                         escape(item.name) + '</div>';
                }
              }"
          )
       )
    }
  }
 
cat("update platform dropdown\n") 
updateSelectizeInput(session, inputId='platform', label = "Platform", server = TRUE,
               choices = choices,
               selected = pl.selected,
	       options = pl.options 
)

if (!is.null(pl)) {
	 d = dataInput()
	 num.samples = sapply(d, function(x) length(sampleNames(x)))
        num.features = sapply(d, function(x) length(featureNames(x)))
        annot = sapply(d, annotation)
        x = paste("There are <b>", num.samples, "</b>samples and<b>", num.features, "</b>features on platform <b>", annot, "</b>")
#        x = paste("<br>", p, "</br>", collapse = "")

x = paste(x, collapse = "<br>")
cat("create platform alert\n")
createAlert(session, "alert1", alertId = "GPL-alert", title = "Please select a platform to continue", style = "success",
            content = x, append = TRUE, dismiss = FALSE) 
}
cat("done create platform alert\n")
})


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

################################################
### Renders drop-down menu for variables/columns 
################################################  
observe({
  #colNames = colnames(editClinicalTable())
  #val = colNames[input$clinicalDataSummary_rows_selected]

  val = NULL

  colNames = rownames(clinicalDataSummary()) 
  val = input$summaryModalTable_row_last_clicked
  val = colNames[val]
 
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
  cat("observe for clinicalDataSummary\n") 
 
  output$clinicalDataSummary <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()), rownames = TRUE,  
                                                                 extensions = 'ColReorder',
                                                                 options = list(#dom = 'Rlfrtip', ajax = list(url = action), 
                                                                                paging = F,  searchHighlight = TRUE),
                                                                 filter = 'none', 
                                                                 selection = 'single') 
    
  })

  
#  dd = clinicalDataSummary()
#  action = dataTableAjax(session, data=dd, rownames = TRUE) # for the row_output as characters
  
})

###########################################################################################
## Reactive for displaying the dataTable, since same display will be used multiple times
##########################################################################################
displayDataTable <-reactive({
  
  DT::renderDataTable({ datatable(clinicalDataProcessed(), rownames = TRUE,
                                                      extensions = 'ColReorder',
                                                      options = list(dom = 'Rlfrtip', #ajax = list(url = action1), 
								    autoWidth = TRUE,
                                                                     scrollX = "auto",
                                                                     scrollY = "400px",
                                                                     paging = T, 
                                                                     searchHighlight = TRUE,
                                                                     columnDefs = list(list(
                                                                       targets = 1: ncol(clinicalDataProcessed()), # applies to the entire table
									width = '200px',

                                                                       render = JS(
                                                                         "function(data, type, row, meta) {",
                                                                         "return type == 'display' && data.length > 50 ?",
                                                                         "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                                                                         "}")
                                                                     ))), 
                                                      select = list(target = "column"),
                                                      filter = 'none')
  })
})


observe({
  cat("observe for summaryModalTable\n")  
  output$summaryModalTable <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()), rownames = FALSE,  
    extensions = 'ColReorder',
    options = list(#dom = 'Rlfrtip', #ajax = list(url = action), 
                   paging = F,  searchHighlight = TRUE),
                   filter = 'none', 
                   selection = 'single') 
    
  })
  
  
#  dd = clinicalDataSummary()
#  action = dataTableAjax(session, data=dd, rownames = TRUE) # for the row_output as characters
  
#  output$summaryModalTable <- displayDataTable() 

 
})

observe ({
  output$clinicalData = displayDataTable()
})


##############################################
# set output variables to display the table
##############################################
##############################
## Expression Profiles plot 
##############################
#observeEvent(input$submitButton,

if (EXPRESSION.PLOT) { 
output$exProfiles <- renderPlot({
  cat("\n\nrendering profiles...\n")

  # Return max 30 exp. samples if there is alot of samples to make the determination easier = unclutterd graphics
  x = profiles()
  cat("profiles() done\n")
  if (is.null(x)) {
	cat("no profiles\n")
	return(NULL)
  }
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
  
#  closeAlert(session, "GPL-alert")


  cat("create expression alert\n")
  createAlert(session, "alert1", alertId = "Expression-alert", title = "Current Status", style = "info",
               content = "Generating boxplot of expression data", append = FALSE, dismiss = TRUE) 
  par(mar=c(2+round(max(nchar(sampleNames(dataInput())))/2),4,2,1))
  title <- paste(isolate(input$GSE), '/', isolate(input$platform), title.detail, sep ='') # need 
 
  cat("expression alert created\n") 
  #library(tidyr) # possible move from reshape2 to tidyr
  #x1 = gather(data = x, na.rm =TRUE)
  fixed.df <- as.data.frame(x=x, stringsAsFactors = FALSE)
  
  x1 <- reshape2::melt(fixed.df, na.rm = TRUE, 
            variable.name = "variable", 
            value.name = "value")
#  View(head(x))
#  View(head(x1))  # to get aes(); X2 column header for GSMXXX values
  
  exp.prof.plot <- ggplot(x1, aes(variable, value)) + 
                geom_boxplot(outlier.colour = "green") +
                labs(title = title, y = y.label, x = "") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(exp.prof.plot)

  cat("close expression alert\n")
  closeAlert(session, "Expression-alert")
 
  createAlert(session, "alert1", alertId = "Analysis-alert", title = "Please select an Analysis", style = "success",
	content = "Gene expression profiles have been downloaded successfully. Please select either a Differential Expression Analysis or a Survival Analysis from the sidebar to continue")
}

) 
}
#) 

####################
####################
observe({
  cat("observe for survTime and outcome\n")

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


#Auto-Generation of columns
## Functions for autogen
##
calc.columns <- function(this){
  # First need to grep the first row of the data, then lapply a function that will return true for
  time.pattern = c("distant-relapse free survival","time","survival \\(mo\\)")
  outcome.pattern = c("distant-relapse event","outcome","dead of disease","dss censor","os censor")
  
  is.time.column <- function(x){
    ans = grepl(paste(time.pattern,collapse="|"),x)
    if(any(ans)){
      return(TRUE)
    }
    return(FALSE)
  }
  is.outcome.column <- function(x){
    ans = grepl(paste(outcome.pattern,collapse="|"),x)
    if(any(ans)){
      return(TRUE)
    }
    return(FALSE)
  }
  x.time = colnames(this)[apply(this,2,is.time.column)]
  y.outcome = colnames(this)[apply(this,2,is.outcome.column)]
  if(length(x.time) > 1){
    
   
    createAlert(session, "warningAlert", alertId = "warn1", title = "Warning: Multiple Time Columns Found",
                content = paste(c("<strong>Columns Found</strong>:", paste(x.time,collapse=", "),"<br><br> shinyGEO has chosen the best fit.")), style= 'danger', dismiss = TRUE, append = TRUE)
    x.time = x.time[1]
  }
  else if(length(x.time) == 0){
    x.time = NA
    
  }
  if(length(y.outcome) > 1)
  {
    createAlert(session, "warningAlert", alertId = "warn2", title = "Warning: Multiple Outcome Columns Found",
                content = paste(c("<strong>Columns Found</strong>: ", paste(y.outcome,collapse=", "),"<br><br> shinyGEO has chosen best fit.")), style= 'danger', dismiss = TRUE, append = TRUE)
    y.outcome = y.outcome[1]
  }
  
  ans = c(x.time,y.outcome)
  return (ans) 
}

reduce <- function(column){
  gsub(".*: ","",column)
}
reduce.columns <- function(time,outcome,this){
  if(is.na(outcome)){
    reduced.time = reduce(this[[time]])
    ans = list(time = reduced.time)
    return(ans)
  }
  else if(is.na(time)){
    reduced.outcome = reduce(this[[outcome]])
    reduced.outcome = replace(reduced.outcome,(reduced.outcome == "NO" | reduced.outcome == "censored"),0)
    reduced.outcome = replace(reduced.outcome,(reduced.outcome == "YES" | reduced.outcome == "uncensored"),1)
    ans = list(outcome = reduced.outcome)
    return (ans)
    
  }
  else if(is.na(time) && is.na(outcome)){
    createAlert(session, "warningAlert", alertId = "warn3", title = "Warning: No Columns were found",
                content = c("<p>Oops! shinyGEO could not find columns for survival analysis in your data. Please try the following: <ol><li>View the table and select the columns relevant to time and outcome or..</li><li>Use manual selection and format your data accordingly.</li></ol></p>"), style= 'danger', dismiss = TRUE, append = TRUE)
    ans = list(time = NA, outcome = NA)
    return(ans)
  }
  else{
  reduced.time = reduce(this[[time]])
  reduced.outcome = reduce(this[[outcome]])
  reduced.outcome = replace(reduced.outcome,(reduced.outcome == "NO" | reduced.outcome == "censored"),0)
  reduced.outcome = replace(reduced.outcome,(reduced.outcome == "YES" | reduced.outcome == "uncensored"),1)
  ans = list(time = reduced.time, outcome = reduced.outcome)
  return (ans)
  }
}

#main function
main.gen <- function(this,columns.data){
  print("Generating automatic column selection and formatting...")
  #Reduce and analyze
  new = reduce.columns(columns.data[1],columns.data[2],this)
  time.analysis <<- new$time
  outcome.orig = as.character(this[[columns.data[2]]])
  outcome.new = new$outcome
  outcome.no = unique(outcome.orig[outcome.new == 0])
  outcome.yes = unique(outcome.orig[outcome.new == 1])
  # Create data frame for time output
  time_both <- data.frame(this[[columns.data[1]]],new$time)
  #Render UI
  toggleModal(session,"autogenModal",toggle="toggle")
  columnItems = as.character(unique(this[[columns.data[2]]]))
  columnItems = setdiff(columnItems,c(""," "))
  updateSelectizeInput(session,"autoColumn.time",choices=colnames(this),selected=columns.data[1])
  updateSelectizeInput(session,"autoColumn.outcome",choices=colnames(this),selected=columns.data[2])
  updateSelectizeInput(session,"columnEvent1",choices=columnItems,selected=outcome.yes,server=TRUE)
  updateSelectizeInput(session,"columnEvent0",choices=columnItems,selected=outcome.no,server=TRUE)
  output$timetable <- DT::renderDataTable({time_both})
  outcome.analysis <<- outcome.new
}

if (AUTOSELECT.SURVIVAL) {

observeEvent(input$autoAnalysis,({
  print("Column selection and formatting for survival analysis started..")
  if (is.null(values.edit$table)) return(NULL)
  this = values.edit$table
  columns.data = calc.columns(this)
  main.gen(this,columns.data)
  print("Column selection and formatting for survival analysis finished..")
}))

observeEvent(input$autoColumn.time,({
  print("observe autoColumn.time")
    this = values.edit$table
    if (is.null(values.edit$table)) return(NULL)
    new = reduce.columns(input$autoColumn.time,NA,this)
    time.analysis <<- new$time
    time_both <- data.frame(this[[input$autoColumn.time]],new$time)
  #  output$timetable <- renderDataTable(time_both)
    output$timetable <- DT::renderDataTable({time_both})
}))


observeEvent(input$autoColumn.outcome,({
  print("observe autoColumn.outcome")
    if (is.null(values.edit$table)) return(NULL)
    this = values.edit$table
    selected = input$autoColumn.outcome
    cat("selected = ", selected, "\n")
    if (selected == "") return(NULL)
    selected = setdiff(selected, c("", " "))
    new = reduce.columns(NA,input$autoColumn.outcome,this)
    outcome.orig = as.character(this[[input$autoColumn.outcome]])
    outcome.new = new$outcome
    outcome.no = unique(outcome.orig[outcome.new == 0])
    outcome.yes = unique(outcome.orig[outcome.new == 1])
    columnItems = as.character(unique(this[[selected]]))
    outcome.analysis <<- outcome.new
    updateSelectizeInput(session,"columnEvent1",choices=columnItems,selected=outcome.yes,server=TRUE)
    updateSelectizeInput(session,"columnEvent0",choices=columnItems,selected=outcome.no,server=TRUE)

}))


observeEvent(input$genBtn,
           ({
	print("observe genBtn\n")
    	if (is.null(values.edit$table)) return(NULL)
             output$kmSurvival <- renderPlot({
               main = paste(input$GSE, geneLabel() , sep = ": ")
		cat("main = ", main, "\n")
               return(plot.shiny.km(time = as.double(time.analysis), 
                                    #death = as.integer(parse.modal()[,2]), 
                                    death = as.integer(outcome.analysis), 
                                    x = x(), 
                                    col = colorsDE3(), title = main))
               
             })
             closeAlert(session,"warn1")
             closeAlert(session,"warn2")
             closeAlert(session,"warn3")
             toggleModal(session,"autogenModal",toggle = "toggle")
             
             tags$script(HTML("window.location.href= '/#kmSurvial'"))
           })
)

} # end autoselect.survival



createAlert(session,"ioAlert",content = "<H4>Directions</H4><p>1. Download the current clinical data you are working with, it will be saved in your 'Downloads' folder.<br>2. Edit the dataset, then save your changes.<br>3. Upload your dataset back.</p>",dismiss=FALSE)
# start clinical data iotab button events
output$downloadSet<- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste("shinyGEO-dataset", "csv", sep = ".")
   },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    
    content = function(file) {
      sep <- ","
      # Write to a file specified by the 'file' argument
      write.table(values.edit$table, file, sep = sep,
                  row.names = TRUE, col.names = NA) 
    }
    
)
observeEvent(input$downloadSet,(
  createAlert(session,"ioAlert2",content = "<H4>Current Status</H4><p><strong>Your file has been downloaded!</p>",style="success",dismiss=FALSE)
))


output$selectedCols <- DT::renderDataTable({ 
  datatable(data = parse.modal(), rownames = F,
		options = list(dom = "Rlrtip", paging = F),
		filter = 'none')
}) 


observe({
  add.tab()
  cat("in file upload observe...\n")  
  infile <- input$fileUpload
  if (!is.null(infile)){
    createAlert(session,"ioAlert3",content = "<H4>Current Status</H4><p><strong>File has been uploaded! You can now view your data table!</p>",style="success",dismiss=FALSE)
    cat("initial data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")
    data = read.table(infile$datapath, header = TRUE, row.names=1, sep = ",")
    isolate(values.edit$table <- data)
    cat("new data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")
  }
  cat("left file upload observe...\n")
})


if (DE.PLOT) {
  observe({
    
      PLOT = TRUE
      
      if (input$selectGenes == "") {
        cat("\n\n=====NO GENE=====\n\n")
        createAlert(session, "alert2", alertId = "Gene-alert", 
                    title = "Please select a gene and probe to continue", 
                    style = "danger",
                    content = "", append = TRUE,
                    dismiss = FALSE) 
        PLOT = FALSE
      }    
      else {
        closeAlert(session, "Gene-alert")
          if (length(input$Group1Values) == 0) {
            output$selectGroupsMessage <-renderUI({
              HTML("<h3>Please Choose The Groups to Compare</h3>")}
            )
            PLOT = FALSE
          }
      }
      
      if (!PLOT) {
              output$plot <-renderPlot({NULL})
      } else  {
          output$selectGroupsMessage <-renderText({""})
          output$plot <- renderPlot({
              x = profiles()[input$selectGenes,] # effected by data transformation
              iv = input$selectedColumn
              m = match(as.character(iv), colnames(clinicalDataProcessed()))  # GD: change grep to match
              clinical = as.character(clinicalDataProcessed()[,m]) 
              selected = c(as.character(input$Group1Values))
              k = clinical%in% selected
    
              y = clinical
              y[!k] = NA
              
              ## make sure levels are in selected order for plot
              y = factor(y, levels = input$Group1Values)
              
              main = paste(input$GSE, geneLabel() , sep = ": ")
              #gd              
              #stripchart2(x,y, col = colorsDE(), group.names = labelsDE(), main = main, ylab = "log2 expression")
              #jd
              print(stripchart2(x,y, group.names = labelsDE(), main = main, col=colorsDE()))
             
              }) # end of plot reactive
          
    }
  })  # end observe
}

cat("end server-output.R\n")
