
####################
observe({
  add.tab()
  cat("observe for survTime and outcome\n")
  
  val1 = input$survTimeUI
  val2 = input$survOutcomeUI
  
  #colNames = colnames(editClinicalTable())
  colNames = colnames(clinicalDataProcessed())
  
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
  cat("end observe survTime and survOutcome\n")
  subtract.tab()
  
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
  else if(length(y.outcome) == 0){
    y.outcome = NA
  }
  if(is.na(y.outcome) & is.na(x.time)){
    createAlert(session,"warningAlert",alertId = "warn3",title = "Warning: No survival information was found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
    
    
  }
  else if(is.na(y.outcome) & !is.na(x.time)){
    createAlert(session,"warningAlert",alertId = "warn3",title = "Warning: No survival outcome columns were found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
    
  }
  else if(is.na(x.time) & !is.na(y.outcome)){
    createAlert(session,"warningAlert",alertId = "warn3",title = "Warning: No survival time columns were found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
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






