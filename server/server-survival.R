library(stringr)

#Auto-Generation of columns
## Functions for autogen
##
calc.columns <- function(this){
  # First need to grep the first row of the data, then lapply a function that will return true for
  time.pattern = c("distant-relapse free survival","time","survival \\(mo\\)", "survival month", "survival \\(months\\)","survival months")
  outcome.pattern = c("distant-relapse event","outcome","dead of disease","dss censor","os censor","overall survival", "cancer specific survival", "survival")
  
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
                content = paste(c("<strong>Columns found</strong>: ", paste(x.time,collapse=", "),"<br> Please check that the selection is correct.")), style= 'danger', dismiss = TRUE, append = TRUE)
    x.time = x.time[1]
  }
  else if(length(x.time) == 0){
    x.time = NA
    
    
  }
  if(length(y.outcome) > 1)
  {
    createAlert(session, "warningAlert", alertId = "warn1", title = "Warning: Multiple Outcome Columns Found",
                content = paste(c("<strong>Columns Found</strong>: ", paste(y.outcome,collapse=", "),"<br><br> Please check that the selection is correct.")), style= 'danger', dismiss = TRUE, append = TRUE)
    y.outcome = y.outcome[1]
  }
  else if(length(y.outcome) == 0){
    y.outcome = NA
  }
  
  if(is.na(x.time) & !is.na(y.outcome)){
    createAlert(session,"warningAlert",alertId = "warn1",title = "Warning: No survival time columns were found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
  }
  else if(is.na(y.outcome) & !is.na(x.time)){
    createAlert(session,"warningAlert",alertId = "warn1",title = "Warning: No survival outcome columns were found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
  }
 
  if(y.outcome == x.time & !is.na(y.outcome) & !is.na(x.time)){
    y.outcome = NA
    createAlert(session,"warningAlert",alertId = "warn1",title = "Warning: No survival outcome columns were found!", content = "<p>If you believe this is incorrect, you can review the clinical data and select the appropriate columns. </p>",style= 'danger', dismiss = TRUE, append = TRUE)
    
  }
  ans = c(x.time,y.outcome)
  return (ans) 
}


##########################################################
# returns the current and formatted time vector  
###########################################################
time.analysis <-reactive({
	this = values.edit$table
        if(is.null(this)) return(NULL)

	code1 = paste0("time.column = \"", input$autoColumnTime, "\"")
 	code2 = paste0("time = as.double(gsub(\".*: \",\"\",data.p[[time.column]]))")

	code = paste(code1, code2, sep = "\n")	
	time = as.double(reduce(this[[input$autoColumnTime]]))	

	list(code = code, time = time)

})


##########################################################
# returns the current and outcome converted to 0/1 format
# this is used to autoselct eventYes and eventNo groups 
###########################################################
outcome.01 <-function(outcome, this) {
	if (is.na(outcome)) return(NA)

	reduced.outcome = reduce(this[[outcome]])
        reduced.outcome = str_to_upper(reduced.outcome) 

        NO = c("NO", "CENSORED", "SURVIVAL")
        YES = c("YES", "UNCENSORED", "DEATH")

    	reduced.outcome = replace(reduced.outcome, reduced.outcome %in% NO ,0)
        reduced.outcome = replace(reduced.outcome, reduced.outcome %in% YES, 1)
        reduced.outcome
}

# remove *:
reduce <- function(column){
  gsub(".*: ","",column)
}

# reduces time and outcome columns
# we need this function right now
# because time and outcome may be autodetected 
# (i.e., not selected in drop down)
reduce.columns <- function(time,outcome,this){
   if(is.na(time) && is.na(outcome)){
    createAlert(session, "warningAlert", alertId = "warn3", title = "Warning: No Columns were found",
                content = c("<p>Oops! shinyGEO could not find columns for survival analysis in your data. Please try the following: <ol><li>View the table and select the columns relevant to time and outcome or..</li><li>Use manual selection and format your data accordingly.</li></ol></p>"), style= 'danger', dismiss = TRUE, append = TRUE)
    ans = list(time = NA, outcome = NA)
    return(ans)
  }

  if(is.na(outcome)){
    reduced.time = reduce(this[[time]])
    ans = list(time = reduced.time, outcome = NA)
    return(ans)
  }
  else if(is.na(time)){
    reduced.outcome = reduce(this[[outcome]])
    reduced.outcome = replace(reduced.outcome,(reduced.outcome == "NO" | reduced.outcome == "censored" | reduced.outcome == "survival"),0)
    reduced.outcome = replace(reduced.outcome,(reduced.outcome == "YES" | reduced.outcome == "uncensored" | reduced.outcome == "death"),1)
    ans = list(time = NA,outcome = reduced.outcome)
    return (ans)
    
  } else{
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
  #Reduce and analyze
  # update inputs for time and outcome columns
  updateSelectizeInput(session,"autoColumnTime",choices=colnames(this),
	selected=columns.data[1])
  updateSelectizeInput(session,"autoColumnOutcome",choices=colnames(this),
	selected=columns.data[2])
  new = reduce.columns(columns.data[1],columns.data[2],this)
  if (!is.na(new$outcome)) {
    outcome.orig = as.character(this[[columns.data[2]]])
    outcome.new = new$outcome
    outcome.no = unique(outcome.orig[outcome.new == 0])
    outcome.yes = unique(outcome.orig[outcome.new == 1])
    columnItems = as.character(unique(this[[columns.data[2]]]))
    columnItems = setdiff(columnItems,c(""," "))
    updateSelectizeInput(session,"columnEvent1",choices=columnItems,
	selected=outcome.yes,server=TRUE)
    updateSelectizeInput(session,"columnEvent0",choices=columnItems,
	selected=outcome.no,server=TRUE)
  }

  if (!is.na(new$time)) {
    time_both <- data.frame("TimeColumnOriginal" = this[[columns.data[1]]],
			  "TimeColumnFormatted" = new$time)
    rownames(time_both) <- rownames(this)
    # remove columns with no data
    keep = !is.na(time_both[,1]) & time_both[,1] != ""
    time_both = subset(time_both, keep)
    output$timetable <- DT::renderDataTable(time_both)
  }
  
}

  # on button click, toggle modal and autogen only when autogen is TRUE (i.e., on first time only) 
  observeEvent(input$autoAnalysis,{

    this = values.edit$table
    if (is.null(this)) return(NULL)

    if (!values.edit$autogen) {
	# use last saved values
  	updateSelectizeInput(session,"autoColumnTime",choices=colnames(this),
		selected=KM$time.col)
  	updateSelectizeInput(session,"autoColumnOutcome",choices=colnames(this),
		selected=KM$outcome.col)

	events = as.character(unique(this[[KM$outcome.col]]))
  	updateSelectizeInput(session,"columnEvent1",choices=events,
		selected=KM$eventYes,server=TRUE)
  	updateSelectizeInput(session,"columnEvent0",choices=events,
		selected=KM$eventNo,server=TRUE)
	
        toggleModal(session,"autogenModal",toggle="open")
	return(NULL)
    }

    columns.data = calc.columns(this)
    main.gen(this,columns.data)  
    toggleModal(session,"autogenModal",toggle="open")
    
  })
 

  # display time table when time column is updated 
  observeEvent(input$autoColumnTime,({
    shinycat("observe autoColumnTime...\n")
    this = values.edit$table
    if (is.null(this)) return(NULL)
    if (input$autoColumnTime == "") return(NULL)
    if (input$autoColumnOutcome == ""){
      shinyjs::disable("genBtn")
    }
    else{
      shinyjs::enable("genBtn")
    }
    
    #new = reduce.columns(input$autoColumnTime,NA,this)
    #if (length(new$time) == 0) return(NULL)


    time_both <- data.frame("TimeColumnOriginal" = this[[input$autoColumnTime]],"TimeColumnFormatted" = time.analysis()$time)

    rownames(time_both) <- rownames(this)

    # remove columns with no data
    keep = !is.na(time_both[,1]) & time_both[,1] != ""
    time_both = subset(time_both, keep)
    output$timetable <- DT::renderDataTable(time_both)
  }))
  
 
  observeEvent(input$autoColumnOutcome,({
    shinycat("observe autoColumnOutcome...\n")
    if (is.null(values.edit$table)) return(NULL)
    this = values.edit$table
    selected = input$autoColumnOutcome
    if (selected == "") return(NULL)
    if (input$autoColumnTime == ""){
      shinyjs::disable("genBtn")
    } else{
      shinyjs::enable("genBtn")
    }
    
    selected = setdiff(selected, c("", " "))
    outcome.orig = as.character(this[[input$autoColumnOutcome]])
    outcome.new = outcome.01(input$autoColumnOutcome, this) 
    outcome.no = unique(outcome.orig[outcome.new == 0])
    outcome.yes = unique(outcome.orig[outcome.new == 1])
    columnItems = as.character(unique(this[[selected]]))

    updateSelectizeInput(session,"columnEvent1",choices=columnItems,selected=outcome.yes,server=TRUE)
    updateSelectizeInput(session,"columnEvent0",choices=columnItems,selected=outcome.no,server=TRUE)
    
  }))
  
  
  observeEvent(input$genBtn,
               ({
                 shinycat("observe genBtn...\n")
		 KM$generated <- TRUE
 	         closeAlert(session, alertId = "SelectKM")
    	         values.edit$autogen <- FALSE
		 KM$time.col = isolate(input$autoColumnTime)
		 KM$outcome.col = isolate(input$autoColumnOutcome)
		 KM$eventNo = isolate(input$columnEvent0)
		 KM$eventYes = isolate(input$columnEvent1)

                 if (is.null(values.edit$table)) return(NULL)
                 output$kmSurvival <- renderPlot({
                   main = paste(input$GSE, geneLabel() , sep = ": ")

                   if (input$autoColumnOutcome == "") return(NULL)

                   outcome.orig = values.edit$table[[KM$outcome.col]]
                   outcome.analysis = rep(NA, length(outcome.orig))
                   outcome.analysis[outcome.orig%in%KM$eventNo] = 0
		   outcome.analysis[outcome.orig%in%KM$eventYes] = 1  
			
                   hr.inverse = FALSE
		   if(KM$hr.format == "low/high") {
				hr.inverse = TRUE
	    	   }

                   time = isolate(time.analysis()$time)
                   death = as.integer(outcome.analysis)
		   x = probe.expr()

		   common = intersect(names(x), rownames(values.edit$table))		 
		   m1 = match(common, names(x))
                   m2 = match(common, rownames(values.edit$table))
		
		   x = x[m1]
                   time = time[m2]
                   death = death[m2]

                   return(plot.shiny.km(time = time, death = death, x = x, 
                                        col = KM$col, title = main,
					xlab = KM$xlab, ylab = KM$ylab,
					hr.inverse = hr.inverse))
                 })
                 closeAlert(session,"warn1")
                 closeAlert(session,"warn2")
                 closeAlert(session,"warn3")
                 toggleModal(session,"autogenModal",toggle = "toggle")
                 
                 tags$script(HTML("window.location.href= '/#kmSurvial'"))
               })
  )
  

