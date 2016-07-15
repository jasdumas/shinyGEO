library(stringr)

createAlert(session, "warningAlert", "survInstructions", title = "Survival Analysis", content = "<i>shinyGEO</i> will attempt to detect the time and outcome columns from the sample data. Please confirm the selection and then generate the KM plot by clicking on the button above. The KM plot will generate survival curves for samples with high expression to samples with low expression, using the median expression value as the cutoff. To use the best cutoff instead, select this option under the 'Survival analysis options' menu below ", style = "success", dismiss = TRUE) 

#Auto-Generation of columns
## Functions for autogen
##
calc.columns <- function(this){
  # First need to grep the first row of the data, then lapply a function that will return true for
  time.pattern = c("distant-relapse free survival","time","survival \\(mo\\)", "survival month", "survival \\(months\\)","survival months")
  outcome.pattern = c("distant-relapse event","outcome","dead of disease","dss censor","os censor","overall survival", "cancer specific survival", "survival")
  
  is.time.column <- function(x){
    ans = grepl(paste(time.pattern,collapse="|"),x, ignore.case = TRUE)
    if(any(ans)){
      return(TRUE)
    }
    return(FALSE)
  }
  is.outcome.column <- function(x){
    ans = grepl(paste(outcome.pattern,collapse="|"),x, ignore.case = TRUE)
    # not an outcome if contains 'month'
    ans2 = grepl("month",x, ignore.case = TRUE)
    ans = ans & !ans2
    if(any(ans)){
      return(TRUE)
    }
    return(FALSE)
  }
  x.time = colnames(this)[apply(this,2,is.time.column)]
  y.outcome = colnames(this)[apply(this,2,is.outcome.column)]

 have.time = length(x.time) > 0
 have.outcome = length(y.outcome) > 0

  if(length(x.time) > 1){
    createAlert(session, "warningAlert", alertId = "warn1", title = "Warning: multiple time columns found",
                content = paste(c("<strong>Columns found</strong>: ", paste(x.time,collapse=", "),"<br>Please check that the selection is correct.")), style= 'danger', dismiss = TRUE, append = TRUE)
    x.time = x.time[1]
  }

  if(length(y.outcome) > 1)
  {
   cat("multiple outcomes...\n")
    createAlert(session, "warningAlert", alertId = "warn2", title = "Warning: multiple outcome columns Found",
                content = paste(c("<strong>Columns Found</strong>: ", paste(y.outcome,collapse=", "),"<br>Please check that the selection is correct.")), style= 'danger', dismiss = TRUE, append = TRUE)
    y.outcome = y.outcome[1]
  }


  title = NULL
  if (!have.time & !have.outcome) {
	title = "Warning: No time or outcome columns were found"
  } else  if(!have.time & have.outcome){
	title = "Warning: No survival time columns were found"
  }
  else if(!have.outcome & have.time){
	title = "Warning: No survival outcome columns were found"
  }

  if (!is.null(title)) {
   content = c("<p>Oops! <i>shinyGEO</i> could not find one or more columns for survival analysis in your data. Please try the following: <ul><li>View the table and select the relevant columns </li><li>If necessary, manually format the data by exporting the data, reformatting, and uploading your data back into <i>shinyGEO</i>.</li><li> Note that complete survival information is not available in all datasets.</ul></p>") 
    createAlert(session, "warningAlert", alertId = "warn3", title = title, content = content, 
		style= 'danger', dismiss = TRUE, append = TRUE)
  }
 
  if (!have.time) x.time = NA
  if (!have.outcome) x.outcome = NA
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
	time = suppressWarnings(as.double(reduce(this[[input$autoColumnTime]])))	

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

outcomeChoices <-reactive({

    

})


main.gen <- function(this,columns.data){
  #Reduce and analyze
  # update inputs for time and outcome columns
  
  updateSelectizeInput(session,"autoColumnTime",choices=colnames(this),
	selected=columns.data[1])
  updateSelectizeInput(session,"autoColumnOutcome",choices=colnames(this),
	selected=columns.data[2])
  new = reduce.columns(columns.data[1],columns.data[2],this)

  if (sum(!is.na(new$outcome)) > 0) { 
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

  if (sum(!is.na(new$time)) > 0) {
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

	updateRadioButtons(session, "radioCutoff", 
			  label = "Cutoff selection to separate high and low expressors",
                          choices = c("Median", "Auto select best cutoff"), inline = TRUE,
			  selected = KM$cutoff)
	
        toggleModal(session,"autogenModal",toggle="open")
	closeAlert(session, "survInstructions")
	return(NULL)
    }

    columns.data = calc.columns(this)
    main.gen(this,columns.data)  
    toggleModal(session,"autogenModal",toggle="open")
    
  })

  ###########################################################
  # sets TimeTable filtering times based on selected events 
  ###########################################################
  setTimeTable <-reactive({

    shinycat("setTimeTable...\n")
    this = values.edit$table
    if (is.null(this) | input$autoColumnTime =="") return(NULL)
    shinycat("getting times..\n")
    time_both <- data.frame("TimeColumnOriginal" = this[[input$autoColumnTime]],"TimeColumnFormatted" = time.analysis()$time)

    rownames(time_both) <- rownames(this)

    # only show relevant times based on selected events #
    selected = input$autoColumnOutcome
    keep = rep(TRUE, nrow(time_both))
    if (selected!="") {
      no = input$columnEvent0
      yes = input$columnEvent1
      events = c(yes, no)
      if (!is.null(events)) keep = keep & this[[selected]]%in% events 
    }

    # remove columns with no data
    keep = keep & !is.na(time_both[,1]) & time_both[,1] != ""
    time_both = subset(time_both, keep)
    output$timetable <- DT::renderDataTable(time_both)
  })
 

  # display time table when time column is updated 
  observeEvent(input$autoColumnTime,({
    shinycat("observe autoColumnTime...\n")
    this = values.edit$table
    if (is.null(this)) return(NULL)
    if (input$autoColumnTime == "") {
	time_both = data.frame(TimeColumnOriginal = NULL, TimeColumnFormatted = NULL)
    	output$timetable <- DT::renderDataTable(time_both)
        shinyjs::disable("genBtn")

	return(NULL)
    }

    if (input$autoColumnOutcome!="") {
	Y = input$columnEvent1
	N = input$columnEvent0
	if (is.null(N) | is.null(Y)) {
		closeAlert(session, "warnSelect")
	} else if (!is.null(N) & N[1] == "" | !is.null(Y) & Y[1] == "") {
		closeAlert(session, "warnSelect") 
	}
    }
    if (input$autoColumnOutcome == ""){
      shinyjs::disable("genBtn")
    } else if (!is.null(input$columnEvent1) & !is.null(input$columnEvent0)) {
       shinyjs::enable("genBtn")
    }
    
    setTimeTable()


  }))
  
 
  observeEvent(input$autoColumnOutcome,({
    shinycat("observe autoColumnOutcome...\n")
    if (is.null(values.edit$table)) return(NULL)
    this = values.edit$table
    setTimeTable()
    selected = input$autoColumnOutcome
    if (selected == "") {
	shinyjs::disable("genBtn")
	shinyjs::hide("columnEvent1")
	shinyjs::hide("columnEvent0")
	return(NULL)
    }
    if (input$autoColumnTime!="") closeAlert(session, "warnSelect")
    shinyjs::show("columnEvent1")
    shinyjs::show("columnEvent0")
 
    selected = setdiff(selected, c("", " "))
    outcome.orig = as.character(this[[input$autoColumnOutcome]])
    outcome.new = outcome.01(input$autoColumnOutcome, this) 
    outcome.no = unique(outcome.orig[outcome.new == 0])
    outcome.yes = unique(outcome.orig[outcome.new == 1])
    columnItems = as.character(unique(this[[selected]]))

    updateSelectizeInput(session,"columnEvent1",choices=columnItems,selected=outcome.yes,server=TRUE)
    updateSelectizeInput(session,"columnEvent0",choices=columnItems,selected=outcome.no,server=TRUE)

    if (!is.null(input$columnEvent1) & !is.null(input$columnEvent0)) {
	    shinyjs::enable("genBtn")
    }

  }))
  
  observe({
	if (!is.null(input$columnEvent1) & !is.null(input$columnEvent0) &
            input$autoColumnTime != "" & input$autoColumnOutcome != "") {
	    closeAlert(session, "warnSelect")
            shinyjs::enable("genBtn")
	    if (length(intersect(input$columnEvent0, input$columnEvent1)) == 0) {
		  closeAlert(session, "warnYesNo")	
	    }	
		

		
    	    setTimeTable()    
	} else if (!values.edit$autogen)  {
          createAlert(session, "warningAlert", alertId = "warnSelect", title = "Time and Outcome Selection",
                content = "Please select an appropriate time and outcome column, and event values.", style= 'danger', dismiss = TRUE, append = TRUE)
          shinyjs::disable("genBtn")
	  if (is.null(input$columnEvent1) | is.null(input$columnEvent0)) {
		setTimeTable()
	  }
	}
  })

  kmReactive <- reactive({
	shinycat("in kmReactive...\n")
  	outcome.orig = values.edit$table[[KM$outcome.col]]
        outcome.analysis = rep(NA, length(outcome.orig))
        outcome.analysis[outcome.orig%in%KM$eventNo] = 0
	outcome.analysis[outcome.orig%in%KM$eventYes] = 1  
			
        time = time.analysis()$time
        death = as.integer(outcome.analysis)
	x = probe.expr()
	ids = names(x)
	
	common = intersect(names(x), rownames(values.edit$table))		 
	m1 = match(common, names(x))
        m2 = match(common, rownames(values.edit$table))
		
	x = x[m1]; ids = ids[m1]
        time = time[m2]
        death = death[m2]
	return(data.frame(id = as.character(ids), x = x, time = time, death = death))
  })

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
		 KM$cutoff = isolate(input$radioCutoff)

		 time = KM$time.col
	         outcome = KM$outcome.col
		 yes = KM$eventYes; no = KM$eventNo

		 check = intersect(yes, no)
		 if (length(check) > 0) {
          		createAlert(session, "warningAlert", alertId = "warnYesNo", title = "Event Selection Error",
                content = "At least one value was selected for both Event:Yes and Event: No. Make sure that the Yes and No events are distinct.", style= 'danger', dismiss = TRUE, append = TRUE)
          shinyjs::disable("genBtn")
			return(NULL)

		 }

                 if (is.null(values.edit$table)) return(NULL)
                 output$kmSurvival <- renderPlot({
			# display wait message in plot #
			plot(1:10,xaxt = "n", yaxt = "n", xlab = "", 
				ylab = "", type = "n")
			legend("center", "Generating KM curves,\nplease wait...", 
				box.lwd = 0, cex =2)

		   createAlert(session, "warningAlert", alertId = "alertWait1", title = "Status", content = "Generating KM curve, please wait...", style = "info", append = TRUE, dismiss = FALSE)


                   if (isolate(input$autoColumnOutcome == "")) return(NULL)
		   if (is.null(input$selectGenes)) return(NULL)

		   km = isolate(kmReactive())
		   if (is.null(km$x)) return(NULL) 
                   main = paste(input$GSE, geneLabel() , sep = ": ")

	 	   hr.inverse = FALSE
		   if(KM$hr.format == "low/high") {
				hr.inverse = TRUE
	    	   }

		   optimal.cut = TRUE
		   if (KM$cutoff == "Median") optimal.cut = FALSE 

                   res = plot.shiny.km(time = km$time, death = km$death, x = km$x,  
                                        col = KM$col, title = main,
					xlab = KM$xlab, ylab = KM$ylab,
					hr.inverse = hr.inverse, optimal.cut = optimal.cut)
		   closeAlert(session, "alertWait1")

		  if (!is.null(res)) {
		     shinyjs::show("Survadd")
		     shinyjs::show("downloadKM")
		     shinyjs::show("formatDEButton2")
		     closeAlert(session, "kmAlert")	
		  } else {

		     shinyjs::hide("Survadd")
		     shinyjs::hide("downloadKM")
		     shinyjs::hide("formatDEButton2")
		     if (!is.null(input$selectGenes) & input$selectGenes!="") {
		        plot(1:10, type = 'n', xaxt = 'n', yaxt = 'n', 
			     lwd = 0, ylab = "", xlab = "", bty = 'n')
                	content = "<b>Error</b>: Survival analysis could not be completed using the selected time and outcome columns for the selected probe. This is typically because the selected columns do not contain survival information. Click on the Select Time/Outcome button to select the appropriate columns, or choose another probe. Note that survival information is not available for all datasets."
			
			if (KM$cutoff != "Median") {
				content = paste0(content, " Alternatively, the survival data may be unbalanced (e.g., 99% of individuals are censored). If this is the case, the Median cutoff must be used.")
			}

    		       createAlert(session, "alert2", alertId = "kmAlert", 
			title = "Survival Analysis",
			content = content,
			style= 'danger', dismiss = TRUE, append = TRUE)
		       }
	             }
                 })

                 #closeAlert(session,"warn1")
                 #closeAlert(session,"warn2")
                 #closeAlert(session,"warn3")
                 toggleModal(session,"autogenModal",toggle = "toggle")
               })
  )
  

