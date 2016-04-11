createAlert(session,"selectionAlert1",content = "<H4>Directions</H4><p> Select a subset of samples to analyze by specifying selection criteria using the drop down boxes below, and then click the 'Apply Selection Criteria' button. <br>For more advanced selections, export and update the data table using the 'Data Export' option)</br></p>", dismiss=FALSE) 


shinyjs::disable("btnSelection")

## drop downs for sample selection  1##
observe({
  
  # reset on tab change
  input$tabs
   
  colNames = rownames(clinicalDataSummary())
  colNames = c("Select column..." = "", colNames)
  output$SampleSelectionCol1 <- renderUI({
      selectInput('SampleSelectionCol1', 'Selected Column',
            choices = colNames, #width='20%',
            selected = NULL, multiple = FALSE, selectize = FALSE
    )
  })

})

observe({

  # reset on tab change
  input$tabs

  vars = isolate(values.edit$table)
  if (is.null(vars)) {
        return(NULL)
  }

  if (is.null(input$SampleSelectionCol1)) {
	output$SampleSelection1 = renderUI({})
	return(NULL)
  } else if (input$SampleSelectionCol1 == "") {
	output$SampleSelection1 = renderUI({})
	return(NULL)
  }

  s1 = vars[,as.character(input$SampleSelectionCol1)]
  s1 = unique(as.character(s1))

  output$SampleSelection1 <- renderUI({
      selectInput('SampleSelection1', 'Selection Criteria',
            choices = s1, #width='20%',
            selected = NULL, multiple = TRUE, selectize = TRUE
    )
  })

})

observeEvent(input$SampleSelection1, {
  if (!is.null(input$SampleSelection1)) {
  	shinyjs::enable("btnSelection")
  } else {
	shinyjs::disable("btnSelection")
	return(NULL)
  }
})
## drop downs for sample selection 2 ##


observe({

  # reset on tab change
  input$tabs

  x = input$SampleSelectionCol1
  if (is.null(x)) {
 	output$SampleSelectionCol2 = renderUI({})
 	output$SampleSelection2 = renderUI({})
	return(NULL)
  }
  if (x == "") {
 	output$SampleSelectionCol2 = renderUI({})
 	output$SampleSelection2 = renderUI({})
	return(NULL)
  }

  colNames = rownames(clinicalDataSummary())
  colNames = c("Select another column..." = "", colNames)
  output$SampleSelectionCol2 <- renderUI({
      selectInput('SampleSelectionCol2', 'Selected Column',
            choices = colNames, #width='20%',
            selected = NULL, multiple = FALSE, selectize = FALSE
    )
  })

})







observe({

  # reset on tab change
  input$tabs

  vars = isolate(values.edit$table)
  if (is.null(vars)) {
        return(NULL)
  }

  x = input$SampleSelectionCol2
  if (is.null(input$SampleSelectionCol2)) {
	return(NULL)
  } else if (input$SampleSelectionCol2 == "") {
	return(NULL)
  }

  s2 = vars[,as.character(input$SampleSelectionCol2)]
  s2 = unique(as.character(s2))

  output$SampleSelection2 <- renderUI({
      selectInput('SampleSelection2', 'Selection Criteria',
            choices = s2, #width='20%',
            selected = NULL, multiple = TRUE, selectize = TRUE
    )
  })

})

shinyjs::onclick("btnSelection", {


  if (is.null(input$SampleSelection1)) {
	return(NULL)
  }

  if (is.null(input$SampleSelectionCol1)) {
	return(NULL)
  }

  shinyjs::enable("ClinicalReset")

  t = isolate(values.edit$table)
  keep = t[[input$SampleSelectionCol1]] %in% input$SampleSelection1

  code = paste0("## sample selection ##\nkeep = data.p[[\"", input$SampleSelectionCol1, "\"]] %in% ", 
	     vector.it(input$SampleSelection1))
  sel1 = paste0(input$SampleSelection1, collapse = "  OR  ") 
  sel1 = paste0(input$SampleSelectionCol1, ":&nbsp &nbsp &nbsp ", sel1) 
  sel2 = NULL
  if (!is.null(input$SampleSelection2)) {
  	keep = keep & t[[input$SampleSelectionCol2]] %in% input$SampleSelection2
	code = paste0(code, "\n",
  	    "keep = keep & data.p[[\"", input$SampleSelectionCol2, "\"]] %in% ",
	    vector.it(input$SampleSelection2))
  	sel2 = paste0(input$SampleSelection2, collapse = "  OR  ") 
  	sel2 = paste0(input$SampleSelectionCol2, ":&nbsp &nbsp &nbsp ", sel2) 
  }

  add.code(code)
  add.code("data.p = data.p[keep,drop = FALSE]")

 if (!is.null(sel2)) {
	sel1 = paste0(sel1, "<br>", sel2, "</br>")
  }


  values.edit$table <- t[keep,] 
  
  content = paste0("<H4>Selection Criteria Applied (", sum(keep), " samples selected)</H4><p>", sel1, "<p>",
	           "To clear selection criteria, select 'Reset' under the 'Data Export' tab</br>" ) 
  createAlert(session,"selectionAlert2",content = content, dismiss = FALSE, style = "danger", append = FALSE) 

 shinyjs::disable("btnSelection")
 
})



