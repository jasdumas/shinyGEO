####################################################################
## respond to Merge Groups button, which opens modal
####################################################################

observeEvent(input$mergeGroupsButton, ({
  closeAlert(session, "merge-alert")
  content = "Merge two or more groups together by selecting the groups from the drop-down boxes on the left, and specifying a name for the new group in the corresponding text boxes on the right. Click on Save, and a new column will be added to the clinical data table."
add = paste0("<p><p> Selected column: <strong>", input$selectedColumn, "</strong></p>.")
 
content = paste0(content, add)
    
  createAlert(session, "mergeGroupsAlert", alertId = "merge-alert", title = "Current Status: Merging", style = "info", content = content)
 })
)

####################################################################
## renders drop-down menus (server-side) for clinical group 
## selection for merging groups in MergeGroupsModal
####################################################################

output$mergeGroup1 <- renderUI({
    selectInput('selectGroup1', 'Group 1', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = NULL,
              width='80%',
              selectize = TRUE
    )
})

output$mergeGroup2 <- renderUI({
    selectInput('selectGroup2', 'Group 2', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = NULL,
              width='80%',
              selectize = TRUE
    )
})

output$mergeGroup3 <- renderUI({
    selectInput('selectGroup3', 'Group 3', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = NULL,
              width='80%',
              selectize = TRUE
    )
})

####################################################################
## respond when Save button is clicked on MergeGroups modal
####################################################################
observeEvent(input$applyMergeGroups, ({
  shinycat("Merging groups...\n")
  content = ""
  col = input$newColumnForMerge
  
  g1 = input$selectGroup1
  g2 = input$selectGroup2 
  g3 = input$selectGroup3 

  g1 = g1[g1!=""]
  g2 = g2[g2!=""]
  g3 = g3[g3!=""]

  g.all = c(g1,g2,g3)

  if (length(g.all) > length(unique(g.all))) {
        content = paste0(content, 
		"<p> Error: A value cannot appear in multiple groups <p>")
  }

  if (col%in%colnames(values.edit$table)) {
        content = paste0(content, 
		"<p> Error: Column Name Exists. Please select a new column name <p>")
  }

  if (content!= "") {
  createAlert(session, "mergeGroupsAlert", alertId = "merge-alert-error", title = "Save Error", style = "danger", content = content, append = TRUE)
    return(NULL)
  }

  X = as.character(values.edit$table[[input$selectedColumn]])
  Y = rep("", length(X))
  
  add1 = "## merge groups from selected column ##\n"
  add1 = paste0(add1, "tmp = as.character(data.p[[\"", input$selectedColumn, "\"]])\n")
  add1 = paste0(add1, "Y = rep(\"\", length(tmp))\n") 

  if (length(g1) > 0 & input$group1Label != "") {
	Y[X %in% g1] = input$group1Label 
	add1 = paste0(add1, "Y[tmp %in% ", vector.it(g1), "] = \"", input$group1Label, "\"\n")  
  }
  if (length(g2) > 0 & input$group2Label != "") { 
        Y[X %in% g2] = input$group2Label 
	add1 = paste0(add1, "Y[tmp %in% ", vector.it(g2), "] = \"", input$group2Label, "\"\n")  
  } 
  if (length(g3) > 0 & input$group3Label != "") {
        Y[X %in% g3] = input$group3Label  
	add1 = paste0(add1, "Y[tmp %in% ", vector.it(g3), "] = \"", input$group3Label, "\"\n")  
  }
  data = values.edit$table
  data[[col]] = Y

  add1 = paste0(add1, "data.p[[\"", col, "\"]] = Y")
  isolate(add.code(add1))

  isolate(values.edit$table <- data)
  toggleModal(session, "MergeGroupsModal", "close")
  updateSelectInput(session, "selectedColumn", choices = ColumnNames(),
       selected = col)
  })
)

