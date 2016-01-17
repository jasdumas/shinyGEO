#######################################################
# from server-reactives.R
########################################################

## reactives for textboxes/drop-downs in modal window
find.str <- reactive({input$find})       
replace.str <- reactive({input$replace})
column.num <- reactive({as.character(input$drop2)}) 

editClinicalTable <- reactive({
  input$Enter  
  add.tab()  
  if (TRACE) cat("In editClinicalTable reactive...\n")
  find.str = isolate(find.str())
  column.num = isolate(column.num())
  replace.str = isolate(replace.str())
  
  if (find.str == "" & replace.str == "") {   # if there is nothing entered it returns the original table
    cat("\t return current table\n")
    subtract.tab()
    return(values.edit$table)
  }
  exactMatch = isolate(input$checkbox) # exact match condition
  
  if (exactMatch) {    # while default is false
    find.str = paste("^", find.str, "$", sep = "")
  }
  
  newClinical <- values.edit$table
  
  cat("\t set newClinical\n")
  ### if factor, change to character.  Otherwise we can't replace it. ##
  if (is.factor(newClinical[,column.num])) {
    newClinical[,column.num] = as.character(newClinical[,column.num])
  }
  
  partialReplace = isolate(input$survCheckbox)
  
  if (partialReplace) {                 
    newClinical[[column.num]] = gsub(find.str, replace.str, newClinical[[column.num]])  ## fixed = T for symbols
    g.total = grep(find.str, newClinical[,column.num])  
    newClinical[g.total,column.num] = replace.str 
    cat("replacing ", find.str, " with ", replace.str)
  } else {
    g.total = grep(find.str, newClinical[,column.num])  
    cat("g.total = ", g.total, " \n")
    newClinical[g.total,column.num] = replace.str 
    cat("replacing ", find.str, " with ", replace.str)
  }

  cat("replacing table..\n")    
  values.edit$table = newClinical
  subtract.tab()
  return (values.edit$table)
  
}) # end of editClinicalTable() reactive

#######################################################
# from ui.R 
########################################################
        actionButton("tabBut", "Edit Data Table"),


        shinyBS::bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
            uiOutput("dropModal"),
            textInput("find", label = "Find", value = ""),
            checkboxInput("checkbox", label = "Exact Match", value = FALSE),
            textInput("replace", label = "Replace", value = ""),
            checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
             actionButton("Enter", label = "Submit"))

