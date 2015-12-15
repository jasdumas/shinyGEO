
###########################################################################
# clinicalDataProcessed: processes clinical data in values.edit$table,
# by removing non-informative columns.   
##########################################################################
clinicalDataProcessed <- reactive({

  add.tab()
  if (TRACE) cat("In clinicalDataProcessed reactive...\n")

  p = values.edit$table
  if (is.null(p)) {
	return(NULL)
  } 
  #####################################################################
  #  display only columns that have more than one possible value; this
  #   removes many columns such as contact info. In addition all 
  #   columns specified by RM.COLS will be removed
  #####################################################################
  
  RM.COLS = c("status", "last_update_date", "submission_date", 
	"supplementary_file", "geo_accession")
  num.levels = apply(p, 2, function(x) nlevels(as.factor(x)))
  
  p = p[,num.levels > 1]
  
  m = match(RM.COLS, colnames(p))
  
  m=m[!is.na(m)]
  
  if (length(m) > 0) p=p[,-m, drop = FALSE]
  
  m = match(colnames(exprInput()), rownames(p))
  
  p = p[m,]
  
  values.edit$table = p
  subtract.tab()	
  return(p)

})


########################################
### Summary of Clinical Data table
########################################  
clinicalDataSummary <- reactive({
  add.tab()
  if (TRACE) cat("In clinicalDataSummary reactive...\n")
  t = clinicalDataProcessed()
  if (is.null(t)) {
	subtract.tab()
	return(NULL)
  }
  cat("colnames = ")
  vars = colnames(t)
  cat("got ncols = ", length(vars), "\n")
  a = apply(t, 2, function(x)levels(as.factor(x)))
  
  ## format function to truncate row contents with a place holder " ..."
  format.it <-function(x, max) {
    x = x[x!=""]
    if (length(x) <= max) return(x)
    x[max] = " ..."
    return(x[1:max])
  }
  a = lapply(a, format.it, Inf)
  
  a = sapply(a, paste, collapse = ", ")
  cat("end clinicalDataSummary reactive\n")
  subtract.tab()
  cbind(column = vars, values = a)
})

########################################
### ColumnNames of clinicial data table
########################################  
ColumnNames <- reactive({
  add.tab()
  if (TRACE) cat("In ColumnNames reactive...\n")
  if (is.null(clinicalDataProcessed()) | is.null(exprInput())) {
	subtract.tab()
	return(NULL)
  }
  vars = colnames(clinicalDataProcessed())
  vars <- as.list(vars)
  subtract.tab()
  return(vars)
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
  
})

observe ({
  output$clinicalData = displayDataTable()
})


