
###########################################################################
# clinicalDataProcessed: processes clinical data in values.edit$table,
# by removing non-informative columns.   
##########################################################################
clinicalDataProcessed <- reactive({

  add.tab()
  if (TRACE) cat("In clinicalDataProcessed reactive...\n")

  p = values.edit$table
  if (is.null(p)) {
 	subtract.tab()
	return(NULL)
  } 
  #####################################################################
  #  display only columns that have more than one possible value; this
  #   removes many columns such as contact info. In addition all 
  #   columns specified by RM.COLS will be removed
  #####################################################################
  # show columns that have unique values; or display all if none 
  num.levels = apply(p, 2, function(x) nlevels(as.factor(x)))
  i = num.levels > 1

  # keep source_name_ch1 
  
  keep = colnames(p) %in% "source_name_ch1"
  if (sum(keep) > 0) {
	i[keep] = TRUE
  }

  if (sum(i) <= 1) {
	i = 1:ncol(p)	
  }
  p = p[,i, drop = FALSE]

  cat("removed dups\n")
 
  ## remove non-informative columns; but keep all if all columns would 
  ## be removed  
  RM.COLS = c("status", "last_update_date", "submission_date", 
	"supplementary_file", "geo_accession")
  m = match(RM.COLS, colnames(p))
  m=m[!is.na(m)]
  if (length(m) == ncol(p)) {
	subtract.tab()
	return(p)
  } 
  
   
  if (length(m) > 0) p=p[,-m, drop = FALSE]

  cat("RM.COLS removed\n")  

  m = match(colnames(exprInput()), rownames(p))
  m = m[!is.na(m)]

  if (sum(m) == 0) {
	values.edit$table = p
	return (p)
  }
  p = p[m,,drop = FALSE]
  
  values.edit$table = p
  if (TRACE) cat("END clinicalDataProcessed reactive...\n")
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
  vars = colnames(t)
  cat("got ncols = ", length(vars), "\n")
  a = apply(t, 2, function(x)levels(as.factor(x)))

  ## if there are no duplicates in each row, the above returns the original table
  ## therefore, make duplicates if necessary
  if (!is.null(nrow(a))) {
    tmp = rep(1,nrow(t))
    tmp[1] = 2
    t$DELETE=tmp
    a = apply(t, 2, function(x)levels(as.factor(x)))
    a$DELETE = NULL
  } 
  
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


observe({
  output$platformData <- DT::renderDataTable({ datatable(as.data.frame(platInfo()), rownames = FALSE,  
                                 # extensions = 'ColReorder',
   				  options = list(dom = 'Rlfrtip', 
                                 	paging = TRUE, scrollY = "400px", autoWidth = TRUE,
                                  	searchHighlight = TRUE, scrollX = "auto"
                          	  ),
                          	  filter = 'none', 
                          	  selection = 'none') 
  })
})

############################################
## displays the Clinical Summary Data Table
###########################################
observe({  # observe needed since data object is a reactive function
  cat("observe for clinicalDataSummary\n") 

  output$clinicalDataSummary <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()), rownames = TRUE,  
                                                                 extensions = 'ColReorder',
                                                                 options = list(#dom = 'Rlfrtip', ajax = list(url = action), 
                                                                                paging = F,  searchHighlight = TRUE,
										autoWidth = TRUE, scrollY = "400px"),
                                                                 filter = 'none', 
                                                                 selection = 'single') 
    
  })


  output$clinicalDataSummarySummary <- DT::renderDataTable({ datatable(as.data.frame(clinicalDataSummary()[,-1, drop = FALSE]), rownames = TRUE,  
                          options = list(dom = 'Rlfrtip',  
 	                         paging = F, scrollY = "400px",
				  searchHighlight = TRUE,
				  columnDefs = list(list(
                                  	targets = 1,#: ncol(clinicalDataSummary()[-1, drop = FALSE]), # applies to the entire table
                                        #width = "200px",
                                        render = JS(
                                        	"function(data, type, row, meta) {",
                                                "return type == 'display' && data.length > 150 ?",
                                                "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
                                        "}")
                                   ))
			  ),
			  filter = 'none',
			  caption = HTML("<b> Summary of clinical data </b>"), 
                          selection = 'none') 
    
  })


 


 
})

###########################################################################################
## Reactive for displaying the dataTable, since same display will be used multiple times
##########################################################################################
displayDataTable <-reactive({
  add.tab()
  depend = values.edit$table
  cat("in displayDataTable reactive...\n") 
  t = DT::renderDataTable({ datatable(clinicalDataProcessed(), rownames = TRUE,
       #                                               extensions = 'ColReorder',
                                                      options = list(dom = 'Rlfrtip', #ajax = list(url = action1), 
								    autoWidth = TRUE,
                                                                     scrollX = "auto",
                                                                     scrollY = "400px",
                                                                     paging = F, 
                                                                     searchHighlight = TRUE,
                                                                     columnDefs = list(list(
                                                                       targets = 1: ncol(clinicalDataProcessed()), # applies to the entire table
									width = "200px",
                                                                        render = JS(
                                                                         "function(data, type, row, meta) {",
                                                                         "return type == 'display' && data.length > 30 ?",
                                                                         "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                                                         "}")
                                                                     ))), 
                                                      select = list(target = "column"),
                                                      filter = 'none')
  })
cat("end displayDataTable reactive\n")
subtract.tab()

return(t)
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
  cat("observe for clinicalData\n")
  output$clinicalData <- displayDataTable()
})


