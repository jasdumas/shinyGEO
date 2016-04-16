
createAlert(session,"ioAlert1",content = "<H4>Directions</H4><p>1. Download the current clinical data you are working with, it will be saved in your 'Downloads' folder.<br>2. Edit the dataset, then save your changes.<br>3. Then upload your dataset back to shinyGEO.</p>",dismiss=FALSE)


shinyjs::onclick("ClinicalReset", {
  if (TEST.DATA) {
        values.edit$table <- CLINICAL.test
    } else {
      values.edit$table <- as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
    }
  createAlert(session, "ioAlert2", content = "<h4>Clinical Data has been reset</h4>", style = "success", dismiss = FALSE, append = FALSE)

  closeAlert(session, "selectionAlert2")
  createAlert(session, "selectionAlert2", content = "All samples currently selected", append = FALSE)
  shinyjs::disable("btnSelection")

  add.code("\n## Restoring original sample data table ##")
  add.code("data.p = pData(data.series[[data.index]])")
  add.code("data.expr = exprs(data.series[[data.index]])")
  add.code("common = intersect(colnames(data.expr), rownames(data.p))")
  add.code("m1 = match(common, colnames(data.expr))")
  add.code("m2 = match(common, rownames(data.p))")
  add.code("data.expr = data.expr[,m1]")
  add.code("data.p = data.p[m2,]")

  if (values.edit$log2) {
        add1  = "data.expr[which(data.expr <= 0)] <- NaN"
        add2 = "data.expr = log2(data.expr)"
        add.code(add1)
        add.code(add2)
  }

})

# start clinical data iotab button events
output$downloadSet<- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      file = paste(input$GSE,"_",input$platform,"_",Sys.time(),"-clinical", ".csv", sep = "")
	file = gsub(":", "-",file)
  msg = paste0("<H4>Current Status</H4><p><strong>The clinical data has been downloaded to the following file: ", file, "</p>")
  createAlert(session,"ioAlert2",content = msg, style="success",dismiss=FALSE, append = FALSE)
        return(file)
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

vector.it <-function(x) {
  x = paste0("\"", x, "\"", collapse = ",")
  paste0("c(", x, ")")
}

observeEvent(input$fileUpload, {
  shinycat("in file upload observe...\n")  
  infile <- input$fileUpload
  if (!is.null(infile)){
    createAlert(session,"ioAlert2",content = "Processing file upload, please wait...", style="info",dismiss=FALSE, append = FALSE)

    data = try(read.table(infile$datapath, header = TRUE, row.names=1, sep = ","), silent = TRUE)
    if (class(data) %in% "try-error") {
       createAlert(session,"ioAlert2",content = "Error: file could not be uploaded. This is most likely because the file is not in the correct format (e.g., is not a csv file)" , style="danger",dismiss=TRUE, append = FALSE)
	return(NULL)
    }

    # make sure we have row names #
    check = rownames(data)%in%colnames(exprInput())

    if (!any(check)) {
       createAlert(session,"ioAlert2",content = "Error: uploaded file must include sample (GSM) numbers in 1st column" , style="danger",dismiss=TRUE, append = FALSE)
    return(NULL)
    }

    # make sure no new row names have been added #
    check = setdiff(rownames(data), isolate(rownames(values.edit$table)))
    if (length(check) > 0) {
       createAlert(session,"ioAlert2",content = "Error: Rows cannot be added. Please remove new rows and re-upload the file" , style="danger",dismiss=TRUE, append = FALSE)
    return(NULL)
    }


    ## summary of changes made, starting with addions and deletions ##
    cols.removed = setdiff(isolate(colnames(values.edit$table)), colnames(data)) 
    cols.added = setdiff(colnames(data), isolate(colnames(values.edit$table)))   
    rows.removed = setdiff(isolate(rownames(values.edit$table)), rownames(data))

    ## summary of modifications ##
    common.rows = intersect(isolate(rownames(values.edit$table)), rownames(data))
    m1A = match(common.rows, isolate(rownames(values.edit$table)))   
    m2A = match(common.rows, rownames(data))   
 
    common.col = intersect(isolate(colnames(values.edit$table)), colnames(data))
    m1B = match(common.col, isolate(colnames(values.edit$table)))   
    m2B = match(common.col, colnames(data))   
 
    t1 = isolate(values.edit$table[m1A,m1B])
    t2 = data[m2A,m2B]

    # needed because some columns are stored as factors 
    t1 = apply(t1, 2, as.character)
    t2 = apply(t2, 2, as.character)

    check = t1 == t2
    
    cols.mod = apply(!check, 2, any)  
    cols.mod = names(which(cols.mod))
 
      
   # generate R code for row removal  ##
   if (length(rows.removed) > 0) {
	comment = "## Analyze only a subset of rows ##"
        isolate(add.code(comment))
        v1 = paste0("keep = ", vector.it(rownames(data)))
	v2 = "m = match(keep, rownames(data.p))"
        v3 = "data.p = data.p[m, , drop = FALSE]" 
  	isolate(add.code(v1))
  	isolate(add.code(v2))
  	isolate(add.code(v3))
  	add.code("m = match(keep, colnames(data.expr))")
  	add.code("data.expr = data.expr[,m]")
   }

   # generate R code for new columns ## 
   if (length(cols.added) > 0) {
       comment = "## Add column to clinical data table ##"
       isolate(add.code(comment))
       for (col in cols.added) {
 	  v = vector.it(data[[col]])
          v = paste0("data.p[[\"", col, "\"]] = ", v)
	  isolate(add.code(v))
       } 
   } 

   # generate R code to modify columns #
   if (length(cols.added) > 0) {
	comment = "## Modify column in clinical data table ##"
       isolate(add.code(comment))
       for (col in cols.mod) {
 	  v = vector.it(data[[col]])
          v = paste0("data.p[[\"", col, "\"]] = ", v)
	  isolate(add.code(v))
       } 
   } 


    format.it <-function(x, label) {
        x = paste0(x, collapse = ", ")
        if (x=="") {
		return(x)
	}
        paste0("<p>", label, x, "</p>")
    } 
 
    cols.added = format.it(cols.added, "Columns added: ")
    cols.mod = format.it(cols.mod, "Columns modified: ")
    cols.removed = format.it(cols.removed, "Columns removed: ") 
    rows.removed = format.it(rows.removed, "Rows removed: ")


    content = "<H4>Current Status</H4><p><strong>Your file has been uploaded, and you may now view the data table. The following changes have been detected: </p>"
    changes = paste0(cols.removed, cols.added, cols.mod, rows.removed)

    if (changes=="") {
	content = paste0(content, "<p> No changes have been detected </p>")
    } else {
	content = paste0(content, changes)
    }

    createAlert(session,"ioAlert2",content = content, style="success",dismiss=TRUE, append = FALSE)
    shinyjs::enable("ClinicalReset") 

    isolate(values.edit$table <- data) 
  }
})


