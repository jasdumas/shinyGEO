
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


observe({
  add.tab()
  cat("in file upload observe...\n")  
  infile <- input$fileUpload
  if (!is.null(infile)){
    data = try(read.table(infile$datapath, header = TRUE, row.names=1, sep = ","), silent = TRUE)
    if (class(data) %in% "try-error") {
       createAlert(session,"ioAlert3",content = "Error: file could not be uploaded. This most likely includes a file that is not in the correct format (e.g., not a csv file)" , style="danger",dismiss=TRUE, append = FALSE)
	return(NULL)
    }

    # make sure we have row names #
    
    check = rownames(data)%in%colnames(exprInput())

    if (!any(check)) {
       createAlert(session,"ioAlert3",content = "Error: uploaded file must include sample (GSM) numbers in 1st column" , style="danger",dismiss=TRUE, append = FALSE)
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
 
    format.it <-function(x) {
        x = paste0(x, collapse = ", ")
        if (x=="") {
		x = "none"
	}
	return (x)
    } 

     
    cols.added = paste0("<p> Columns added: ", format.it(cols.added), "</p>")
    cols.mod = paste0("<p> Columns modified: ", format.it(cols.mod), "</p>")
    cols.removed = paste0("<p> Columns removed: ", format.it(cols.removed), "</p>")
    rows.removed = paste0("<p> Rows removed: ", format.it(rows.removed), "</p>")
    content = "<H4>Current Status</H4><p><strong>File has been uploaded! You can now view your data table!</p>"
    content = paste0(content, cols.removed, cols.added, cols.mod, rows.removed)

    createAlert(session,"ioAlert3",content = content, style="success",dismiss=TRUE, append = FALSE)
    cat("initial data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")

    isolate(values.edit$table <- data) 

    cat("new data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")
  }
  cat("left file upload observe...\n")
})


