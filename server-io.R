
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
    createAlert(session,"ioAlert3",content = "<H4>Current Status</H4><p><strong>File has been uploaded! You can now view your data table!</p>",style="success",dismiss=FALSE)
    cat("initial data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")
    data = read.table(infile$datapath, header = TRUE, row.names=1, sep = ",")
    isolate(values.edit$table <- data)
    cat("new data = ", isolate(nrow(values.edit$table)), ", ", isolate(ncol(values.edit$table)), "\n")
  }
  cat("left file upload observe...\n")
})




