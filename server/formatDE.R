
colors <-function() {
  c(palette(), "darkred", "darkgreen", "darkblue", "orange")
}

# get current color from colors() palette; recycle if necessary
current.color <-function(i) {
  col = colors()
  i = (i-1) %% length(col) + 1  
  col[i]
}


observeEvent(input$formatDEButton2, {
  shinycat("formatDE2 modal\n")
  updateTextInput(session, "km.xlab", value = KM$xlab) 
  updateTextInput(session, "km.ylab", value = KM$ylab)
  updateRadioButtons(session, "hr.format", selected = KM$hr.format)

  df = c("Low Expression", "High Expression")
  aa.color = NULL
  aa.label = NULL
  
  for (i in 1:length(df)) {
    col = KM$col[i]
    s=selectizeInput(paste0("colorKM",i), "",choices = colors(), width = '150px', selected = col) 
    s[[2]]$class = ""  # remove class
    s[[3]][[1]] = NULL # remove label
    s = gsub("<div>", "", s)
    s = gsub("</div>", "", s)
    s = paste(s,"</div>")
    s = gsub("\n", "", s)
    
    t = textInput(paste0("labelDE",i), "", df[i])
    t[[2]]$class = "" # remove class
    t[[3]][[1]] = ""  # remove label
    t = gsub("<input id", "<input size = \"20\" id", t)
    t = gsub("\n", "", t)
    t = gsub("class=\"form-control\"", "", t)
    aa.color = c(aa.color,paste(s, collapse = "") )
    aa.label = c(aa.label, paste(t, collapse = ""))
  }  
  aa.color = gsub("<label[ -=A-Za-z0-9\"]*></label>", "", aa.color)
  
  
  df = cbind(df, aa.color)
  df_rows <- apply(df, 1, row_html) 
  
  header = c("Expression Level", "Color")
  header = row_html(header, TRUE)
  
  df_rows = c(header, df_rows)
  
  p=paste0("<table border = 1>", 
	paste0(df_rows, collapse = ""), "</table>")
  
  p=gsub("class=\"form-control\"", "", p)
  p=gsub("class=\"\"", "", p)  

  output$formatDE2 <- renderUI({
    HTML(p)
  })
})

observeEvent(input$formatDEButton, {
  if (length(input$Group1Values) == 0) {return(NULL)} 
  df = input$Group1Values
  aa.color = NULL
  aa.label = NULL

  n = length(input$Group1Values)
  col = DE$col
  if (is.null(col)) {
	col = current.color(1:n)
  }

  labels = DE$labels
  if (is.null(labels)) {
	labels = input$Group1Values
  }
  
  for (i in 1:length(input$Group1Values)) {
    s=selectizeInput(paste0("colorDE",i), "",choices = colors(), width = '200px', 
	selected = col[i]) 
    s[[2]]$class = ""  # remove class
    s[[3]][[1]] = NULL # remove label
    s = gsub("<div>", "", s)
    s = gsub("</div>", "", s)
    s = paste(s,"</div>")
    s = gsub("\n", "", s)
    
    t = textInput(paste0("labelDE",i), "", labels[i])
    t[[2]]$class = "" # remove class
    t[[3]][[1]] = ""  # remove label
    t = gsub("<input id", "<input size = \"50\" id", t)
    t = gsub("\n", "", t)
    t = gsub("class=\"form-control\"", "", t)
    aa.color = c(aa.color,paste(s, collapse = "") )
    aa.label = c(aa.label, paste(t, collapse = ""))
  }  
  aa.color = gsub("<label[ -=A-Za-z0-9\"]*></label>", "", aa.color)
  
  df = cbind(df, aa.label, aa.color)
  df_rows <- apply(df, 1, row_html) 
  
  header = c("Group", "Label", "Color")
  header = row_html(header, TRUE)
  
  df_rows = c(header, df_rows)
  
  p=paste0("<table border = 1>", paste0(df_rows, collapse = ""), "</table>")
  
  p=gsub("class=\"form-control\"", "", p)
  p=gsub("class=\"\"", "", p)  

  output$formatDE <- renderUI({ 
    HTML(p)
  })

})

### store current colors and labels

## get current colors ##
colorsDE2 <-reactive({
  names = paste0("colorDE", 1:length(isolate(input$Group1Values)))
  vals = NULL
  for (n in names) {
    vals = c(vals, input[[n]])
  }
  vals
})
  
labelsDE2 <-reactive({
  names = paste0("labelDE", 1:length(isolate(input$Group1Values)))
  vals = NULL
  for (n in names) {
    vals = c(vals, input[[n]])
  }
  vals
})


observeEvent(input$Group1Values, {
  # Note: The statement below does not work because colorsDE2() searches colors
  # before the selectInput boxes are created. Therefore, the
  # default colors are returned
  DE$labels = input$Group1Values
})

observeEvent(input$applyFormatDE, {
  DE$col = colorsDE2()
  DE$labels = labelsDE2() 
})

#############
# Survival 
#############
observeEvent(input$applyFormatDE2, { # trigger on Save Changes button within bsModal
  KM$col = c(input$colorKM1, input$colorKM2)
  KM$xlab <- input$km.xlab
  KM$ylab <- input$km.ylab
  KM$hr.format <- input$hr.format
})



