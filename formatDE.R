current.color <-function(i) {
  i = (i-1)%%length(palette(c("red", "blue")))+1
  palette()[i]
}

output$formatDE <- renderUI({ 
  HTML(formatTableDE())
  
})
# add for survival KM-format
output$formatDE2 <- renderUI({ 
  HTML(formatTableDE2())
  
})

formatTableDE <-reactive({  
  if (length(input$Group1Values) == 0) {return(NULL)} 
  df = input$Group1Values
  aa.color = NULL
  aa.label = NULL
  
  for (i in 1:length(input$Group1Values)) {

    col = current.color(i)
    s=selectizeInput(paste0("colorDE",i), "",choices = colors(), width = '150px', selected = col) 
    s[[2]]$class = ""  # remove class
    s[[3]][[1]] = NULL # remove label
    s = gsub("<div>", "", s)
    s = gsub("</div>", "", s)
    s = paste(s,"</div>")
    s = gsub("\n", "", s)
    
    t = textInput(paste0("labelDE",i), "", input$Group1Values[i])
    t[[2]]$class = "" # remove class
    t[[3]][[1]] = ""  # remove label
    t = gsub("<input id", "<input size = \"20\" id", t)
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
  #print(p)
  p
})

### store current colors and labels
reactiveFormat = reactiveValues(colorsDE = NULL, labels = NULL)
colorsDE <-reactive({reactiveFormat$colorsDE})
labelsDE <-reactive({reactiveFormat$labels})

## get current colors ##
colorsDE2 <-reactive({
  names = paste0("colorDE", 1:length(isolate(input$Group1Values)))
#  cat("names = ", names, "\n")
  vals = NULL
  for (n in names) {
 #   cat("color = ", input[[n]], "\n")
    vals = c(vals, input[[n]])
  }
#  cat("colors = ", vals, "\n")
  vals
})
  
labelsDE2 <-reactive({
  names = paste0("labelDE", 1:length(isolate(input$Group1Values)))
  #cat("names = ", names, "\n")
  vals = NULL
  for (n in names) {
    vals = c(vals, input[[n]])
  }
 # cat("labels = ", vals, "\n")
  vals
})


observeEvent(input$Group1Values, {
  # Note: The statement below does not work because colorsDE2() searches colors
  # before the selectInput boxes are created. Therefore, the
  # default colors are returned
  #reactiveColors$DE = colorsDE2()  
  reactiveFormat$colorsDE = current.color(1:length(input$Group1Values))
  reactiveFormat$labels = input$Group1Values
})

observeEvent(input$applyFormatDE, {
  reactiveFormat$colorsDE = colorsDE2()
  reactiveFormat$labels = labelsDE2()
})

#############
# Survival 
#############

formatTableDE2 <-reactive({
  time.surv = as.double(parse.modal()[,1])
  event.surv = as.integer(parse.modal()[,2])
  
  if (length(time.surv) == 0 & length(event.surv) == 0) {return(NULL)} # the length is always 2 (Time & Outcome)
  df = c("High Expression", "Low Expression")
  aa.color = NULL
  aa.label = NULL
  
  for (i in 1:length(df)) {
    
    col = current.color(i)
    s=selectizeInput(paste0("colorDE",i), "",choices = colors(), width = '150px', selected = col) 
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
  
  p=paste0("<table border = 1>", paste0(df_rows, collapse = ""), "</table>")
  
  p=gsub("class=\"form-control\"", "", p)
  p=gsub("class=\"\"", "", p)  
  #print(p)
  p
})


### store current colors and labels
reactiveFormat3 = reactiveValues(colorsDE3 = NULL, labels = NULL)
colorsDE3 <-reactive({reactiveFormat3$colorsDE3})
labelsDE3 <-reactive({reactiveFormat3$labels})

## get current colors ##
colorsDE4 <-reactive({
  names = paste0("colorDE", 1:length(isolate(c("High Expression", "Low Expression"))))
  #  cat("names = ", names, "\n")
  vals = NULL
  for (n in names) {
    #   cat("color = ", input[[n]], "\n")
    vals = c(vals, input[[n]])
  }
  #  cat("colors = ", vals, "\n")
  vals
})

labelsDE3 <-reactive({
  names = paste0("labelDE", 1:length(isolate(c("High Expression", "Low Expression"))))
  #cat("names = ", names, "\n")
  vals = NULL
  for (n in names) {
    vals = c(vals, input[[n]])
  }
  # cat("labels = ", vals, "\n")
  vals
})


observeEvent(input$submitButton, { # trigger is the GSE submit button so the graph appears initially
  # Note: The statement below does not work because colorsDE2() searches colors
  # before the selectInput boxes are created. Therefore, the
  # default colors are returned
  #reactiveColors$DE = colorsDE2()  
  reactiveFormat3$colorsDE3 = current.color(1:2)
  reactiveFormat3$labels = c("High Expression", "Low Expression")
})

observeEvent(input$applyFormatDE2, { # trigger on Save Changes button within bsModal
  reactiveFormat3$colorsDE3 = colorsDE4()
  reactiveFormat3$labels = labelsDE3()
})



