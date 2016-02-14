add.code <-function(line) {  ## add code to ace editor for the report 
   if (is.null(reproducible$report)) {
	reproducible$report = line
   } else {
     reproducible$report = paste(isolate(reproducible$report), line, sep = "\n")
  }
}

observeEvent(reproducible$report, {
     cat("updateAceEditor for report\n")
     updateAceEditor(session, "rmd", reproducible$report, 
                  mode = "markdown", theme = "chrome")
})


observeEvent(input$reportBtn, {
      cat("generating report\n")
      test.file = "reports/test.R"
      createAlert(session, "reportAlert", alertId = "report-alert", title = "Generating Report...", 
	style = "info", content = "Your report is being generated (this may take 1-2 minutes)", 
	dismiss = FALSE) 
      write(reproducible$report, file = test.file)
      rmarkdown::render(test.file)
      createAlert(session, "reportAlert", alertId = "report-alert2", title = "Generating Report...", 
	style = "success", content = "Your report has been generated", append = FALSE, 
	dismiss = FALSE)
      toggleModal(session, "reportModal", "close")
      system("open reports/test.html")
 
})

######################################
# Initial Code Append to Report
######################################
observeEvent(profiles(),  {
  if (is.null(profiles())) return(NULL)
  cat("observe profiles\n")
  if (TRACE) cat("In Initial...\n")
  GSE = input$GSE
  if (GSE == "") {
    GSE = strsplit(names(GEO.test),"-")[[1]][1]
  }
  initialCode <- paste0(
"## Load required packages ##
library(GEOquery)
library(reshape2)
library(survival)
library(ggplot2)
library(GGally)

## Download data from GEO ##
GSE = \"", GSE, "\"
GPL = \"", Platforms()[platformIndex()], "\"
 
data.series = getGEO(GEO = GSE, AnnotGPL = FALSE, getGPL = FALSE)
data.platform = getGEO(GPL)
data.index = match(GPL, sapply(data.series, annotation))
data.p = pData(data.series[[data.index]])
data.expr = exprs(data.series[[data.index]])
") # end of paste of intial code download
  
  add.code(initialCode)

  if (values.edit$log2) {
	add1  = "data.expr[which(data.expr <= 0)] <- NaN"
	add2 = "data.expr = log2(data.expr)"
	add.code(add1)
	add.code(add2)
  }
  
  exp = paste0("
## generate boxplot of expression profiles ##
title = \"samples\" 
s.num = 1:ncol(data.expr)
n = ncol(data.expr)
if (n > 30) {
  s.num = sample(1:n, 30)
  title = \"selected samples\"
}
title = paste0(GSE, \"/\", GPL, \" \", title)

fixed.df <- as.data.frame(x=data.expr[,s.num], stringsAsFactors = FALSE)

x1 <- reshape2::melt(fixed.df, na.rm = TRUE, id.vars = NULL,
         variable.name = \"variable\", value.name = \"value\")

exp.prof.plot <- ggplot(x1, aes(variable, value)) +
                geom_boxplot(outlier.colour = \"green\") +
                labs(title = title, y = \"log2 expression\", x = \"\") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(exp.prof.plot)
")

  add.code(exp)
  
 cat("END Initial\n") 
})


######################################
# Expression Profiles Append to Report
######################################
observeEvent(input$exprAdd, {
  if (TRACE) cat("In report Append for expression profiles...\n")

  
}) # end of observeEvent for expression profiles plot 

##################################
## Diff. Expression Append to report 
#################################

quote.it <-function(x) paste0("\"", x, "\"")

observeEvent(input$DEadd, {
  if (TRACE) cat("In report append DE...\n")


if (!CODE$stripchart.loaded) {
  s2function = scan(file = "stripchart2.R", what = character(), sep = "\n")
  sapply(s2function, add.code)
  CODE$stripchart.loaded = TRUE
  add.code("")
} 

vector.it <-function(x) {
  x = paste0("\"", x, "\"", collapse = ",")
  paste0("c(", x, ")")
}


 
## generate differential expression plot ##
add.de.header = "## Differential Expression Analysis ##"
add.probe = paste0("probe = \"", input$selectGenes, "\"") 
add.column = paste0("column = \"", input$selectedColumn, "\"")
add.groups = paste0("groups = ", vector.it(input$Group1Values)) 

add.code(add.de.header)
add.code(add.probe)
add.code(add.column)
add.code(add.groups)

 s3plot <- paste0("
x = data.expr[probe,]
y = as.character(data.p[,column]) 

k = y%in% groups
y[!k] = NA
y = factor(y)

group.names = ", vector.it(labelsDE())," 
main = paste(GSE, \"", geneLabel(), "\", sep = \": \")
col = ", vector.it(colorsDE()), "
print(stripchart2(x,y, groups, group.names = group.names, main = main, col=col))
")
add.code(s3plot)

}) # end of observeEvent for DE

##################################
## Survival Plot Append to report 
#################################
observeEvent(input$Survadd, {

   if (!CODE$plot.km.loaded) {
     kmfunction = scan(file = "plot.shiny.km.R", what = character(), sep = "\n")
     sapply(kmfunction, add.code)
     add.code(time.analysis()$code)
     add.code("")
     CODE$plot.km.loaded = TRUE
  }

add.code("## Survival Analysis ##")
add.probe = paste0("probe = \"", input$selectGenes, "\"") 
kmplot <-paste0("probe = \"", input$selectGenes, "\" 
x = data.expr[probe,]

outcome.column = \"", input$autoColumn.outcome, "\"
outcome.orig = data.p[[outcome.column]]
outcome = rep(NA, length(outcome.orig))

eventNo = ", vector.it(input$columnEvent0), "
eventYes = ", vector.it(input$columnEvent1), "
outcome[outcome.orig %in% eventNo] = 0
outcome[outcome.orig %in% eventYes] = 1
 
main = paste(GSE, \"", geneLabel(), "\", sep = \": \")
plot.shiny.km(time = time, death = as.integer(outcome), x = x, col = ", vector.it(colorsDE3()), ", title = main)
")
 
 add.code(kmplot)
  if (TRACE) cat("In report Append Observe for Survival...\n")

}) # end of observeEvent

