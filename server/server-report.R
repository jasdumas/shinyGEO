## add R code to ace editor 
add.code <-function(line) {  
   if (is.null(reproducible$report)) {
	reproducible$report = line
   } else {
     reproducible$report = paste(isolate(reproducible$report), line, sep = "\n")
  }
}

observeEvent(reproducible$report, {
     shinycat("updateAceEditor for report...\n")
     updateAceEditor(session, "rmd", reproducible$report, 
                  mode = "markdown", theme = "chrome")
})

observeEvent(input$reportBtn, {
      shinycat("generating report...\n")
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

#########################################################
# profiles() or CODE$expression.code will trigger report 
#########################################################
observe({
  shinycat("observe profiles or CODE$expression.code for report...\n")
  if (CODE$expression.code <0) return(NULL)
  if (is.null(profiles())) return(NULL)

  GSE = input$GSE
  if (GSE == "") {
    GSE = strsplit(names(GEO.test),"-")[[1]][1]
  }


if (CODE$expression.code == 0) {
  initialCode <- paste0(
"## Load required packages ##
library(GEOquery)
library(reshape2)
library(survival)
library(ggplot2)
library(GGally)
library(survMisc)

## Download data from GEO ##
GSE = \"", GSE, "\"
GPL = \"", Platforms()[platformIndex()], "\"
 
data.series = getGEO(GEO = GSE, AnnotGPL = FALSE, getGPL = FALSE)
data.platform = getGEO(GPL)
data.index = match(GPL, sapply(data.series, annotation))
data.p = pData(data.series[[data.index]])
") # end of paste of intial code download
  
  add.code(initialCode)
}

# if here, CODE$expression.code is not negative 1, so always add expression

if (CODE$expression.code == 1) {
  add.code("# change default expression normalization")
}
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
  CODE$expression.code <- -1 
 
})


##################################
## Diff. Expression Append to report 
#################################

quote.it <-function(x) paste0("\"", x, "\"")

observeEvent(input$DEadd, {
  shinycat("Add DE code to report...\n")

createAlert(session, "alert2", title = "", style = "success",
            content = "<H4>R Code Generation</H4><p> R Code for Differential Expression Analysis has been added to the <i> Code </i> page </p>", append = TRUE, dismiss = TRUE) 

if (!CODE$stripchart.loaded) {
  s2function = scan(file = "misc/stripchart2.R", what = character(), sep = "\n")
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

col = DE$col
if (is.null(col)) {
   col = current.color(1:length(input$Group1Values))
}
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

group.names = ", vector.it(DE$labels)," 
main = paste(GSE, \"", geneLabel(), "\", sep = \": \")
col = ", vector.it(col), "
print(stripchart2(x,y, groups, group.names = group.names, main = main, col=col))
")
add.code(s3plot)

}) # end of observeEvent for DE

##################################
## Survival Plot Append to report 
#################################
observeEvent(input$Survadd, {
  shinycat("Add survival code to report...\n")

  createAlert(session, "alert2", title = "", style = "success",
            content = "<H4>R Code Generation</H4><p> R Code for Survival Analysis has been added to the <i> Code </i> page </p>", append = TRUE, dismiss = TRUE) 
   if (!CODE$plot.km.loaded) {
     kmfunction = scan(file = "misc/plot.shiny.km.R", what = character(), sep = "\n")
     sapply(kmfunction, add.code)
     CODE$plot.km.loaded = TRUE
  }


xlab = input$km.xlab
ylab = input$km.ylab

if (is.null(xlab)) {
	xlab = "NULL"
} else {
	xlab = paste0("\"",xlab, "\"")
}
if (is.null(ylab)) {
	ylab = "NULL"
} else {
	ylab = paste0("\"",ylab, "\"")
}

hr.inverse = FALSE
if (!is.null(input$hr.format)) {
	if(input$hr.format == "low/high") {
        	hr.inverse = TRUE
        }
}

labels = paste0("\nxlab = ", xlab, "\nylab = ", ylab, "\nhr.inverse = ", hr.inverse) 



add.code("## Survival Analysis ##")
add.probe = paste0("probe = \"", input$selectGenes, "\"") 

add.code(time.analysis()$code)
add.code("")

kmplot <-paste0("probe = \"", input$selectGenes, "\" 
x = data.expr[probe,]

outcome.column = \"", input$autoColumnOutcome, "\"
outcome.orig = data.p[[outcome.column]]
outcome = rep(NA, length(outcome.orig))

eventNo = ", vector.it(input$columnEvent0), "
eventYes = ", vector.it(input$columnEvent1), "
outcome[outcome.orig %in% eventNo] = 0
outcome[outcome.orig %in% eventYes] = 1

optimal.cut = ", KM$cutoff != "Median", "

main = paste(GSE, \"", geneLabel(), "\", sep = \": \")\n", labels, "

plot.shiny.km(time = time, death = as.integer(outcome), x = x, col = ", vector.it(KM$col), ", title = main, xlab = xlab, ylab = ylab, hr.inverse = hr.inverse, optimal.cut = optimal.cut)
")
 
 add.code(kmplot)

}) # end of observeEvent

