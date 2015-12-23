add.graph <-function(line) {  ## add graphic info to ace editor for the report
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


######################################
# Initial Code Append to Report
######################################
observeEvent(profiles(),  {
  cat("observe profiles\n")
  if (TRACE) cat("In Initial...\n")
  initialCode <- paste0(
"## Load required packages ##
library(GEOquery)
library(reshape2)
library(survival)
library(ggplot2)

## Download data from GEO ##
GSE = \"", input$GSE, "\"
GPL = \"", Platforms()[platformIndex()], "\"
 
data.series = getGEO(GEO = GSE, AnnotGPL = FALSE, getGPL = FALSE)
data.platform = getGEO(GPL)
data.index = match(GPL, sapply(data.series, annotation))
data.p = pData(data.series[[data.index]])
data.expr = exprs(data.series[[data.index]])
") # end of paste of intial code download
  
  add.graph(initialCode)

  if (values.edit$log2) {
	add1  = "data.expr[which(data.expr <= 0)] <- NaN"
	add2 = "data.expr = log2(data.expr)"
	add.graph(add1)
	add.graph(add2)
  }
  
  exp = paste0("
## generate boxplot of expression profiles ##
title = \"samples\"; 
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

  add.graph(exp)
  
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

s2function = scan(file = "stripchart2.R", what = character(), sep = "\n")
sapply(s2function, add.graph)
 
vector.it <-function(x) {
  x = paste0("\"", x, "\"", collapse = ",")
  paste0("c(", x, ")")
}


 
## generate differential expression plot ##
add.probe = paste0("probe = \"", input$selectGenes, "\"") 
add.column = paste0("column = \"", input$selectedColumn, "\"")
add.groups = paste0("groups = ", vector.it(input$Group1Values)) 

add.graph(add.probe)
add.graph(add.column)
add.graph(add.groups)

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
add.graph(s3plot)

}) # end of observeEvent for DE

##################################
## Survival Plot Append to report 
#################################
observeEvent(input$Survadd, {
  if (TRACE) cat("In report Append Observe for Survival...\n")
  
survfunction <- paste0(
"# Survival Analysis Plot
plot.shiny.km <- function(time, death, x, title = '',  
no.plot = FALSE, 
subset = rep(TRUE, length(time)), 
col = NULL,  ...) {
## filter out missing data ##
subset = subset & !is.na(time) & !is.na(death) & !is.na(x) & !is.nan(x)
x = x[subset]; time = time[subset]; death = death[subset]
if (length(x) ==0 | length(time) == 0 | length(death) == 0) {
cat('no samples, returning...\n')
return(data.frame(hr = NA, p.km = NA))
}
## make sure x has at least 2 levels if categorical ##  
if (is.character(x)) x = as.factor(x)
if (is.factor(x)) {
n = length(levels(x))
if (n <2) {
cat('x must have at least 2 levels, returning...\n')
return(data.frame(hr = NA, p.km = NA))
}
} 
## use 'km' below for continuous x when x is numeric
#  km = survfit(Surv(time, death) ~x)  
## use median to split into high and low groups ## 
if (is.numeric(x)) {
newX = x
cut = median(x,na.rm=TRUE)
newX[x > cut] = 'high'
newX[x <= cut] = 'low'
x = as.factor(newX)
}
n = length(levels(x))
km.group = coxph(Surv(time, death) ~ x)
p.km = 1 - pchisq(km.group$score,n-1)
hr = exp(km.group$coefficients)
n = km.group$n
if(no.plot) {
return(data.frame(hr = hr, p.km = p.km))
}
## plot graph ##
km.group1 = survfit(Surv(time, death) ~ x)
km.group = ggsurv(km.group1, 
                    main = title, xlab = 'Time', ylab = 'Survival',
                    surv.col = col, cens.col = 'black') 

if (is.null(col)) col = col
plot(km.group, ...)

if (!is.null(title)) {
hr.str = '' 
p.str = paste('P = ', round(p.km,4), sep = '')
if (length(hr) == 1) {
hr.str = paste('HR = ', round(hr,2), ', ')
}
if (title!='') title = paste(title, '\n', sep = '')
title = paste(title, hr.str, p.str)
title(title)
}
}

")
add.graph(survfunction) 
survComment <- paste0(
"match.h = match(as.character(\"",input$selectProbes, "\"),rownames(data.expr))
x = data.expr[match.h,]
print(plot.shiny.km(time = as.double(\"",parse.modal(), "\" [,1]), 
              death = as.integer(\"",parse.modal(), "\" [,2]), 
              x = x(), 
              col = as.character(\"", colorsDE3(), "\") ))
")
add.graph(survComment)

if (identical(survComment, survComment)) { 
  add.graph("")
}
}) # end of observeEvent

##############################
## Find and Replace to report 
##############################
observeEvent(input$Enter, {
  if (TRACE) cat("In report Append Observe for Find & Replace...\n")
  
  find.repace <- paste0("# Find and Replace of Clinical Data
                        find.str = as.character(\"",input$find,"\")
                        column.num = as.character(\"",input$drop2,"\")
                        replace.str = as.character(\"",input$replace, "\")
                        
                        if (find.str == '' & replace.str == '') {   
                        return(values.edit$table)
                        }
                        exactMatch = isolate(input$checkbox) # exact match condition
                        
                        if (exactMatch) {    
                        find.str = paste(\',^,'\', find.str, \',$,'\', sep = '')
                        }
                        
                        newClinical <- as.data.frame(\"",values.edit$table, "\")
                        
                        if (is.factor(newClinical[,column.num])) {
                        newClinical[,column.num] = as.character(newClinical[,column.num])
                        }
                        
                        partialReplace = as.logical(\"",input$survCheckbox, "\")
                        
                        if (partialReplace) {                 
                        newClinical[[column.num]] = gsub(find.str, replace.str, newClinical[[column.num]])  ## fixed = T for symbols
                        g.total = grep(find.str, newClinical[,column.num])  
                        newClinical[g.total,column.num] = replace.str 
                        } else {
                        g.total = grep(find.str, newClinical[,column.num])  
                        newClinical[g.total,column.num] = replace.str 
                        }
                        
                        as.data.frame(\"",values.edit$table, "\") = newClinical
                        return (as.data.frame(\"",values.edit$table, "\"))
                        
                        ")
  
  add.graph(find.replace)
  
})


if (0) {
#####################################
# removed from ui.tab.reproducible.R
#####################################
tab.report = tabItem("Report",
        div(style = "display:inline-block; width:30%", 
            downloadButton('downloadData', 'Download Report')
        ),
             h3("Report"), 
             htmlOutput("knitDoc")
    )
}

#####################################
# removed from server-output.R
#####################################

###################
# Knitr Report
###################
output$knitDoc <- renderPlot(
  #input$exprAdd
  #input$DEadd
  #input$Survadd
  #return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))))
  #cat("knitDoc\n")
  print(input$exProfiles)
  )  
   
# a reactive to supply the content function with the text from the aceEditor
knit.report <- reactive({
  knit2html(text = input$rmd, quiet = TRUE)
})
### Download knitr report ###
output$downloadData <- downloadHandler(
  filename = function() { 
    paste("report", "html", sep=".") 
  },
  
  content = function(file) {
    #input$knitDoc ## trial to connect knitr with download button
    
    src <- normalizePath('report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd')
  
    library(rmarkdown)
    #out <- render('report.Rmd', output_format = html_document())
    #a <- knit(input$rmd)
    out <- render(input$rmd, output_format = html_document())
    
    file.rename(out, file)
  
  }
)


