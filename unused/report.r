
#####################################
# removed from server-reactives.R
#####################################

add.graph <-function(line) {  ## add graphic info to ace editor for the report
   reproducible$report = paste(isolate(reproducible$report), line, sep = "\n")
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
"# Initial Data Download
library(DT)  ## tested on development version 0.1.32
library(shiny)
library(GEOquery)
library(Biobase)
library(reshape2)
library(survival)
library(affy)
library(limma)
library(shinyBS)
library(GGally)
library(ggplot2)
library(shinyAce)
library(knitr)
data.series = getGEO(GEO = \"", input$GSE, "\", AnnotGPL = FALSE, getGPL = FALSE)
data.platform = getGEO(\"", Platforms()[platformIndex()],  "\")
data.index = match(\"", Platforms()[platformIndex()], "\", sapply(data.series, annotation))
data.p = pData(data.series[[data.index]])
data.expr = exprs(data.series[[data.index]])
") # end of paste of intial code download
  
  add.graph(initialCode)
  
  if (identical(initialCode, initialCode)) { # if the GSE and platform are the same, don't keep adding the same info
    add.graph("")
  }
 cat("END Initial\n") 
})






######################################
# Expression Profiles Append to Report
######################################
observeEvent(input$exprAdd, {
  if (TRACE) cat("In report Append for expression profiles...\n")

  exp <- paste0( 
"# Expression Profiles Plot
ex <- data.expr
if (is.null(ex)) return (NULL)
qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
LogC <- (qx[5] > 100) |
(qx[6]-qx[1] >50 & qx[2] >0) |
(qx[2] > 0 & qx[2] <1 & qx[4] > 1 & qx[4] < 2) 
if (LogC & as.logical(\"", input$radio == 1, "\")) {
ex[which(ex <= 0)] <- NaN
return(ex <- log2(ex)) }
if (as.logical(\"",input$radio == 2 , "\")) {
return (ex <- log2(data.expr))
} else { return (ex <- data.expr)
}
x = ex
if (is.null(x)) return(NULL)
n = ncol(x)
if (n > 30) {
s = sample(1:n, 30)
x = x[,s]
}
if (n > 30) {
title.detail = ' selected samples'
} else {
title.detail = ' samples'
}
if (as.logical(\"",input$radio == 1, "\") | as.logical(\"",input$radio == 2, "\")) {
y.label = 'log2 Expression'       
} else {
y.label = 'Expression'
}
#par(mar=c(2+round(max(nchar(sampleNames(\"",input$GSE, "\")))/2),4,2,1))
title <- paste(isolate(\"", input$GSE, "\"), '/', isolate(\"",input$platform, "\") , title.detail, sep ='') 

fixed.df <- as.data.frame(x=x, stringsAsFactors = FALSE)
  
  x1 <- reshape2::melt(fixed.df, na.rm = TRUE, 
            variable.name = 'variable', 
            value.name = 'value')
  
  exp.prof.plot <- ggplot(x1, aes(variable, value)) + 
                geom_boxplot(outlier.colour = 'green') +
                labs(title = title, y = y.label, x = '') + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  print(exp.prof.plot)

") # end of paste
  add.graph(exp)
  
  if (identical(exp, exp)) { # if the GSE and platform are the same, don't keep adding the same info
    add.graph("")
  }
}) # end of observeEvent for expression profiles plot 




##################################
## Diff. Expression Append to report 
#################################

quote.it <-function(x) paste0("\"", x, "\"")

observeEvent(input$DEadd, {
  if (TRACE) cat("In report append DE...\n")

#########################################################  
#   cat("outputting test script...\n")
#   
#   #GSE = "GSE13"
#   #GPL = "GPL75"
#   #LOG = TRUE
#   #PROBE = "aa000380_s_at"
#   #DE.column = "source_name_ch1"
#   #DE.groups = c("Immature B cells", "Mature B cells")
#   
#   script.GSE = paste0("GSE = ",quote.it(input$GSE), "\n")
#   script.GPL =   paste0("GPL = ", quote.it(isolate(Platforms()[platformIndex()])), "\n")
# #  script.probe = paste0("PROBE = ", quote.it(input$selectProbes), "\n")
#   script.log2 = paste0("LOG2 = ", values.edit$log2, "\n")
#   script.DE.column = paste0("DE.column = ", quote.it(input$selectedColumn), "\n")
#   
#   grps = paste0(sapply(input$Group1Values, quote.it), collapse = ",")
#   grps = paste0("c(", grps, ")")  
#   script.DE.groups = paste0("DE.groups = ", grps)
#     
#   print(script.GSE)
#   print(script.GPL)
# #  print(script.probe)
#   print(script.log2)
#   print(script.DE.column)
#   print(script.DE.groups)
#   
#   test.file = paste0("test/", input$GSE, "_test.R")
#   
#   cmd = paste0("echo '", script.GSE, script.GPL, script.log2, 
#                script.DE.column, script.DE.groups, "' > ", test.file)
#   
#   #cmd = "echo 'hello, how are you\n' > tmp.R"
#   system(cmd)
#   cmd = paste0("cat GEO-script-template.R >> ", test.file)
#   system(cmd) 
# 
#   cat("\n==========RUNNING TEST ON", test.file, "========\n")
#   rmarkdown::render(test.file)
# 
######################################
s2function <- paste0(
"# Differential Expression Plot
stripchart2 <- function(x,y, group.names = NULL, jitter = 0.3, line.off = 0.3, 
lwd = 5, col = NULL, main = '', mark = 'mean') {
s = split(x,y)
if (is.null(group.names)) group.names = names(s)
if (is.null(col)) col = 1:length(s)
add = NULL
if (length(s) == 2) {
m = lapply(s,mean, na.rm=TRUE)
fc = round(2**(m[[2]] - m[[1]]), 2)
t = t.test(s[[1]], s[[2]])
p = round(t$p.value, 3)   
if (p < 0.001) {
p = '(P < 0.001)'
} else {
p = paste0('(P = ', p, ')')
}
add = paste('\nFC = ', fc, p, collapse = '')
} else if (length(s) > 2) {
l = lm(x~y); l = summary(l)
p = 1-pf(l$fstatistic[1], l$fstatistic[2], l$fstatistic[3])
p = round(p, 3)
if (p < 0.001) {
add = '\nP < 0.001'
} else {
add = paste0('\nP = ', p)
}
} 
if (is.null(main)) {
main = '' 
} else {
main = paste(main, add)
}
me = melt(s, na.rm=TRUE)
stripchart3 <- ggplot(me, aes(x = as.factor(L1), y = value, color=L1)) 
return(stripchart3 + 
labs(title = main, y = 'log2 expression', x='') +
theme(legend.position='none') +
scale_x_discrete(labels=group.names) +
geom_point(position = 'jitter', aes(colour = L1)) + 
scale_colour_manual(values = col) +
geom_errorbar(stat = 'hline', yintercept = 'mean', width=0.8,aes(ymax=..y..,ymin=..y..)))
if (mark %in% c('mean', 'median')) {
if (mark == 'mean') mm = lapply(s,mean, na.rm=TRUE)
if (mark == 'median') mm = lapply(s, median, na.rm=TRUE)
for (i in 1:length(mm)) {
lines(c(i-line.off, i+line.off), c(mm[[i]], mm[[i]]), lwd = lwd)
    }
  }
}

")

add.graph(s2function)

if (identical(s2function, s2function)) { 
  add.graph("")
}
  
  s3plot <- paste0(
"match.y = match(as.character(\"",input$selectProbes, "\"),rownames(data.expr))
x = data.expr[match.y,] 
iv = as.character(\"", input$selectedColumn, "\")
m = match(as.character(iv), colnames(data.p)) 
clinical = as.character(data.p[,m])  
selected = as.character(\"", input$Group1Values,"\")
k = clinical %in% selected
y = clinical
y[!k] = NA
y = factor(y, levels = \"", input$Group1Values,"\" )
main = paste(\"",input$GSE ,"\", \"", input$selectGenes ,"\", sep = '\')
print(stripchart2(x,y, group.names = labelsDE(), main = main, col=colorsDE()))

")
add.graph(s3plot)

if (identical(s3plot, s3plot)) { 
  add.graph("")
}

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


