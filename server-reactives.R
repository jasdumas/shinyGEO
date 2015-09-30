#############################################################################
## Reactives
#############################################################################

createAlert(session, "alert1", alertId = "GSE-alert", 
            title = "Enter a GSE accession number and click the Submit button to begin", style = "danger",
            content = NULL, append = TRUE, dismiss = FALSE) 

###################################################
# Edit table reactiveValues()
###################################################
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL, original = NULL, log2 = FALSE)
reproducible <-reactiveValues(code = NULL, report = NULL)

GLOBAL <-reactiveValues(needGSE = TRUE)


### functions to append/aggregate a new line to the aceEditor
add.line <-function(line) {
    reproducible$code = paste(isolate(reproducible$code), line, sep = "\n")
}

add.graph <-function(line) {  ## add graphic info to ace editor for the report
   reproducible$report = paste(isolate(reproducible$report), line, sep = "\n")
}


observeEvent(input$submitButton, { 
  closeAlert(session, "geneSymbolAlert")
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
})

observeEvent(input$platform, {
  closeAlert(session, "geneSymbolAlert")
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
})


observeEvent(reproducible$code, {
     updateAceEditor(session, "myEditor", reproducible$code,
                         mode="r", theme="chrome")
 })

observeEvent(reproducible$report, {
     updateAceEditor(session, "rmd", reproducible$report, 
                  mode = "markdown", theme = "chrome")
})

observeEvent(input$tabs,{
  cat("selected tab = ", input$tabs, "\n")  
  if (is.null(dataInput())) {
      updateTabsetPanel(session, "tabs", selected = "Expression Profiles")
      closeAlert(session, "Gene-alert")
  }
  
})



####################################
### dataInput: the GEO object ######
####################################
dataInput <- reactive({
  input$submitButton
  # Runs the intial input once the button is pressed from within the 
  # reactive statement
  
  if (is.null(isolate(input$GSE))) return(NULL)
  if (TRACE) cat("In dataInput reactive...\n")  
  GSE = isolate(gsub(" ", "", (input$GSE)))   # remove white space
  if (GSE=="") return(NULL)
  closeAlert(session, "GSE-alert")
  closeAlert(session, "GPL-alert")
  cat("creating alert...\n")
   createAlert(session, "alert1", alertId = "GSE-alert", title = "Current Status", style = "info",
              content = "Downloading Series (GSE) data from GEO", append = TRUE, dismiss = FALSE) 
  code = paste0("data.series = getGEO(GEO = \"", GSE, "\", AnnotGPL = FALSE, getGPL = FALSE)")
  add.line(code)
  getGEO(GEO = isolate(GSE), AnnotGPL=FALSE, getGPL = FALSE)  
})

################################################################
### Platforms: returns the platform only if the GSE # is entered 
################################################################  
Platforms <- reactive({
  if (TRACE) cat("In Platforms reactive...\n")
  if (is.null(dataInput())) {
    return(NULL)
  }
  closeAlert(session, "GSE-alert")  
  as.character(sapply(dataInput(), annotation))    
})

#########################################
### Platforms: the chosen platform Index 
#########################################  
platformIndex <- reactive({
  if (TRACE) cat("In platformIndex reactive...\n")
  if (is.null(dataInput()) | length(input$platform) ==0) {
    return(NULL)
  }
#  if (length(dataInput())==1) return (1)
  cat("matching platform = ", input$platform, "\n")
  m = match(input$platform, as.character(sapply(dataInput(), annotation)))    
  if (is.na(m)) return(NULL)
  return(m)
})

################################################
### return selected platform info as a table
################################################  
platInfo <- reactive({
  if (TRACE) cat("In platInfo reactive...\n")
  if (is.null(Platforms()) | is.null(platformIndex())) return (NULL)

  code = paste0("data.platform = getGEO(\"", Platforms()[platformIndex()],  "\")
")
  add.line(code)
  closeAlert(session, "GPL-alert")
  createAlert(session, "alert1", alertId = "GPL-alert", title = "Current Status", style = "info",
              content = "Downloading platform (GPL) data from GEO", append = TRUE, dismiss = FALSE) 
  
  a = isolate(Platforms())
  b = isolate(platformIndex())
  cat("Platforms() = ", a, "\n")
  cat("platformIndex = ", b, "\n")
  cat("downloading platform now...\n")
  
  t = Table(getGEO(Platforms()[platformIndex()]))
  
  k = t[,"ID"]
  common.probes = intersect(row.names(exprInput()), as.character(k))
  n = match(common.probes, k)
  r = t[n,]
  return(r)
})

## update gene and probe names when platform index changes
## we do this so the user is not waiting for genes/probes
## to be displayed when moving to Differential Expression
## Analysis panel
observe({
  platformIndex()
  geneNames()
  probeNames()
})

##############################################
### unique gene names for selected platform
##############################################
geneNames <- reactive ({
  if (TRACE) cat("In geneNames reactive...\n")
  if (is.null(platInfo())) return (NULL)
 
  gene.column = values.edit$platformGeneColumn
  if (is.null(gene.column)) {
    check.names = c("Gene Symbol", "GeneSymbol", "Symbol",
                 "GENE_SYMBOL")
    m = check.names%in%colnames(platInfo()) 
    w = which(m)
    if (length(w) == 0) {
      createAlert(session, "alert2", alertId = "geneSymbolAlert", title = "Could not find gene symbol", style = "danger",
                  content = "A gene symbol for this platform could not be found. Please select another platform or analyze another dataset.", 
                  append = FALSE, dismiss = FALSE)
      values.edit$platformGeneColumn = NULL
      return(NULL)
    }
    gene.column = check.names[w[1]]
    values.edit$platformGeneColumn = gene.column
  }
 
  #cat("finding column: ", gene.column, "\n")
  m = match(gene.column,colnames(platInfo()))
  t = unique(platInfo()[,m])  
  return(sort(as.character(t)))
})

########################################
### selected Gene index
########################################  
selectedGene <- reactive ({
  if (TRACE) cat("In selectGene reactive...\n")
  gene.column = values.edit$platformGeneColumn
  if (is.null(input$selectGenes) | is.null(gene.column)) return (NULL)
  
  # without this line, this will find all probes that do not
  # have a matching gene, i.e., the selected gene is ""
  if (input$selectGenes == "") return(NULL)
  #cat("using gene column = ", gene.column, "\n")
  m = match(gene.column,colnames(platInfo()))
  g = grep(paste("^",input$selectGenes,"$",sep=""), as.character(platInfo()[,m]))
  return(g)
})

#############################################
### probe names for current expression data
#############################################  
probeNames <- reactive({
  if (TRACE) cat("In probeNames reactive...\n")
  if (is.null(dataInput())) return(NULL)
  else if (is.null(selectedGene())) return(NULL)
  return (as.character(platInfo()[selectedGene(),match("ID",colnames(platInfo()))]))
})

#######################################################
# clinicalInput: Clinical Data
#######################################################
clinicalInput <- reactive({
  if (TRACE) cat("In clinicalInput reactive...\n")
  if (is.null(dataInput()) | is.null(platformIndex())) {
    return(NULL)
  }
  ### Checks if initial values.edit$table is NULL (which it is set to initially)
  if (!is.null(values.edit$table)) {
    p = values.edit$table
  } else {
    code = paste0("data.p = pData(data.series[[data.index]])")
    add.line(code)
    p = as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
  } 
  #####################################################################
  #  display only columns that have more than one possible value; this
  #   removes many columns such as contact info. In addition all 
  #   columns specified by RM.COLS will be removed
  #####################################################################
  
  RM.COLS = c("status", "last_update_date", "submission_date")
  num.levels = apply(p, 2, function(x) nlevels(as.factor(x)))
  
  p = p[,num.levels > 1]
  
  m = match(RM.COLS, colnames(p))
  
  m=m[!is.na(m)]
  
  if (length(m) > 0) p=p[,-m, drop = FALSE]
  
  m = match(colnames(exprInput()), rownames(p))
  
  p = p[m,]
  
  values.edit$table = p
  
  return(p)

})
# to-do commit for a revert button for the survival analysis table
###########################
### Original Clinical Table 
###########################
# originalTable <- reactive({
#   if (TRACE) cat("In clinicalInput reactive...\n")
#   if (is.null(dataInput()) | is.null(platformIndex())) {
#     return(NULL)
#   }
#   ### Checks if initial values.edit$table is NULL (which it is set to initially)
#   if (!is.null(values.edit$original)) {
#     p = values.edit$original
#   } else {
#     p = as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
#   } 
#   #####################################################################
#   #  display only columns that have more than one possible value; this
#   #   removes many columns such as contact info. In addition all 
#   #   columns specified by RM.COLS will be removed
#   #####################################################################
#   
#   RM.COLS = c("status", "last_update_date", "submission_date")
#   num.levels = apply(p, 2, function(x) nlevels(as.factor(x)))
#   p = p[,num.levels > 1]
#   m = match(RM.COLS, colnames(p))
#   m=m[!is.na(m)]
#   if (length(m) > 0) p=p[,-m, drop = FALSE]
#   m = match(colnames(exprInput()), rownames(p))
#   p = p[m,]
#   values.edit$original = p
#   return(p)
#   
# })


######################################################
# exprInput - expression data for selected platform
######################################################
exprInput <- reactive({
  if (TRACE) cat("In exprInput reactive...\n")
  pi = platformIndex()
  cat("pi = ", pi, "\n")
  if (is.null(dataInput()) | is.null(pi)) return(NULL)
  pl=Platforms()[platformIndex()]
  code = paste0("data.index = match(\"", pl, "\", sapply(data.series, annotation))")
  add.line(code)
  code = paste0("data.expr = exprs(data.series[[data.index]])")
  add.line(code)
  ans = exprs(dataInput()[[pi]])
  return(ans)
})

########################################
### selected probe as row number
########################################  
selectedProbe <- reactive ({
  if (TRACE) cat("In selectedProbe reactive...\n")
  return(match(input$selectProbes,rownames(exprInput())))
})

########################################
### ColumnNames of clinicial data table
########################################  
ColumnNames <- reactive({
  if (TRACE) cat("In ColumnNames reactive...\n")
  if (is.null(clinicalInput()) | is.null(exprInput())) return(NULL)
  vars = colnames(clinicalInput())
  vars <- as.list(vars)
  return(vars)
})

########################################
### Summary of Clinical Data table
########################################  
clinicalDataSummary <- reactive({
  if (TRACE) cat("In clinicalDataSummary reactive...\n")
  t = clinicalInput()
  if (is.null(t)) return(NULL)
  vars = colnames(t)
  a = apply(t, 2, function(x)levels(as.factor(x)))
  
  ## format function to truncate row contents with a place holder " ..."
  format.it <-function(x, max) {
    x = x[x!=""]
    if (length(x) <= max) return(x)
    x[max] = " ..."
    return(x[1:max])
  }
  a = lapply(a, format.it, 4)
  
  a = sapply(a, paste, collapse = ", ")
  cbind(variable = vars, values = a)
})


###################################################
# get possible values of the selected column names
###################################################
groupsForSelectedColumn <- reactive({
  if (TRACE) cat("In groupsForSelectedColumn reactive...\n")
  vars = values.edit$table
  #if (is.null(vars)) return(NULL)      
  if (is.null(vars) | is.null(input$selectedColumn)) return(NULL)      
  
  vars <- vars[, as.character(input$selectedColumn)] 
  vars = factor(vars)
  return(as.list(levels(vars)))
  print(vars)
})  

###################################################
# get default levels of the selected column, either
# all levels or NULL if number of levels exceeds
# a cut-off 
###################################################
defaultGroupsForSelectedColumn <- reactive({
  if (TRACE) cat("In defaultGroupsForSelectedColumn reactive...\n")
  g = groupsForSelectedColumn()
  if (length(g) > 8) return(NULL) 
  g    
})


#######################################################
## Find & Replace Method for editing tables 
######################################################
# drop-down of column names
 output$dropModal <- renderUI({
      selectInput("drop2", "Column Names", choices = ColumnNames(), selected = "")
    })

## reactives for textboxes/drop-downs in modal window
find.str <- reactive({input$find})       
replace.str <- reactive({input$replace})
column.num <- reactive({as.character(input$drop2)}) 

editClinicalTable <- reactive({
  input$Enter    
  if (TRACE) cat("In editClinicalTable reactive...\n")
  find.str = isolate(find.str())
  column.num = isolate(column.num())
  replace.str = isolate(replace.str())
  
  if (find.str == "" & replace.str == "") {   # if there is nothing entered it returns the original table
    return(values.edit$table)
  }
  exactMatch = isolate(input$checkbox) # exact match condition
  
  if (exactMatch) {    # while default is false
    find.str = paste("^", find.str, "$", sep = "")
  }
  
  newClinical <- values.edit$table
  
  ### if factor, change to character.  Otherwise we can't replace it. ##
  if (is.factor(newClinical[,column.num])) {
    newClinical[,column.num] = as.character(newClinical[,column.num])
  }
  
  ### if the check box for partial match is checked
  ## TO-DO: I now need adjust for characters ie: \\(months \\)
  partialReplace = isolate(input$survCheckbox)
  
  if (partialReplace) {                 
    newClinical[[column.num]] = gsub(find.str, replace.str, newClinical[[column.num]])  ## fixed = T for symbols
    g.total = grep(find.str, newClinical[,column.num])  
    newClinical[g.total,column.num] = replace.str 
    cat("replacing ", find.str, " with ", replace.str)
  } else {
    g.total = grep(find.str, newClinical[,column.num])  
    cat("g.total = ", g.total, " \n")
    newClinical[g.total,column.num] = replace.str 
    cat("replacing ", find.str, " with ", replace.str)
  }

  cat("replacing table..\n")    
  values.edit$table = newClinical
  return (values.edit$table)
  
}) # end of editClinicalTable() reactive

###########################################
# Expression profiles - data transformation
###########################################
profiles <- reactive({
  if (TRACE) cat("In profiles reactive...\n")
  ### log2 transform (auto-detect) citation ###
  # Edgar R, Domrachev M, Lash AE.
  # Gene Expression Omnibus: NCBI gene expression and hybridization array data repository
  # Nucleic Acids Res. 2002 Jan 1;30(1):207-10
  
  ex <- exprInput()
  if (is.null(ex)) return(NULL)
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC & input$radio == 1) { 
    ex[which(ex <= 0)] <- NaN
    values.edit$log2 = TRUE
  return (ex <- log2(ex)) 
  }    
  
  if (input$radio == 2) {
    values.edit$log2 = TRUE
    return (ex <- log2(exprInput()))   # forced Yes
  }
  values.edit$log2 = FALSE
  return (ex <- exprInput())  # No

  
})

###########################################
# Survival Reactives (time, occurance, x)
###########################################
# Note: We should also include a bsAlert to inform 
# the user to only select unique columns (which they would already do)
time <- reactive({
  if (TRACE) cat("In time reactive...\n")
  input$survTimeUI
})

outcome <- reactive ({
  if (TRACE) cat("In outcome reactive...\n")
 input$survOutcomeUI
})

x <- reactive ({ # not seen in sidebar - changed through the gene/probe drop-downs above plot area
  if (TRACE) cat("In x reactive...\n")
  x = profiles()[selectedProbe(),]
})

#### reactive column selection in summary form for edit bsModal for survival ####

parse.modal <- reactive ({ 
  input$parseEnter
  if (TRACE) cat("In parse.modal reactive...\n")
  parse.modal <- data.frame(
  editSelectedCols()[time()],
  editSelectedCols()[outcome()])
  ## removes any null strings present in the rows of the data.frame
  parse.modal <- parse.modal[!apply(parse.modal, 1, function(x) any(x=="")),] 
  return(parse.modal)
}) 

#######################################################
## SURVIVAL::: Find & Replace Method for editing tables 
######################################################
find.str.surv <- reactive({input$survfind})       
replace.str.surv <- reactive({input$survreplace})

editSelectedCols <- reactive({
  input$parseEnter  # trigger actionButton 
  if (TRACE) cat("In editSelectedCols reactive...\n")
  find.str.surv = isolate(find.str.surv())
  replace.str.surv = isolate(replace.str.surv())
   
  newCols <- values.edit$table
  
#   try.this <- values.edit$original
#   
#   if (input$undo) { # if the revert button is pressed it returns the original data table
#     return(try.this)
#   }
  
  if (find.str.surv == "" & replace.str.surv == "") {   # if there is nothing entered it returns the selected columns
    return(newCols)
  }
  
  ### if factor for both columns, change to character.  Otherwise we can't replace it. ##
  if (is.factor(newCols[,time()]) || is.factor(newCols[,outcome()])) {
    newCols[,time()] = as.character(newCols[,time()])
    newCols[,outcome()] = as.character(newCols[,outcome()])
  }
  
  newCols[[time()]] = gsub(find.str.surv, replace.str.surv, newCols[[time()]], fixed = T)  ## fixed = T for symbols
  g1 = grep(find.str.surv, newCols[,time()])  
  newCols[g1,time()] = replace.str.surv 
  cat("replacing time() ", find.str.surv, " with ", replace.str.surv, "\n")
  
  ## duplicate of time() partial replace block which applies over the entire selected columns
  newCols[[outcome()]] = gsub(find.str.surv, replace.str.surv, newCols[[outcome()]], fixed = T)
  g2 = grep(find.str.surv, newCols[,outcome()])  
  newCols[g2,outcome()] = replace.str.surv 
  cat("replacing outcome() ", find.str.surv, " with ", replace.str.surv, "\n")
  
  values.edit$table = newCols
  return (values.edit$table)  
  
}) # end of editSelectedCols() reactive

######################################
# Initial Code Append to Report
######################################
observeEvent(profiles(),  {
  if (TRACE) cat("In Initial...\n")
  initialCode <- paste0(
     
"
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
  
})

######################################
# Expression Profiles Append to Report
######################################
observeEvent(input$exprAdd, {
  if (TRACE) cat("In report Append for expression profiles...\n")

  exp <- paste0( "## Expression Profiles Plot\n",
    "
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
  cat("outputting test script...\n")
  
  #GSE = "GSE13"
  #GPL = "GPL75"
  #LOG = TRUE
  #PROBE = "aa000380_s_at"
  #DE.column = "source_name_ch1"
  #DE.groups = c("Immature B cells", "Mature B cells")
  
  script.GSE = paste0("GSE = ",quote.it(input$GSE), "\n")
  script.GPL =   paste0("GPL = ", quote.it(isolate(Platforms()[platformIndex()])), "\n")
  script.probe = paste0("PROBE = ", quote.it(input$selectProbes), "\n")
  script.log2 = paste0("LOG2 = ", values.edit$log2, "\n")
  script.DE.column = paste0("DE.column = ", quote.it(input$selectedColumn), "\n")
  
  grps = paste0(sapply(input$Group1Values, quote.it), collapse = ",")
  grps = paste0("c(", grps, ")")  
  script.DE.groups = paste0("DE.groups = ", grps)
    
  print(script.GSE)
  print(script.GPL)
  print(script.probe)
  print(script.log2)
  print(script.DE.column)
  print(script.DE.groups)
  
  test.file = paste0("test/", input$GSE, "_test.R")
  
  cmd = paste0("echo '", script.GSE, script.GPL, script.probe, script.log2, 
               script.DE.column, script.DE.groups, "' > ", test.file)
  
  #cmd = "echo 'hello, how are you\n' > tmp.R"
  system(cmd)
  cmd = paste0("cat GEO-script-template.R >> ", test.file)
  system(cmd) 

  cat("\n==========RUNNING TEST ON", test.file, "========\n")
  rmarkdown::render(test.file)

######################################
  
  s2function <- paste0("## Differential Expression Plot",
                       
"
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
"
yeah = match(as.character(\"",input$selectProbes, "\"),rownames(data.expr))
x = data.expr[yeah,] 
iv = as.character(\"", input$selectedColumn, "\")
m = match(as.character(iv), colnames(data.p)) 
clinical = as.character(data.p[,m])  
selected = as.character(\"", input$Group1Values,"\")
k = clinical %in% selected
y = clinical
y[!k] = NA
y = factor(y, levels = input$Group1Values )
main = paste(input$GSE, input$selectGenes, input$selectProbes, sep = '\')
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
  
  survCode <- paste0("## Survival Analysis Plot") 
  add.graph(survCode)
  
  survfunction <- paste0("

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

survComment <- paste0("
                      
hi = match(as.character(\"",input$selectProbes, "\"),rownames(data.expr))
x = data.expr[hi,]
#print(plot.shiny.km(time = as.double( \"", parse.modal(),"\" [,1]), death = as.integer(\"", parse.modal(), "\" [,2]), x = x)

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
