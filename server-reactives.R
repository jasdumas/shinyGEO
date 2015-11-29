#############################################################################
## Reactives
#############################################################################
cat("begin server-reactives.R\n")

LAST.TAB = "Home"

createAlert(session, "alert0", alertId = "Welcome-alert", title = "shinyGEO", style = "danger",
  	content = "shinyGEO is a tool for downloading and analyzing gene expression data from the
                Gene Expression Omnibus (GEO), in order to evaluate whether or not a gene of interest is (1) associated with survival in datasets with this information and (2) differentially expressed across two or more groups.", dismiss = TRUE)

       content = HTML("To find a dataset, search the <a href = 'http://www.ncbi.nlm.nih.gov/geo/\'>Gene Expression Omnibus</a> and filter by 'Expression profiling by array'.")

createAlert(session, "alert1", alertId = "GSE-begin-alert", 
            title = "Please select a GSE accession number to begin", style = "success",
            content = content, append = FALSE, dismiss = FALSE) 

###################################################
# Edit table reactiveValues()
###################################################
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL, original = NULL, log2 = FALSE)
reproducible <-reactiveValues(code = NULL, report = NULL)
KM <-reactiveValues(eventNames = NULL, outcome = NULL)

### functions to append/aggregate a new line to the aceEditor
add.line <-function(line) {
    reproducible$code = paste(isolate(reproducible$code), line, sep = "\n")
}

add.graph <-function(line) {  ## add graphic info to ace editor for the report
   reproducible$report = paste(isolate(reproducible$report), line, sep = "\n")
}


################################
# Tab Observers 
################################
observeEvent(input$tabs, {
  cat("tab change...\n")
  if (input$tabs == "FullDataTable") {
	toggleModal(session, "summaryBSModal", "toggle")
        updateTabItems(session, "tabs", LAST.TAB) 
        return (NULL)
  }
  LAST.TAB <<-input$tabs

  if (input$tabs == "DifferentialExpressionAnalysis") {
  	if (input$selectGenes == "") {
	  createAlert(session, "alert1", alertId = "SelectGene-alert", title = "Current Status", style = "success",
              content = "Please select a gene/probe to continue", append = FALSE, dismiss = TRUE) 
        } 
  } else if (input$tabs == "SurvivalAnalysis") {
	closeAlert(session, alertId = "SelectGroups")
  	if (input$selectGenes == "") {
	  createAlert(session, "alert1", alertId = "SelectGene-alert", title = "Current Status", style = "success",
              content = "Please select a gene/probe to continue", append = FALSE, dismiss = TRUE) 
        } 
  }

  if (input$tabs != "Home") {
	closeAlert(session, alertId = "Analysis-alert")
  } 

})


observeEvent(input$selectGenes, {
  cat("observing selectGenes...\n")

  if (input$tabs == "DifferentialExpressionAnalysis" & is.null(input$selectedGroups)) {
          closeAlert(session, alertId = "SelectGene-alert")
  	  createAlert(session, "alert1", alertId = "SelectGroups", title = "Group selection", style = "success",
		content = "You may now view the clinical data and select your groups to continue by first selecting the column with the data of interest."
	  )
	}
   }
)

observeEvent(input$ClinicalDataBtn2, {
	cat("testModal\n")
	toggleModal(session, "testModal", "toggle")
})


observeEvent(input$platform, {
  cat("observe platform\n")
  closeAlert(session, "geneSymbolAlert")
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
})


observeEvent(reproducible$code, {
     cat("updateAceEditor for code\n")
     updateAceEditor(session, "myEditor", reproducible$code,
                         mode="r", theme="chrome")
 })

observeEvent(reproducible$report, {
     cat("updateAceEditor for report\n")
     updateAceEditor(session, "rmd", reproducible$report, 
                  mode = "markdown", theme = "chrome")
})


observeEvent(input$submitButton, {
  cat("observe submitButton\n") 
  closeAlert(session, "geneSymbolAlert")
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
  KM$outcome = NULL
})


####################################
### dataInput: the GEO object ######
####################################

dataInput <- reactive({
  add.tab()
  input$submitButton
  # Runs the intial input once the button is pressed from within the 
  # reactive statement
  if (TRACE) cat("In dataInput reactive...\n")  

  if (TEST.DATA) {
	cat("return GEO.test data\n")
	subtract.tab()
	return (GEO.test)
  }

  if (is.null(isolate(input$GSE))) {
	subtract.tab()
	return(NULL)
  }
  GSE = isolate(input$GSE)   
  if (GSE=="") return(NULL)
 
  closeAlert(session, "GSE-begin-alert")
  closeAlert(session, "GSE-progress-alert")
  closeAlert(session, "GPL-alert")
  cat("creating alert...\n")

  content = "Downloading Series (GSE) data from GEO" 
# content = HTML("<img src = 'PleaseWait.gif' width=50% height =50%>")

   createAlert(session, "alert1", alertId = "GSE-progress-alert", title = "Current Status", style = "success",
              content = content , append = TRUE, dismiss = FALSE) 
  code = paste0("data.series = getGEO(GEO = \"", GSE, "\", AnnotGPL = FALSE, getGPL = FALSE)")
  add.line(code)

    geo = getGEO(GEO = isolate(GSE), AnnotGPL=FALSE, getGPL = FALSE) 
    subtract.tab()
    geo
})

####################################
### display Gene Series Info  ######
####################################

output$dataInputPrint <- renderPrint({
  dataInput()
})

################################################################
### Platforms: returns the platform only if the GSE # is entered 
################################################################  
Platforms <- reactive({
  add.tab()
  if (TRACE) cat("In Platforms reactive...\n")
  if (is.null(dataInput())) {
    subtract.tab()
    return(NULL)
  }
  closeAlert(session, "GSE-progress-alert")  
  ans = as.character(sapply(dataInput(), annotation))   
  print(ans)
  subtract.tab()
  ans 
})

#########################################
### Platforms: the chosen platform Index 
#########################################  
platformIndex <- reactive({
#  input$submitPlatform
  add.tab()
  if (TRACE) cat("In platformIndex reactive...\n")
  if (TEST.DATA) {
	subtract.tab()
	return(1)
  }
  if (is.null(dataInput()) | length(isolate(input$platform)) ==0) {
    return(NULL)
  }
  if (length(dataInput())==1) return (1)
  cat("matching platform = ", isolate(input$platform), "\n")
  m = match((input$platform), as.character(sapply(dataInput(), annotation)))   
  subtract.tab() 
  if (is.na(m)) return(NULL)
  return(m)
})

################################################
### return selected platform info as a table
################################################  
platInfo <- reactive({
  add.tab()
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

  t = NULL
  if (TEST.DATA) {
	t = GPL.test
  } else { 
    t = Table(getGEO(Platforms()[platformIndex()]))
  }
  

 
  k = t[,"ID"]
  common.probes = intersect(row.names(exprInput()), as.character(k))
  n = match(common.probes, k)
  r = t[n,]
  subtract.tab()
  return(r)
})

## update gene and probe names when platform index changes
## we do this so the user is not waiting for genes/probes
## to be displayed when moving to Differential Expression
## Analysis panel
#observe({
#  platformIndex()
#  geneNames()
#  probeNames()
#})

#########################################################
### returns a data.frame of gene and probe names used by
### selectGenes selectizeInput 
#########################################################
geneNames <- reactive ({
  add.tab()
  if (TRACE) cat("In geneNames reactive...\n")

  plat.info = platInfo()

  if (is.null(plat.info)) {
        subtract.tab()
	cat("return NULL...\n")
	return (NULL)
  }
  cat("find column...\n")

  gene.column = values.edit$platformGeneColumn
  if (is.null(gene.column)) {
    check.names = c("Gene Symbol", "GeneSymbol", "Symbol",
                 "GENE_SYMBOL")
    m = check.names%in%colnames(plat.info) 
    w = which(m)
    if (length(w) == 0) {
      createAlert(session, "alert2", alertId = "geneSymbolAlert", title = "Could not find gene symbol", style = "danger",
                  content = "A gene symbol for this platform could not be found. Please select another platform or analyze another dataset.", 
                  append = FALSE, dismiss = TRUE)
      values.edit$platformGeneColumn = NULL
      gene.column = NULL
    } else { 
      gene.column = check.names[w[1]]
      values.edit$platformGeneColumn = gene.column
    }
  }
  cat("gene column is  ", gene.column, "\n")
  probes = as.character(plat.info$ID)
  cat("got probes...\n")
  genes = "Gene not specified"
  if (!is.null(gene.column)) { 
  cat("finding column: ", gene.column, "\n")
    genes = as.character(plat.info[[gene.column]])
    cat("gene[1] = ", genes[1], "\n")
    label = paste0(genes, " (",  probes, ")")
  } else {
    label = probes
  } 

  # create data.frame for use with selectizeInput #

  cat("probes1 = ", probes[1], "\n")
  cat("label1 = ", label[1], "\n")
  cat("gene1 = ", genes[1], "\n")

  cat("length probes = ", length(probes), "\n")
  cat("length label = ", length(label), "\n")
  cat("length genes = ", length(genes), "\n")

  cat("creating geneName data.frame...\n")
#  save(probes, label, genes, file = "look.RData")
  
  tmp = 1:length(probes)

#  dd = data.frame(value = tmp, label = tmp, genes = tmp, probes = tmp)
  dd = data.frame(value = probes, label = label, genes = genes, probes = probes)
#  save(probes, label, genes, dd, file = "look.RData")
  cat("data.frame created\n")
  subtract.tab()
  return(dd)
})


##############################################################
## gene label of the form gene (probe) for the selected probe
##############################################################
geneLabel <-reactive({
   if (input$selectGenes == "" ) return("")
   genes = geneNames()
   m = match(input$selectGenes, genes$probes)
   genes$label[m]  
})


#######################################################
# clinicalInput: Clinical Data
#######################################################
clinicalInput <- reactive({
  add.tab()
  if (TRACE) cat("In clinicalInput reactive...\n")

  if (is.null(dataInput()) | is.null(platformIndex())) {
    subtract.tab()
    return(NULL)
  }
  p = NULL
  cat("set p to NULL\n")
  ### Checks if initial values.edit$table is NULL (which it is set to initially)
  if (!is.null(values.edit$table)) {
    p = values.edit$table
    cat("p = values.edit$table\n")
  } else {
    code = paste0("data.p = pData(data.series[[data.index]])")
    add.line(code)
    if (TEST.DATA) {
        cat("set p to CLINICAL.test\n")
	p = CLINICAL.test
    } else {
      p = as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
    }
  } 
  #####################################################################
  #  display only columns that have more than one possible value; this
  #   removes many columns such as contact info. In addition all 
  #   columns specified by RM.COLS will be removed
  #####################################################################
  
  RM.COLS = c("status", "last_update_date", "submission_date", 
	"supplementary_file", "geo_accession")
  num.levels = apply(p, 2, function(x) nlevels(as.factor(x)))
  
  p = p[,num.levels > 1]
  
  m = match(RM.COLS, colnames(p))
  
  m=m[!is.na(m)]
  
  if (length(m) > 0) p=p[,-m, drop = FALSE]
  
  m = match(colnames(exprInput()), rownames(p))
  
  p = p[m,]
  
  values.edit$table = p
  subtract.tab()	
  return(p)

})

######################################################
# exprInput - expression data for selected platform
######################################################
exprInput <- reactive({
  add.tab()
  if (TRACE) cat("In exprInput reactive...\n")
  pi = platformIndex()
  cat("pi = ", pi, "\n")
  if (is.null(dataInput()) | is.null(pi)) {
	subtract.tab()
	return(NULL)
  }
  pl=Platforms()[platformIndex()]
  code = paste0("data.index = match(\"", pl, "\", sapply(data.series, annotation))")
  add.line(code)
  code = paste0("data.expr = exprs(data.series[[data.index]])")
  add.line(code)
  ans = exprs(dataInput()[[pi]])
  subtract.tab()
  return(ans)
})

########################################
### ColumnNames of clinicial data table
########################################  
ColumnNames <- reactive({
  add.tab()
  if (TRACE) cat("In ColumnNames reactive...\n")
  if (is.null(clinicalInput()) | is.null(exprInput())) {
	subtract.tab()
	return(NULL)
  }
  vars = colnames(clinicalInput())
  vars <- as.list(vars)
  subtract.tab()
  return(vars)
})

########################################
### Summary of Clinical Data table
########################################  
clinicalDataSummary <- reactive({
  add.tab()
  if (TRACE) cat("In clinicalDataSummary reactive...\n")
  t = clinicalInput()
  if (is.null(t)) {
	subtract.tab()
	return(NULL)
  }
  cat("colnames = ")
  vars = colnames(t)
  cat("got ncols = ", length(vars), "\n")
  a = apply(t, 2, function(x)levels(as.factor(x)))
  
  ## format function to truncate row contents with a place holder " ..."
  format.it <-function(x, max) {
    x = x[x!=""]
    if (length(x) <= max) return(x)
    x[max] = " ..."
    return(x[1:max])
  }
  a = lapply(a, format.it, Inf)
  
  a = sapply(a, paste, collapse = ", ")
  cat("end clinicalDataSummary reactive\n")
  subtract.tab()
  cbind(variable = vars, values = a)
  #cbind(values = a)
})


###################################################
# get possible values of the selected column names
###################################################
groupsForSelectedColumn <- reactive({
  add.tab()
  if (TRACE) cat("In groupsForSelectedColumn reactive...\n")
  vars = values.edit$table
  if (is.null(vars) | is.null(input$selectedColumn)) {
	subtract.tab()
	return(NULL)   
  }
  
  vars <- vars[, as.character(input$selectedColumn)] 
  vars = factor(vars)
  subtract.tab()
  return(as.list(levels(vars)))
})  

###################################################
# get default levels of the selected column, either
# all levels or NULL if number of levels exceeds
# a cut-off 
###################################################
defaultGroupsForSelectedColumn <- reactive({
  add.tab()
  if (TRACE) cat("In defaultGroupsForSelectedColumn reactive...\n")
  g = groupsForSelectedColumn()
  subtract.tab()
  if (length(g) > 5) return(NULL) 
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
  add.tab()  
  if (TRACE) cat("In editClinicalTable reactive...\n")
  find.str = isolate(find.str())
  column.num = isolate(column.num())
  replace.str = isolate(replace.str())
  
  if (find.str == "" & replace.str == "") {   # if there is nothing entered it returns the original table
    cat("\t return current table\n")
    subtract.tab()
    return(values.edit$table)
  }
  exactMatch = isolate(input$checkbox) # exact match condition
  
  if (exactMatch) {    # while default is false
    find.str = paste("^", find.str, "$", sep = "")
  }
  
  newClinical <- values.edit$table
  
  cat("\t set newClinical\n")
  ### if factor, change to character.  Otherwise we can't replace it. ##
  if (is.factor(newClinical[,column.num])) {
    newClinical[,column.num] = as.character(newClinical[,column.num])
  }
  
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
  subtract.tab()
  return (values.edit$table)
  
}) # end of editClinicalTable() reactive

###########################################
# Expression profiles - data transformation
###########################################
profiles <- reactive({
  cat("HHHH in profiles\n")
  add.tab()
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
  subtract.tab()
  return (ex <- log2(ex)) 
  }    
  
  if (input$radio == 2) {
    values.edit$log2 = TRUE
    subtract.tab()
    return (ex <- log2(exprInput()))   # forced Yes
  }
  values.edit$log2 = FALSE
  subtract.tab()
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
  add.tab()
  if (TRACE) cat("In x reactive...\n")
  subtract.tab()
  x = profiles()[input$selectGenes,]
  x
})

#### reactive column selection in summary form for edit bsModal for survival ####

observeEvent(input$parseButton, {
  cat("updating event names\n")
  cat("outcome = ", outcome(), "\n")
  values = unique(as.character(parse.modal()[,2]))
  cat("valuess= ", values, "\n")
  ans = setdiff(values, c("", " ", NA))
  KM$eventNames = ans
})

parse.modal <- reactive ({ 
  input$parseEnter
  add.tab()
  if (TRACE) cat("In parse.modal reactive...\n")
  parse.modal <- data.frame(
  editSelectedCols()[time()],
  editSelectedCols()[outcome()])
  ## removes any null strings present in the rows of the data.frame
  #parse.modal <- parse.modal[!apply(parse.modal, 1, function(x) any(x=="")),]
  subtract.tab() 
  return(parse.modal)
}) 

#######################################################
## SURVIVAL::: Find & Replace Method for editing tables 
######################################################
find.str.surv <- reactive({input$survfind})       
replace.str.surv <- reactive({input$survreplace})


###########################################################
# Currently, clicking submit will format the time column,
# overwriting the original table; while the outcome will
# be changed to 0s and 1s based on the drop down selection.
# The updated outcome is saved in KM$outcome, and the 
# original outcomes are not overwritten. This is partly
# because overwriting the originals causes some kind of
# issue where the clinical data tables get stuck on 
# 'processing' and are not displayed
###########################################################
editSelectedCols <- reactive({
  input$parseEnter  # trigger actionButton
  add.tab()
  # make sure to isolate everything so we're not repeatedly calling this
  time.col = isolate(time())
  outcome.col = isolate(outcome())
 
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
    subtract.tab()
    return(newCols)
  }
  
  ### if factor for both columns, change to character.  Otherwise we can't replace it. ##
  if (is.factor(newCols[,time.col]) || is.factor(newCols[,outcome.col])) {
    newCols[,time.col] = as.character(newCols[,time.col])
    newCols[,outcome.col] = as.character(newCols[,outcome.col])
  }
  
  newCols[[time.col]] = gsub(find.str.surv, replace.str.surv, newCols[[time.col]], fixed = T)  ## fixed = T for symbols
  g1 = grep(find.str.surv, newCols[,time.col])  
  newCols[g1,time.col] = replace.str.surv 
  cat("replacing time.col ", find.str.surv, " with ", replace.str.surv, "\n")
 
 ##################################
 ## find and replace for outcome ##  
 ##################################
  ## duplicate of time() partial replace block which applies over the entire selected columns
#  newCols[[outcome()]] = gsub(find.str.surv, replace.str.surv, newCols[[outcome()]], fixed = T)
#  g2 = grep(find.str.surv, newCols[,outcome()])  
#  newCols[g2,outcome()] = replace.str.surv 
#  cat("replacing outcome() ", find.str.surv, " with ", replace.str.surv, "\n")

  eventYes = isolate(input$eventYes)
  eventNo = isolate(input$eventNo)
  tmp = rep(NA, nrow(newCols)) 
  cat("event = ", isolate(eventYes), "\n")
  cat("no = ", isolate(eventNo), "\n")
  tmp[newCols[[outcome.col]] %in% eventYes ] = 1
  tmp[newCols[[outcome.col]] %in% eventNo ] = 0

  if (all(is.na(tmp))) return (values.edit$table)

  cat("print tmp: ")
  print(tmp)

  KM$outcome = tmp


  values.edit$table <- newCols

  subtract.tab()
  return (values.edit$table)  
  
}) # end of editSelectedCols() reactive

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


cat("end server-reactives.R\n")
