#############################################################################
## Reactives
#############################################################################

###################################################
# Edit table reactiveValues()
###################################################
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL)

reproducible <-reactiveValues(code = NULL)
add.line <-function(line) {
    reproducible$code = paste(isolate(reproducible$code), line, sep = "\n")
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

observe({
  cat("selected tab = ", input$tabs, "\n")
})

####################################
### dataInput: the GEO object ######
####################################
dataInput <- reactive({
  # Runs the intial input once the button is pressed from within the 
  # reactive statement
  if (TRACE) cat("In dataInput reactive...\n")  
  input$submitButton
  GSE = isolate(gsub(" ", "", input$GSE))   # remove white space
  if (GSE=="") return(NULL)
  closeAlert(session, "GSE-alert")
  closeAlert(session, "GPL-alert")
  cat("creating alert...\n")
   createAlert(session, "alert", alertId = "GSE-alert", title = "Current Status", style = "info",
              content = "Downloading Series (GSE) data from GEO", append = TRUE) 
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
  createAlert(session, "alert", alertId = "GPL-alert", title = "Current Status", style = "info",
              content = "Downloading platform (GPL) data from GEO", append = TRUE) 
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
  if (length(dataInput())==1) return (1)
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
      createAlert(session, "alert", alertId = "geneSymbolAlert", title = "Could not find gene symbol", style = "danger",
                  content = "A gene symbol for this platform could not be found. Please select another platform or analyze another dataset.", append = FALSE)
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
selectGene <- reactive ({
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
  else if (is.null(selectGene())) return(NULL)
  return (as.character(platInfo()[selectGene(),match("ID",colnames(platInfo()))]))
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

## editClinicalTable uses grep as a all-or-nothing replacement -> soon to include gsub for changing parts of cells!   
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
  return (ex <- log2(ex)) }    
  
  if (input$radio == 2) return (ex <- log2(exprInput()))   # forced Yes
  else return (ex <- exprInput())  # No
  
})

###########################################
# Survival Reactives (time, occurance, x)
###########################################
# Note: We should also include a bsAlert to inform 
# the user to only select unique columns (which they would already do)
time <- reactive({
  if (TRACE) cat("In time reactive...\n")
  isolate(input$survTimeUI)
})

outcome <- reactive ({
  if (TRACE) cat("In outcome reactive...\n")
  isolate(input$survOutcomeUI)
})

x <- reactive ({ # not seen in sidebar - changed through the gene/probe drop-downs above plot area
  if (TRACE) cat("In x reactive...\n")
  #input$survXUI
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
