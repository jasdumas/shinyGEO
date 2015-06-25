#############################################################################
## Reactives
#############################################################################

####################################
### dataInput: the GEO object ######
####################################
dataInput <- reactive({
  # Runs the intial input once the button is pressed from within the 
  # reactive statement
  input$submitButton
  GSE = isolate(gsub(" ", "", input$GSE))   # remove white space
  if (GSE=="") return(NULL)
  getGEO(GEO = isolate(GSE), AnnotGPL=TRUE)
})

################################################################
### Platforms: returns the platform only if the GSE # is entered 
################################################################  
Platforms <- reactive({
  if (is.null(dataInput())) {
    return(NULL)
  }
  as.character(sapply(dataInput(), annotation))    
})

#########################################
### Platforms: the chosen platform Index 
#########################################  
platformIndex <- reactive({
  if (is.null(dataInput()) | length(input$platform) ==0) {
    return(NULL)
  }
  if (length(dataInput())==1) return (1)
  m = match(input$platform, as.character(sapply(dataInput(), annotation)))    
  return(m)
})

################################################
### return selected platform info as a table
################################################  
platInfo <- reactive({
  if (is.null(Platforms())) return (NULL)
  t = Table(getGEO(Platforms()[platformIndex()]))
  k = t[,"ID"]
  common.probes = intersect(row.names(exprInput()), as.character(k))
  n = match(common.probes, k)
  r = t[n,]
  return(r)
})

##############################################
### unique gene names for selected platform
##############################################
geneNames <- reactive ({
  if (is.null(platInfo())) return (NULL)
  m = match("Gene Symbol",colnames(platInfo()))
  if(is.na(m)) { m = match("Symbol", colnames(platInfo()))}
  t = unique(platInfo()[,m])  
  return(sort(as.character(t)))
})

########################################
### selected Gene index
########################################  
selectGene <- reactive ({
  if (is.null(input$selectGenes)) return (NULL)
  m = match("Gene Symbol",colnames(platInfo()))
  if(is.na(m)) { m = match("Symbol", colnames(platInfo()))}
  g = grep(paste("^",input$selectGenes,"$",sep=""), as.character(platInfo()[,m]))
  return(g)
})

#############################################
### probe names for current expression data
#############################################  
probeNames <- reactive({
  if (is.null(dataInput())) return(NULL)
  else if (is.null(selectGene())) return(as.list(row.names(exprInput())))
  return (as.character(platInfo()[selectGene(),match("ID",colnames(platInfo()))]))
})

#######################################################
# clinicalInput: Clinical Data
#######################################################
clinicalInput <- reactive({
  if (is.null(dataInput()) | is.null(platformIndex())) {
    return(NULL)
  }
  ### Checks if initial values.edit$table is NULL (which it is set to initially)
  if (!is.null(values.edit$table)) {
    p = values.edit$table
  } else {
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
  pi = platformIndex()
  if (is.null(dataInput()) | is.null(pi)) return(NULL)
  ans = exprs(dataInput()[[pi]])
  return(ans)
})

########################################
### selected probe as row number
########################################  
selectedProbe <- reactive ({
  return(match(input$selectProbes,rownames(exprInput())))
})

########################################
### ColumnNames of clinicial data table
########################################  
ColumnNames <- reactive({
  if (is.null(clinicalInput()) | is.null(exprInput())) return(NULL)
  vars = colnames(clinicalInput())
  vars <- as.list(vars)
  return(vars)
})

########################################
### Summary of Clinical Data table
########################################  

clinicalDataSummary <- reactive({
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
  vars = values.edit$table
  if (is.null(vars)) return(NULL)      
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
  g = groupsForSelectedColumn()
  if (length(g) > 8) return(NULL) 
  g    
})

###################################################
# Edit table reactiveValues()
###################################################

values.edit <- reactiveValues(table = NULL)

observeEvent(input$submitButton, { 
values.edit$table <- NULL  
})
