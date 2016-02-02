#############################################################################
## Reactives
#############################################################################
cat("begin server-reactives.R\n")

shinyjs::onclick("sidebarToggle", 
#  cat("refreshing display...\n")
)


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
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL, original = NULL, log2 = FALSE, profilesPlot = FALSE, autogen = TRUE)

reproducible <-reactiveValues(code = NULL, report = NULL)
KM <-reactiveValues(eventNames = NULL, outcome = NULL)


### functions to append/aggregate a new line to the aceEditor
add.line <-function(line) {
    reproducible$code = paste(isolate(reproducible$code), line, sep = "\n")
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

	if(values.edit$platformGeneColumn=="ID") {
		content = paste0("A gene symbol for this platform could not be found. ",
		"You may view the platform data below to select a feature column if desired") 
		createAlert(session, "alert2", alertId = "geneSymbolAlert", 
			title = "Gene symbol not found", style = "danger",
	  		content = content, append = FALSE, dismiss = TRUE)
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
  shinyjs::disable('GSE')
  shinyjs::disable('platform')
  } else {
    shinyjs::enable('GSE')
    shinyjs::enable('platform')  
	closeAlert(session, alertId = "SelectGene-alert")
	closeAlert(session, alertId = "SelectGroups")
  } 

})


observeEvent(input$selectGenes, {
  cat("observing selectGenes...\n")

  if (input$tabs == "DifferentialExpressionAnalysis" & is.null(input$Group1Values)) {
          closeAlert(session, alertId = "SelectGene-alert")
  	  createAlert(session, "alert1", alertId = "SelectGroups", title = "Group selection", style = "success",
		content = "You may now view the clinical data and select your groups to continue by first selecting the column with the data of interest."
	  )
	}
   }
)

observeEvent(input$Group1Values, {
	cat("input$selectdGroups = ", input$Group1Values, "\n")
     if (input$Group1Values!="") {
	closeAlert(session, alertId = "SelectGroups")	
     }
})

observeEvent(input$platform, {
  cat("observe platform\n")
  closeAlert(session, "geneSymbolAlert")
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
  values.edit$autogen = TRUE
})


observeEvent(reproducible$code, {
     cat("updateAceEditor for code\n")
     updateAceEditor(session, "myEditor", reproducible$code,
                         mode="r", theme="chrome")
 })



observeEvent(input$hiButton, {
  cat("observe submitButton\n") 
  updateTabItems(session, "tabs", "Home") 
})


####################################
### dataInput: the GEO object ######
####################################

dataInput <- reactive({
  input$submitButton
  add.tab()
  updateTabItems(session, "tabs", "Home") 
  # reset variables 
  values.edit$table <- NULL  
  values.edit$platformGeneColumn <- NULL
  KM$outcome = NULL


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
  if (!TEST.DATA) {
    createAlert(session, "alert1", alertId = "GPL-alert", title = "Current Status", style = "info",
              content = "Downloading platform (GPL) data from GEO", append = TRUE, dismiss = FALSE) 
  }
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
    check.names = toupper(c("Gene Symbol", "GeneSymbol", 
				"Gene_Symbol","Gene", "Symbol"))
    m = check.names%in%toupper(colnames(plat.info)) 
    w = which(m)
    if (length(w) == 0) {
      gene.column = colnames(platInfo())[1]  # use 1st col if not found
    } else { 
      i = match(toupper(check.names[w[1]]), toupper(colnames(plat.info)))
      gene.column = colnames(plat.info)[i] 
    }
    values.edit$platformGeneColumn = gene.column
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

  cat("creating geneName data.frame...\n")
  
  dd = data.frame(value = probes, label = label, genes = genes, probes = probes)
  cat("data.frame created\n")
  subtract.tab()
  genes[genes==""] = NA
  o = order(genes, probes)
  return(dd[o,])
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


### when platform changes, update clinical table
observe({
  add.tab()
  if (TRACE) cat("observe platform to update clinical table...\n")
  if (is.null(dataInput()) | is.null(platformIndex())) {
    subtract.tab()
    cat("update table to NULL\n")
    values.edit$table = NULL
    return(NULL)
  }

  # only update table if values.edit$table is null
  if (is.null(values.edit$table)) {
    code = paste0("data.p = pData(data.series[[data.index]])")
    add.line(code)
    if (TEST.DATA) {
        cat("set table to CLINICAL.test\n")
        values.edit$table = CLINICAL.test
    } else {
	cat("set table pData(dataInput)\n")	
      values.edit$table = as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
    }
  }
  subtract.tab()
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





shinyjs::onclick("noGenePlatLink", {
	cat("link link\n")
	toggleModal(session, "platformModal", toggle = "open")
  }
)





cat("end server-reactives.R\n")
