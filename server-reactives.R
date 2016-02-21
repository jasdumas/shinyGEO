#############################################################################
## Reactives
#############################################################################
cat("begin server-reactives.R\n")

shinyjs::onclick("sidebarToggle", 
#  cat("refreshing display...\n")
)

LAST.TAB = "Home"
       content = HTML("To find a dataset, search the <a href = 'http://www.ncbi.nlm.nih.gov/geo/\'>Gene Expression Omnibus</a> and filter by 'Expression profiling by array'.")

createAlert(session, "alert1", alertId = "GSE-begin-alert", 
            title = "Please select a GSE accession number to begin", style = "success",
            content = content, append = FALSE, dismiss = FALSE) 

###################################################
# Edit table reactiveValues()
###################################################
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL, original = NULL, log2 = FALSE, profilesPlot = FALSE, autogen = TRUE, norm = 1, norm.open = FALSE)

reproducible <-reactiveValues(report = NULL)
KM <- reactiveValues(time.col = NULL, outcome.col = NULL, 
	eventYes = NULL, eventNo = NULL, xlab = "Time", ylab = "Survival", hr.format = "high/low", 
	col = c("darkblue", "darkred"))

# expression.code is -1 (do not add code), 0 (add all code), or 1 (update expression code)
CODE <- reactiveValues(stripchart.loaded = FALSE, plot.km.loaded = FALSE, expression.code = 0)


reactiveValues.reset <-function() {
	values.edit$table = NULL
	values.edit$platformGeneColumn = NULL
	values.edit$original = NULL
	values.edit$log2 = FALSE
	values.edit$profilesPlot = FALSE
	values.edit$autogen = TRUE
	values.edit$norm = 1

	reproducible$report = NULL

	KM$time.col = NULL
	KM$outcome.col = NULL
	KM$eventYes = NULL
	KM$eventNo = NULL

        CODE$stripchart.loaded = FALSE
	CODE$plot.km.loaded = FALSE
	CODE$expression.code = 0
}

### functions to append/aggregate a new line to the aceEditor

    
observeEvent(input$GSE, {
  if (input$GSE!= "") {
     shinyjs::enable('submitButton')
  } else {
     shinyjs::disable('submitButton')
  }
})

# open/close normalizationModal
#observeEvent(input$normalizationModal, {
#  cat("normalizationModal...\n")
#  updateRadioButtons(session, "radio", label = "Apply log normalization to expression data:",
#                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3),
#                                    selected = values.edit$norm, inline = TRUE)
#})

## when user saves changes to expression
#observeEvent(input$saveExp, {
#  CODE$expression.code <- 1
#  values.edit$norm <- input$radio
#  toggleModal(session, "normalizationModal", "close") 
#})

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
  	shinyjs::disable('submitButton')
  } else {
    	shinyjs::enable('GSE')
    	shinyjs::enable('platform')  
    	if (input$GSE!="") shinyjs::enable('submitButton')  
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


####################################
### dataInput: the GEO object ######
####################################
dataInput <- reactive({
  input$submitButton
  add.tab()
  updateTabItems(session, "tabs", "Home") 

  # reset variables 
  reactiveValues.reset()

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
  closeAlert(session, "GSE-error-alert")
  closeAlert(session, "GSE-progress-alert")
  closeAlert(session, "GPL-alert")
  closeAlert(session, "Analysis-alert")
  cat("creating alert...\n")

  content = "Downloading Series (GSE) data from GEO" 
# content = HTML("<img src = 'PleaseWait.gif' width=50% height =50%>")

   createAlert(session, "alert1", alertId = "GSE-progress-alert", title = "Current Status", style = "success",
              content = content , append = TRUE, dismiss = FALSE) 
  code = paste0("data.series = getGEO(GEO = \"", GSE, "\", AnnotGPL = FALSE, getGPL = FALSE)")

    geo = try(getGEO(GEO = isolate(GSE), AnnotGPL=FALSE, getGPL = FALSE))

    if (class(geo) == "try-error") {
        content = "This is typically an indication that the GEO data is not in the correct format. Please select another dataset to continue. <p><p>The specific error appears below:<p>"
	content = paste0(content,  gsub("\n", "<p>",geo[[1]]))
	createAlert(session, "alert1", alertId = "GSE-error-alert", title = "Error downloading GEO dataset", style = "danger", content = content, append = FALSE, dismiss = TRUE)
	return(NULL) 
    }
 
    subtract.tab()
    geo
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
  code = paste0("data.expr = exprs(data.series[[data.index]])")
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

###########################################
# Expression profiles - data transformation
###########################################
profiles <- reactive({
  cat("In profiles\n")
  add.tab()
  if (TRACE) cat("In profiles reactive...\n")
  ### log2 transform (auto-detect) citation ###
  # Edgar R, Domrachev M, Lash AE.
  # Gene Expression Omnibus: NCBI gene expression and hybridization array data repository
  # Nucleic Acids Res. 2002 Jan 1;30(1):207-10
 
  ex <- exprInput()
  if (is.null(ex) | is.null(values.edit$table)) return(NULL)
  qx <- as.numeric(quantile(ex, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)

  # remove radio button, so always use autodetect (=1)
  #radio <- input$radio 
  radio = 1

  if (LogC & radio == 1 | radio == 2) { 
    	ex[which(ex <= 0)] <- NaN
    	values.edit$log2 = TRUE
  	subtract.tab()
    	ex <- log2(ex)
  } else {
  	values.edit$log2 = FALSE
  }

  subtract.tab()
  return (ex)  
})

probe.expr <-reactive({
	if (is.null(values.edit$table)) return(NULL)
	if (input$selectGenes=="") return (NULL)
        x = profiles()[input$selectGenes,] # effected
        if (is.null(x)) return(NULL)
        #m = match(names(x), rownames(values.edit$table)) 
        #m = m[!is.na(m)] 
	#cat("=== data for ", length(m), " samples ====\n")
        #x[m]	
	x
})

cat("end server-reactives.R\n")
