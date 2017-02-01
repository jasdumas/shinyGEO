#############################################################################
## Reactives
#############################################################################
shinycat("begin server-reactives.R\n")

# disable a few buttons initially
shinyjs::disable("ClinicalReset")

shinyjs::onclick("sidebarToggle",
#  cat("refreshing display...\n")
)

shinyjs::hide("DEadd")
shinyjs::hide("downloadDE")
shinyjs::hide("formatDEButton")

shinyjs::hide("Survadd")
shinyjs::hide("downloadKM")
shinyjs::hide("formatDEButton2")

LAST.TAB = "Home"
       content = HTML("To find a dataset, search the <a href = 'http://www.ncbi.nlm.nih.gov/geo/\'>Gene Expression Omnibus</a> and filter by 'Expression profiling by array'.")

createAlert(session, "alert1", alertId = "GSE-begin-alert", 
            title = "Please select a GSE accession number to begin...", style = "success",
            content = content, append = FALSE, dismiss = FALSE) 

###################################################
# Edit table reactiveValues()
###################################################
values.edit <- reactiveValues(table = NULL, platformGeneColumn = NULL, original = NULL, log2 = FALSE, profilesPlot = FALSE, autogen = TRUE, norm = 1, norm.open = FALSE)

reproducible <-reactiveValues(report = NULL)
KM <- reactiveValues(time.col = NULL, outcome.col = NULL, generated = FALSE, 
	eventYes = NULL, eventNo = NULL, xlab = "Time", ylab = "Survival", hr.format = "high/low", 
	col = c("darkblue", "darkred"), cutoff = "Median")

DE <- reactiveValues(labels = NULL, col = NULL)

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

	DE$labels = NULL
   	DE$col = NULL

	KM$time.col = NULL
	KM$outcome.col = NULL
	KM$eventYes = NULL
	KM$eventNo = NULL
        KM$generated = FALSE 
	KM$cutoff = "Median"

        CODE$stripchart.loaded = FALSE
	CODE$plot.km.loaded = FALSE
	CODE$expression.code = 0
}

observeEvent(input$GSE, {
  if (input$GSE!= "") {
     shinyjs::enable('submitButton')
  } else {
     shinyjs::disable('submitButton')
  }
})

################################
# Tab Observers 
################################
observeEvent(input$tabs, {
  shinycat("tab change...\n")
 
  if (input$tabs == "NewAnalysis") {
	shinycat("New Analysis")
	shinyjs::enable("GSE")
        runjs("location.reload()") 
  }
 
  if (input$tabs == "FullDataTable") {
	toggleModal(session, "summaryBSModal", "toggle")
        updateTabItems(session, "tabs", LAST.TAB) 
        return (NULL)
  }
  LAST.TAB <<-input$tabs

  if (input$tabs == "DifferentialExpressionAnalysis" | input$tabs == "SurvivalAnalysis") {

	if (input$tabs == "DifferentialExpressionAnalysis") {
		closeAlert(session, alertId = "SelectKM")
	}

  	if (input$selectGenes == "") {
	  createAlert(session, "alert1", alertId = "SelectGene-alert", 
		title = "Please select a probe/gene to continue...", 
		style = "success",
              	content = "To search by a different feature, click on the link below", 
		append = FALSE, dismiss = TRUE) 
	  if(values.edit$platformGeneColumn=="ID") {
		content = paste0("A gene symbol for this platform could not be found. ",
		"You may view the platform data below to select a feature column if desired") 
		createAlert(session, "alert2", alertId = "geneSymbolAlert", 
			title = "Gene symbol not found", style = "danger",
	  		content = content, append = FALSE, dismiss = TRUE)
	   }
        }
  } 
  if (input$tabs == "SurvivalAnalysis") {
	closeAlert(session, alertId = "SelectGroups")
	if (input$selectGenes!="" & !KM$generated) {
	   createAlert(session, "alert1", alertId = "SelectKM", title = "Please select the time/outcome columns to continue...", style = "success",
                content = "Select the time/outcome columns by clicking on the button below")
        }   
  }

  if (input$tabs != "Home") {
	closeAlert(session, alertId = "Analysis-alert")
  	shinyjs::disable('GSE')
  	shinyjs::disable('platform')
  	shinyjs::disable('submitButton')
  } else {
    	#shinyjs::enable('platform')  
	closeAlert(session, alertId = "SelectGene-alert")
	closeAlert(session, alertId = "SelectGroups")
  } 

})


observeEvent(input$selectGenes, {
  shinycat("observing selectGenes...\n")
  if (input$selectGenes=="") {
	if (input$tabs == "DifferentialExpressionAnalysis") {
		closeAlert(session, alertId = "SelectGroups")
	} else if (input$tabs == "SurvivalAnalysis") {
		closeAlert(session, alertId = "SelectKM")
	}
	return (NULL)
  } 
  closeAlert(session, alertId = "SelectGene-alert")

  if (input$tabs == "DifferentialExpressionAnalysis" & is.null(input$Group1Values)) {
  	  createAlert(session, "alert1", alertId = "SelectGroups", title = "Please select your groups for Differential Expression Analysis to continue...", style = "success",
		content = "<p>You may now select the groups to compare by first selecting the appropriate column, and then the group labels of interest.</p><p> If you do not know which column to select, you may view the clinical data table by clicking the 'View Clinical Data Table' link in the sidebar.You may also merge two or more groups together by clicking the 'Merge Groups' button." )
   } else if (input$tabs == "SurvivalAnalysis" & !KM$generated) {
  	  createAlert(session, "alert1", alertId = "SelectKM", title = "Please select the time/outcome columns to continue...", style = "success",
		content = "Select the time/outcome columns by clicking on the button below")
   }
})

observeEvent(input$Group1Values, {
     shinycat("input$selectedGroups = ", input$Group1Values, "\n")
     if (length(input$Group1Values) > 1 || input$Group1Values!="") {
	closeAlert(session, alertId = "SelectGroups")	
     }
})

observeEvent(input$platform, {
  shinycat("observe platform\n")
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
  shinycat("In dataInput reactive...\n")  
  updateTabItems(session, "tabs", "Home") 

  # reset variables 
  reactiveValues.reset()

  # Runs the intial input once the button is pressed from within the 
  # reactive statement

  if (TEST.DATA) {
	return (GEO.test)
  }

  if (is.null(isolate(input$GSE))) {
	return(NULL)
  }
  GSE = isolate(input$GSE)   
  if (GSE=="") return(NULL)
 
  closeAlert(session, "GSE-begin-alert")
  closeAlert(session, "GSE-error-alert")
  closeAlert(session, "GSE-progress-alert")
  closeAlert(session, "GPL-alert")
  closeAlert(session, "Analysis-alert")

  content = "Downloading Series (GSE) data from GEO" 
# content = HTML("<img src = 'PleaseWait.gif' width=50% height =50%>")

   createAlert(session, "alert1", alertId = "GSE-progress-alert", title = "Current Status", style = "success",
              content = content , append = TRUE, dismiss = FALSE) 

    geo = try(getGEO(GEO = isolate(GSE), AnnotGPL=FALSE, getGPL = FALSE), silent = TRUE)

    if (class(geo) == "try-error") {
        content = "This is typically an indication that the GEO data is not in the correct format. Please select another dataset to continue. <p><p>The specific error appears below:<p>"
	content = paste0(content,  gsub("\n", "<p>",geo[[1]]))
	createAlert(session, "alert1", alertId = "GSE-error-alert", title = "Error downloading GEO dataset", style = "danger", content = content, append = FALSE, dismiss = TRUE)
	return(NULL) 
    }

    shinyjs::disable('GSE')
    shinyjs::disable('submitButton')
    geo
})

################################################################
### Platforms: returns the platform only if the GSE # is entered 
################################################################  
Platforms <- reactive({
  shinycat("In Platforms reactive...\n")
  if (is.null(dataInput())) {
    return(NULL)
  }
  closeAlert(session, "GSE-progress-alert")  
  ans = as.character(sapply(dataInput(), annotation))   
  ans 
})

#########################################
### Platforms: the chosen platform Index 
#########################################  
platformIndex <- reactive({
#  input$submitPlatform
  shinycat("In platformIndex reactive...\n")
  if (TEST.DATA) {
	return(1)
  }
  if (is.null(dataInput()) | length(isolate(input$platform)) ==0) {
    return(NULL)
  }
  if (length(dataInput())==1) return (1)
  m = match((input$platform), as.character(sapply(dataInput(), annotation)))   
  if (is.na(m)) return(NULL)
  shinyjs::disable('platform')
  return(m)
})

################################################
### return selected platform info as a table
################################################  
platInfo <- reactive({
  shinycat("In platInfo reactive...\n")
  if (is.null(Platforms()) | is.null(platformIndex())) return (NULL)

  closeAlert(session, "GPL-alert")
  if (!TEST.DATA) {
    createAlert(session, "alert1", alertId = "GPL-alert", title = "Current Status", style = "info",
              content = "Downloading platform (GPL) data from GEO", append = TRUE, dismiss = FALSE) 
  }
  a = isolate(Platforms())
  b = isolate(platformIndex())

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
  return(r)
})

#########################################################
### returns a data.frame of gene and probe names used by
### selectGenes selectizeInput 
#########################################################
geneNames <- reactive ({
  shinycat("In geneNames reactive...\n")

  plat.info = platInfo()

  if (is.null(plat.info)) {
	return (NULL)
  }

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
  probes = as.character(plat.info$ID)
  genes = "Gene not specified"
  if (!is.null(gene.column)) { 
    genes = as.character(plat.info[[gene.column]])
    label = paste0(genes, " (",  probes, ")")
  } else {
    label = probes
  } 

  # create data.frame for use with selectizeInput #
  dd = data.frame(value = probes, label = label, genes = genes, probes = probes)
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
  shinycat("observe platform to update clinical table...\n")
  if (is.null(dataInput()) | is.null(platformIndex())) {
    values.edit$table = NULL
    return(NULL)
  }

  # only update table if values.edit$table is null
  if (is.null(values.edit$table)) {
    if (TEST.DATA) {
        values.edit$table = CLINICAL.test
    } else {
      values.edit$table = as.data.frame(pData(phenoData(object = dataInput()[[platformIndex()]])))
    }
  }
})

######################################################
# exprInput - expression data for selected platform
######################################################
exprInput <- reactive({
  shinycat("In exprInput reactive...\n")
  pi = platformIndex()
  if (is.null(dataInput()) | is.null(pi)) {
	return(NULL)
  }
  pl=Platforms()[platformIndex()]
  ans = exprs(dataInput()[[pi]])
  return(ans)
})


###################################################
# get possible values of the selected column names
###################################################
groupsForSelectedColumn <- reactive({
  shinycat("In groupsForSelectedColumn reactive...\n")
  vars = values.edit$table
  if (is.null(vars) | is.null(input$selectedColumn)) {
	return(NULL)   
  }
  
  vars <- vars[, as.character(input$selectedColumn)] 
  vars = factor(vars)
  return(as.list(levels(vars)))
})  

###################################################
# get default levels of the selected column, either
# all levels or NULL if number of levels exceeds
# a cut-off 
###################################################
defaultGroupsForSelectedColumn <- reactive({
  shinycat("In defaultGroupsForSelectedColumn reactive...\n")
  g = groupsForSelectedColumn()
  if (length(g) > 5) return(NULL) 
  g    
})

###########################################
# Expression profiles - data transformation
###########################################
profiles <- reactive({
  shinycat("In profiles reactive...\n")
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
    	ex <- log2(ex)
  } else {
  	values.edit$log2 = FALSE
  }

  return (ex)  
})

probe.expr <-reactive({
	if (is.null(values.edit$table)) return(NULL)
	if (input$selectGenes=="") return (NULL)
        x = profiles()[input$selectGenes,] # selected
        # make sure expression values are named
        if (is.null(names(x))) names(x) = colnames(profiles()) 
        if (is.null(x)) return(NULL)
	x
})


##### enable/disable DE graph buttons ###

observe({

  if (is.null(input$Group1Values)) { 
     shinyjs::hide('DEadd')
     shinyjs::hide('downloadDE')
     shinyjs::hide('formatDEButton')
  } else {
     shinyjs::show('DEadd')
     shinyjs::show('downloadDE')
     shinyjs::show('formatDEButton')
  }
})

#######################################
# Download handler for DE data export
#######################################
output$downloadDE <- downloadHandler(
    filename = function() {
      file = paste(input$GSE,"_",input$platform,"_",input$selectGenes,"_", Sys.time(),"-DE", ".csv", sep = "")
	file = gsub(":", "-",file)
	file = gsub(" ", "_",file)
  msg = paste0("<H4>Differential Expression Data Exported</H4><p> The expression and grouping data has been downloaded to the following file in your Downloads folder: <b>", file, "</p>")
  createAlert(session,"alert2",content = msg, style="success",dismiss=TRUE, append = TRUE)
        return(file)
},

   content = function(file) {
	s = stripReactive()
	if (is.null(s)) return(NULL)
        d = data.frame(ID = names(s$x), X = s$x, Group = s$y)
	d = subset(d, !is.na(Group))
	if (nrow(d) > 0) {
		o = order(d$Group)
 		write.csv(d[o,], file, row.names = FALSE)
	}	
    }
 )


#######################################
# Download handler for KM data export
#######################################
output$downloadKM <- downloadHandler(
    filename = function() {
      file = paste(input$GSE,"_",input$platform,"_",input$selectGenes,"_", Sys.time(),"-KM", ".csv", sep = "")
	file = gsub(":", "-",file)
	file = gsub(" ", "_",file)
  msg = paste0("<H4>Survival Data Exported</H4><p> The expression and survival data has been downloaded to the following file in your Downloads folder: <b>", file, "</b></p>")
  createAlert(session,"alert2",content = msg, style="success",dismiss=TRUE, append = TRUE)
        return(file)
},

   content = function(file) {
	km = kmReactive()
	if (is.null(km)) return(NULL)
	optimal.cut = TRUE
        if (KM$cutoff == "Median") optimal.cut = FALSE 
        res = plot.shiny.km(time = km$time, death = km$death, x = km$x, 
			    ids = km$id, no.plot = TRUE, optimal.cut = optimal.cut)
	if (is.null(res)) return(NULL)
 		write.csv(res, file, row.names = FALSE)
	}	
 )



