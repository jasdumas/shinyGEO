##########################################################
# display functions for conditional panels              ##
##########################################################

load("series/series.RData")
load("platforms/platforms.RData")

createAlert(session, "addCodeDEAlert", alertId = "DE-add-alert", title = "", style = "success",
            content = "R Code for Differential Expression Analysis Added", append = FALSE, dismiss = FALSE) 

createAlert(session, "addCodeSurvAlert", alertId = "Surv-add-alert", title = "", style = "success",
            content = "R Code for Survival Analysis Added", append = FALSE, dismiss = FALSE) 


m = matrix(rnorm(1000), ncol=20)
rownames(m) = paste0("row", 1:nrow(m))

opp = list(dom = 'Rlfrtip', #ajax = list(url = action1), 
                       #scrollX = "auto",
                       #scrollY = "400px",
                       paging = T, 
                       searchHighlight = TRUE,
                       columnDefs = list(list(
                         targets = 1: ncol(m), # applies to the entire table
                         render = JS(
                           "function(data, type, row, meta) {",
                           "return type == 'display' && data.length > 20 ?",
                           "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
                           "}")
                       ))
) 

output$summary <-renderUI({
  x = exprInput()
  if (is.null(x)) {
	return(NULL)
  }

  createAlert(session, "alert1", alertId = "Analysis-alert", title = "Please choose an analysis from the sidebar to continue...", style = "success",
               content = "Your selected dataset has been downloaded successfully, and is summarized below. <p>Please select either <b>Differential Expression Analysis</b> or <b>Survival Analysis</b> from the sidebar to continue.</p>", append = FALSE, dismiss = TRUE) 


  
  p.tag <-function(x) {
	for (i in 1:length(x)){ 
		x[i] = paste0("<p>",x[i], "</p>")	
	} 
        paste0(x, collapse = "") 
  }  

  gse = paste0("<b>", input$GSE, "/", input$platform,  
	" (", nrow(values.edit$table), " samples, ", nrow(x), " probes)</b>")

  
  msg = p.tag(gse)
	
  HTML(msg)


})

output$GeneColumn <- renderUI({
  gpl = paste0("Currently searching by probe / ", values.edit$platformGeneColumn)
  HTML(gpl)
})


#############################################
# dynamically change shinyTitle
#############################################
shinyTitle <-reactive({
  gse = isolate(input$GSE)
  platform = isolate(input$platform)
  if (is.null(gse) | gse == "") return("shinyGEO")
  paste0("shinyGEO - ", gse, "/", platform, sep = "")
})

output$shinyTitle = renderText(shinyTitle())

######################################################
# Hidden text boxes for conditional output
######################################################

# when platform info is availabe the other drop-down boxes are shown in the sidebar panel
sidebarDisplay <-reactive({
  if (is.null(dataInput())) return ("GSE-ONLY")
  if (is.null(platInfo())) return("PLATFORM")
  return("ALL")
})

output$sidebarDisplay <- renderText(sidebarDisplay())
outputOptions(output, 'sidebarDisplay', suspendWhenHidden=FALSE)


observe({
  shinycat("observing for selectGenes drop down...\n")
  options=  list(
      render = I(
        "{
            option: function(item, escape) {
                return '<div> <strong>' + item.genes + '</strong> - ' +  escape(item.probes) + '</div>';
            }
        }"
      )
    )


  label =  paste0("Select Probe (You May Search By  ", values.edit$platformGeneColumn, ")")

  updateSelectizeInput(session, "selectGenes", 
	label = label, server = TRUE, 
 	choices = geneNames(), options = options 
  )
})

observe({
 shinycat("update geneColumn selectizeInput...\n")
 updateSelectizeInput(session, "geneColumn", server = TRUE, 
	choices = colnames(platInfo()), selected = values.edit$platformGeneColumn) 
}) 

observeEvent(input$geneColumn, {
	if (is.null(input$geneColumn) | input$geneColumn == "") return(NULL)
	values.edit$platformGeneColumn = input$geneColumn
})


PlatformLinks <- reactive({
  pl = Platforms()
  if (is.null(pl)) return(NULL)
  pl = paste0("<a target = \"_blank\" href = \"http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=", 
         pl, "\">", pl, "</a>")
  pl = paste0("<p>",pl, "</p>")
  pl = paste0(pl, collapse = "")
  beg ="<p>Click on the links below for more information about the availalbe platforms:</p>"
  paste0(beg, pl)
})

output$PlatformLinks <-renderUI( {
  HTML(PlatformLinks())
})


observe ({
  ## only show plaforms for selected series ##
  pl = Platforms()
  shinycat("updating for platform = ", pl, "\n")
 
  pl.selected = NULL
  choices = NULL
  pl.options = NULL 
  if (!is.null(pl)) {
    keep = platforms.accession %in% pl
    pl.accession = platforms.accession[keep]
    pl.description = platforms.description[keep]
    if (length(pl.accession) == 1) {
      pl.selected = pl.accession 
      choices = pl.selected
    } else {
      pl.selected = NULL
      choices = data.frame(label = pl.accession, value = pl.accession, 
		name = pl.description)
      pl.options = list(
          render = I(
             "{
                option: function(item, escape) {
                     return '<div> <strong>' + item.label + '</strong> - ' +
                         escape(item.name) + '</div>';
                }
              }"
          )
       )
    }
  }
 
  updateSelectizeInput(session, inputId='platform', label = "Platform", server = TRUE,
               choices = choices,
               selected = pl.selected,
	       options = pl.options 
)

  if (!is.null(pl)) {
	 d = dataInput()
	 num.samples = sapply(d, function(x) length(sampleNames(x)))
        num.features = sapply(d, function(x) length(featureNames(x)))
        annot = sapply(d, annotation)
        x = paste("There are <b>", num.samples, "</b>samples and<b>", num.features, "</b>features on platform <b>", annot, "</b>")
#        x = paste("<br>", p, "</br>", collapse = "")

  	x = paste(x, collapse = "<br>")

	if (!TEST.DATA) {
    	createAlert(session, "alert1", alertId = "GPL-alert", 
		title = "Please select a platform to continue", 
		style = "success", content = x, append = TRUE, dismiss = FALSE) 
  	}
  }
})


###############################################################
# drop down options are in form of GSE number - description
# when a selection is made only the GSE number (label)
# is stored in the textbox. However, only the
# GSE number (label) can be searched.
# Ideally, we want to search both the number and description
# but only display the number when selected
# 'value' is what gets returned to server (GSE number)
###############################################################

updateSelectizeInput(session, inputId='GSE', label = "Accession Number", server = TRUE,
    choices =  data.frame(label = series.accession, value = series.accession, name = series.description),
    options = list(
      #create = TRUE, persist = FALSE,
      render = I(
      "{
          option: function(item, escape) {
      return '<div> <strong>' + item.label + '</strong> - ' +
      escape(item.name) + '</div>';
      }
      }"
    ))
)


################################################
### Renders drop-down menu for variables/columns 
################################################  
observe({

  val = NULL

  colNames = rownames(clinicalDataSummary()) 
  val = input$summaryModalTable_row_last_clicked
  val = colNames[val]
 
  output$selectedColumn <- renderUI({  
      # show possible choices (column names)
      selectInput('selectedColumn', 'Selected Column', 
            choices = ColumnNames(), #width='20%',
            selected = val, multiple = FALSE, selectize = FALSE
    )
  })

  val = input$selectedColumn
  output$selectedColumnForCombine <- renderUI({  
      # show possible choices (column names)
      selectInput('selectedColumnForCombine', 'Selected Column', 
            choices = ColumnNames(), #width='20%',
            selected = val, multiple = F, selectize = FALSE
    )
  })

})

output$test <- renderPrint(sessionInfo())

####################################################################
## renders drop-down menus (server-side) for clinical group selection
####################################################################
output$selectedGroups <- renderUI({
  selectInput('Group1Values','Select Groups for Comparison', 
              choices = groupsForSelectedColumn(), multiple=TRUE,
              selected = defaultGroupsForSelectedColumn(),
              width='100%',
              selectize = TRUE
              
  )
})


##############################################
# set output variables to display the table
##############################################
##############################
## Expression Profiles plot 
##############################

observe ({
  shinycat("Rendering profiles plot...\n")

  # Return max 30 exp. samples if there is alot of samples to make the determination easier = unclutterd graphics
  x = profiles()
  if (is.null(x)) {
	return(NULL)
  }
  n = ncol(x)
  if (n > 30) {
    s = sample(1:n, 30)
    x = x[,s]
  }
  
  # if more than 30 samples change the title to include " selected samples" since they are randomly selected, else " samples"
  if (n > 30) {
    title.detail = " selected samples"
  } else {
    title.detail = " samples"
  }
  
  y.label = "log2 expression"  

  title <- paste(isolate(input$GSE), '/', isolate(input$platform), title.detail, sep ='') # need 
 
  fixed.df <- as.data.frame(x=x, stringsAsFactors = FALSE)
  
  x1 <- reshape2::melt(fixed.df, na.rm = TRUE, id.vars = NULL, 
            variable.name = "variable", 
            value.name = "value")
  
  exp.prof.plot <- ggplot(x1, aes(variable, value)) + 
                geom_boxplot(outlier.colour = "green") +
                labs(title = title, y = y.label, x = "") + 
                theme(axis.text.x = element_text(angle = 90, hjust = 1))
  isolate(values.edit$profilesPlot <- TRUE) 
  output$exProfiles <- renderPlot({print(exp.prof.plot)})
})

observe({
  if (values.edit$profilesPlot) {
    closeAlert(session, "Expression-alert")
    values.edit$profilePlot = FALSE
  }
})

observe({
    
  PLOT = TRUE
      
  if (input$selectGenes == "") {
        PLOT = FALSE
  } else {
        closeAlert(session, "Gene-alert")
          if (length(input$Group1Values) == 0) {
            PLOT = FALSE
          }
  }
      
  if (!PLOT) {
        output$plot <-renderPlot({NULL})
  } else  {
        output$plot <- renderPlot({
              iv = input$selectedColumn
              m = match(as.character(iv), colnames(clinicalDataProcessed()))  
              clinical = as.character(clinicalDataProcessed()[,m]) 
              selected = c(as.character(input$Group1Values))
              k = clinical%in% selected
    
              y = clinical
              y[!k] = NA
            
              ## make sure levels are in selected order for plot
              y = factor(y)
	      x = probe.expr()

	      common = intersect(names(x), rownames(values.edit$table))
              m1 = match(common, names(x))
              m2 = match(common, rownames(values.edit$table))

	      x = x[m1]
              y = y[m2]

              main = paste(input$GSE, geneLabel() , sep = ": ")
              print(stripchart2(x,y, input$Group1Values, group.names = DE$labels,
		 main = main, col=DE$col))
              }) # end of renderPLot
    }
})  # end observe

