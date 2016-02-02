#####################################
# dashboard body 
#####################################

source("ui.tab.expression.R")
source("ui.tab.analyses.R")
source("ui.tab.reproducible.R")
source("ui.tab.about.R")

####################################
# DE and survival analyses
####################################
analyses.common = conditionalPanel(condition = "input.tabs == 'DifferentialExpressionAnalysis' | input.tabs == 'SurvivalAnalysis'",
        bsAlert("alert2"),
        div(style = "display:inline-block; width: 40%",
         	selectizeInput('selectGenes', "Select Gene/Probe", choices = NULL)
	),

    div(style = "display:inline-block; width: 25%",
    		a(id = "platLink", "Change Search Parameter",
			style="cursor:pointer")
    ),
       bsModal("platformModal", "Platform annotation", 
                       "platLink", size = "large",
			selectizeInput('geneColumn', 'Selected Feature', choices = NULL),	
                       DT::dataTableOutput("platformData")
        ), 

 
       	div(style = "display:inline-block; width: 35%",
		conditionalPanel(condition = "input.tabs =='SurvivalAnalysis'",
          		bsButton("autoAnalysis","Select Time/Outcome", style="success",disabled = TRUE),
            		genBSModal("autogenModal","Survival Analyses","",size="large")
        	)
	)
)

body = dashboardBody(
  conditionalPanel(condition = "input.tabs != 'About' & input.tabs != 'Code'",
                   bsAlert("alert1"),
                   uiOutput("busy")
                   ),


  shinyjs::useShinyjs(),
  summaryBSModal("summaryBSModal","Clinical Data","ClinicalDataBtn", size = "large",  

    tabsetPanel(
	tabPanel("Summary", DT::dataTableOutput("summaryModalTable")),
	tabPanel("Full Clinical Table",   
        DT::dataTableOutput("clinicalData")
	),

	tabPanel("Data I/O",
	      fluidRow(
	        column(12,
	               bsAlert("ioAlert1"),
	               bsAlert("ioAlert2"),
	               bsAlert("ioAlert3")
	               )
	      ),


	      fluidRow(
	        column(5,
	               tags$h4(class="ioTitle","Download Dataset"),
	               hr(),
	               downloadButton("downloadSet","Download")
	               
	              
	               ),
	        column(2,
	              tags$p("")
	               ),
	        column(5,
	               tags$h4(class="ioTitle","Upload Dataset"),
	               hr(),
	               fileInput('fileUpload', '',
	                         accept=c('text/csv', 
	                                  'text/comma-separated-values,text/plain', 
	                                  '.csv'))
	               
	        )
	      )
	   )    
   )
 ),

  # please wait conditional panel

  ## originally shiny-busy
  conditionalPanel(
	condition="$('html').hasClass('shiny-busy') & input.tabs == 'Home'",

#        div(style = "position:center; width:100%; height:100; text-align:center",
#            img(src="PleaseWait.gif", style = "width:50%")
#		"Please wait..."
 #      )

	div(style = "position:fixed; bottom: 40%; right: 10%;
		    border: 3px solid; text-align: center; background-color: white; z-index:100;",
            	    img(src="PleaseWait.gif", style = "width:50%")
        )
    ),


   analyses.common, 

   tabItems(
      # First tab content
      tabItem("sessionInfo", verbatimTextOutput("test")),
      tab.expression,
      tab.DE.analysis,
      tab.survival.analysis,
      tab.code,
      tab.about
    )
)


