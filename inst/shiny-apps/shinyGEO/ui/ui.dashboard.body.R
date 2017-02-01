#####################################
# dashboard body 
#####################################

source("ui/ui.tab.expression.R")
source("ui/ui.tab.analyses.R")
source("ui/ui.tab.reproducible.R")
source("ui/ui.tab.about.R")
source("misc/html.R")


## fileUploadBar without label
fileUploadBar = fileInput('fileUpload', 'select file',
	                         accept=c('text/csv', 
	                                  'text/comma-separated-values,text/plain', 
	                                  '.csv'))
fileUploadBar[[3]][1] = NULL 

gse.input = div(style = "display:inline-block; width: 75%",
            selectizeInput('GSE', label = "Accession Number", choices = NULL, width = 275,
              options = list(placeholder = "Please enter a GSE #",
                          maxOptions = 100)
            )
          )

####################################
# DE and survival analyses
####################################
analyses.common = conditionalPanel(condition = "input.tabs == 'DifferentialExpressionAnalysis' | input.tabs == 'SurvivalAnalysis'",
        bsAlert("alert2"),
        div(style = "display:inline-block; width: 40%",
         	selectizeInput('selectGenes', "Select Gene/Probe", choices = NULL)
	),

    div(style = "display:inline-block; width: 25%",
    		a(id = "platLink", "Change Search Feature",
			#style="cursor:pointer; display:block; margin-bottom:5px;")
			style="cursor:pointer; display:block; position:relative; bottom:20px;")
    ),
       bsModal("platformModal", "Platform annotation", 
                       "platLink", size = "large",
			selectizeInput('geneColumn', 'Selected Feature', choices = NULL),	
                       DT::dataTableOutput("platformData")
        ), 

 
       	div(style = "display:inline-block; width: 35%",
		conditionalPanel(condition = "input.tabs =='SurvivalAnalysis'",
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
  summaryBSModal("summaryBSModal","Sample Data","ClinicalDataBtn", size = "large",  

    tabsetPanel(id = "tabClinicalData",
	tabPanel("Summary View", DT::dataTableOutput("summaryModalTable")),
	tabPanel("Standard View",   
        DT::dataTableOutput("clinicalData")
	),

	# sample selection
	tabPanel("Sample Selection",
  	      fluidRow(
		column(12, bsAlert("selectionAlert1"))
	      ),

	      fluidRow(
      		column(4, uiOutput("SampleSelectionCol1")),
      		column(4, uiOutput("SampleSelection1")),
		column(4, HTML("<br><button id='btnSelection' type='button' class='btn btn-info action-button'>Apply Selection Criteria</button>"))
    	      ),
    	      fluidRow(
      		column(4, uiOutput("SampleSelectionCol2")),
      	        column(4, uiOutput("SampleSelection2"))
    	      ),
	      fluidRow(
		 column(12, bsAlert("selectionAlert2"))
	      )
	),

	tabPanel("Data Export",
	      fluidRow(
	        column(12,
	               bsAlert("ioAlert1"),
	               bsAlert("ioAlert2")
	               )
	      ),

	      fluidRow(
	        column(4,
	               tags$h4(class="ioTitle","Download Dataset"),
	               hr(),
	               downloadButton("downloadSet","Download")
	               ),
	        column(4,
	               tags$h4(class="ioTitle","Upload Dataset"),
	               hr(),
	               fileUploadBar
	        ),
		column(4,
			tags$h4(class="ioTitle","Reset Clinical Data to Original"),
			hr(),
			HTML("<button id='ClinicalReset' type='button' class='btn btn-danger action-button'>Reset Clinical Data</button>")		
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


            	    HTML("<div class=\"progress\" style=\"height:25px !important\"><div class=\"progress-bar progress-bar-striped active\" role=\"progressbar\" aria-valuenow=\"40\" aria-valuemin=\"0\" aria-valuemax=\"100\" style=\"width:100%\">
        <span id=\"bar-text\">Loading...</span></div></div>") ),
  HTML("<link href='https://fonts.googleapis.com/css?family=Courgette' rel='stylesheet' type='text/css'>"),


   analyses.common, 

   tabItems(
      # First tab content
      tab.expression,
      tab.DE.analysis,
      tab.survival.analysis,
      tab.code,
      tab.about
    )
)


