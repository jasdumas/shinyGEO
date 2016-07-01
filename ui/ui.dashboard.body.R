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

header = dashboardHeader(
  title = uiOutput("shinyTitle"), titleWidth = 350, disable = FALSE 
)

# add id to sidebar toggle link so that we can refresh when clicked
tmp = header$children[[3]]$children[[2]]
tmp = gsub("\"#\"", "\"#\" id = \"sidebarToggle\"", tmp)
header$children[[3]]$children[[2]] = tmp

gse.input = div(style = "display:inline-block; width: 75%",
            selectizeInput('GSE', label = "Accession Number", choices = NULL, width = 275,
              options = list(placeholder = "Please enter a GSE #",
                          maxOptions = 100)
            )
          )

gse.button = div(style = "display:inline-block; width: 11%",
                actionButton("submitButton", "Go!")
          )

gse.platform=  conditionalPanel(condition = "output.sidebarDisplay=='PLATFORM'|output.sidebarDisplay=='ALL'",

                  div(style = "display:inline-block; width: 75%",
                        selectizeInput('platform', label = "Platform", choices = NULL, width = 275,
                                options = list(placeholder = "Please select a platform",
                                maxOptions = 10)
                        )
                  )

# Button was needed to trigger server-busy for please wait message based on server-busy
#                  div(style = "display:inline-block; width: 11%",
#                        actionButton("submitPlatform", "Go!")
#                  )
                )

sidebar = dashboardSidebar(width = 350,
  includeCSS('www/ecsu.css'),
  includeScript('www/ecsu.js'),
	gse.input, gse.button, gse.platform,
	conditionalPanel(condition = "output.sidebarDisplay=='ALL'",
	sidebarMenu(id = "tabs",
		hr(),
        menuItem("New Analysis", tabName = "NewAnalysis", icon = icon("refresh")), 
	hr(),
        menuItem("Home", tabName = "Home", icon = icon("home"), selected = TRUE),
        menuItem("Differential Expression Analysis", 
		tabName = "DifferentialExpressionAnalysis", icon = icon("flask")),
	menuItem("Survival Analysis", tabName = "SurvivalAnalysis", icon = icon("life-ring")),
	menuItem("View Sample Data Table", tabName = "FullDataTable", icon = icon("table")),
	menuItem("Code", tabName = "Code", icon = icon("code")),
	menuItem("About", tabName = "About", icon = icon("info-circle"))
	     )
      )
)


####################################
# DE and survival analyses
####################################
analyses.common = conditionalPanel(condition = "input.tabs == 'DifferentialExpressionAnalysis' | input.tabs == 'SurvivalAnalysis'",
        bsAlert("alert2"),
        div(style = "display:inline-block; width: 40%", id="sGenes",
         	selectizeInput('selectGenes', "Select Gene/Probe", choices = NULL)
	),

    div(style = "display:inline-block; width: 25%",
    		a(id = "platLink", "Change Search Feature",
			#style="cursor:pointer; display:block; margin-bottom:5px;")
			style="cursor:pointer; display:block; position:relative; bottom:5px;")
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


