cat("begin source ui.R\n")

library(shinyAce)
library(RCurl)
library(shinyBS)
library(shinydashboard)

source("ui.navbar.R")
source("ui.tab.expression.R")
source("ui.tab.analyses.R")
source("ui.tab.clinical.R")
source("ui.tab.reproducible.R")
source("ui.tab.about.R")
source("html.R")


if (0) {
#shinyUI(fluidPage(title = "GEO-AWS",
 shinyUI(bootstrapPage(    
  tags$style(type="text/css", "body {padding: 70px;}"),
       
   ############################################################
   # Navigation Bar
   ############################################################
   navbarPage(title = uiOutput("shinyTitle"), #title ="shinyGEO", 
              #id = "tabs", 
	      inverse = TRUE, position = "fixed-top",
              windowTitle = "shinyGEO", 
              collapsible = TRUE,
              header = navbar.header,
              tab.expression,
              tab.analyses,
              tab.clinical,
              tab.reproducible,
              tab.about
     )  # end NavBar Page
     
   ) # end fluidPage and shinyUI
)
}

header = dashboardHeader(
  title = uiOutput("shinyTitle"), titleWidth = 350, disable = FALSE 
)

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
                        selectizeInput('platform', label = NULL, choices = NULL, width = 275,
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
	gse.input, gse.button, gse.platform,
	conditionalPanel(condition = "output.sidebarDisplay=='ALL'",
	sidebarMenu(id = "tabs",
		hr(),
    menuItem("Home", tabName = "Home", icon = icon("home")),
		menuItem("Differential Expression Analysis", tabName = "DifferentialExpressionAnalysis", icon = icon("flask")),
		menuItem("Survival Analysis", tabName = "SurvivalAnalysis", icon = icon("life-ring")),
		menuItem("Full Data Table", tabName = "FullDataTable", icon = icon("table")),
		#menuItem("Clinical Data Summary", tabName = "ClinicalDataSummary", icon = icon("table")),
		menuItem("Code", tabName = "Code", icon = icon("code")),
		menuItem("Report", tabName = "Report", icon = icon("file-text")),
		menuItem("About", tabName = "About", icon = icon("info-circle"))
	     )
      )
)

analyses.common = conditionalPanel(condition = "input.tabs == 'DifferentialExpressionAnalysis' | input.tabs == 'SurvivalAnalysis'",

        div(style = "display:inline-block; width: 40%",
         	selectizeInput('selectGenes', "Select Gene/Probe", choices = NULL)
	),
 
       	div(style = "display:inline-block; width: 35%",
		conditionalPanel(condition = "input.tabs =='SurvivalAnalysis'",
          		bsButton("autoAnalysis","Select Time/Outcome", style="success"),
            		genBSModal("autogenModal","Survival Analyses","",size="large")
        	), 
		conditionalPanel(condition = "input.tabs =='DifferentialExpressionAnalysis' & input.selectedGenes!=''",
          		bsButton("ClinicalDataBtn","View Clinical Data", style="success") #,
          		#bsButton("ClinicalDataBtn2","View Clinical Data", style="success")
        	) 
	),
            hr()
)

body = dashboardBody(
  bsAlert("alert1"),
  uiOutput("test"),
  uiOutput("busy"),

  summaryBSModal("summaryBSModal","Clinical Data Summary (Large)","ClinicalDataBtn", size = "large",  

  tabsetPanel(
	tabPanel("Summary", DT::dataTableOutput("summaryModalTable")),
	tabPanel("Full Clinical Table",   
    actionButton("tabBut", "Edit Data Table"),
        DT::dataTableOutput("clinicalData"),
        shinyBS::bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
            uiOutput("dropModal"),
            textInput("find", label = "Find", value = ""),
            checkboxInput("checkbox", label = "Exact Match", value = FALSE),
            textInput("replace", label = "Replace", value = ""),
            checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
             actionButton("Enter", label = "Submit"))
	)
  )
 ),
 # empty modal 
 #bsModal("summary2", "Clinical Data Summary (small)", "ClinicalDataBtn2", size = "small", "Small Modal"),


  # please wait conditional panel

  ## originally shiny-busy
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
        div(style = "position:center; width:100%; height:100; text-align:center",
#            img(src="PleaseWait.gif", style = "width:50%")
		"Wait please..."
       )
    ),

   analyses.common, 

   tabItems(
      # First tab content
      tab.expression,
      tab.DE.analysis,
      tab.survival.analysis,
#      tab.data.full,
      tab.data.summary,
      tab.code,
      tab.report,
      tab.about
    )
)

shinyUI(

dashboardPage(
  header,
  sidebar,
  body
))


cat("end source ui.R\n")
