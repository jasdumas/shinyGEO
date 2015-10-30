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
 theme = "ecsu.css", 
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
  title = uiOutput("shinyTitle"), titleWidth = 300, disable = FALSE 
)



gse.input = div(style = "display:inline-block; width: 85%",
            selectizeInput('GSE', label = "Accession Number", choices = NULL,width = 500,
              options = list(placeholder = "Please enter a GSE accession number",
                          maxOptions = 100)
            )
          )

gse.button = div(style = "display:inline-block; width: 11%",
                actionButton("submitButton", "Go!")
          )

gse.platform=  conditionalPanel(condition = "output.sidebarDisplay=='PLATFORM'|output.sidebarDisplay=='ALL'",

                  div(style = "display:inline-block; width: 85%",
                        selectizeInput('platform', label = NULL, choices = NULL, width = 500,
                                options = list(placeholder = "Please select a platform",
                                maxOptions = 10)
                        )
                  )

# Button was needed to trigger server-busy for please wait message based on server-busy
#                  div(style = "display:inline-block; width: 11%",
#                        actionButton("submitPlatform", "Go!")
#                  )
                )

sidebar = dashboardSidebar(width = 400,
  	includeCSS("www/ecsu.css"),
	gse.input, gse.button, gse.platform,
	conditionalPanel(condition = "output.sidebarDisplay=='ALL'",
	sidebarMenu(id = "tabs",
        	menuItem("Home", tabName = "Home", icon = icon("area-chart")),
		hr(),
		menuItem("Differential Expression Analysis", tabName = "DifferentialExpressionAnalysis", icon = icon("flask")),
		menuItem("Survival Analysis", tabName = "SurvivalAnalysis", icon = icon("life-ring")),
		hr(),
		menuItem("Full Data Table", tabName = "FullDataTable", icon = icon("table")),
		#menuItem("Clinical Data Summary", tabName = "ClinicalDataSummary", icon = icon("table")),
		hr(),
		menuItem("Code", tabName = "Code", icon = icon("code")),
		menuItem("Report", tabName = "Report", icon = icon("file-text")),
		hr(),
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
          		bsButton("ClinicalDataBtn","View Clinical Data", style="success")
        	) 
	),
        
            hr()
)

body = dashboardBody(
    bsAlert("alert1"),

  uiOutput("test"),
  uiOutput("busy"),
  summaryBSModal("summaryBSModal","Clinical Data Summary","manuBtn"),

  # please wait conditional panel

  ## originally shiny-busy
  #conditionalPanel(condition="$('submitButton').hasClass('shiny-show-progress')",
  conditionalPanel(condition="output.busy=='TRUE'",
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
      tab.data.full,
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


