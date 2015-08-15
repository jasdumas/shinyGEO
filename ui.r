library(shinyAce)
library(RCurl)
library(shinyBS)

source("html.R")

#shinyUI(fluidPage(title = "GEO-AWS",
 shinyUI(bootstrapPage(    

  tags$style(type="text/css", "body {padding: 70px;}"),
     
   #######################################################################
   ### ONLY DISPLAY REST AFTER PLATFORM IS SELECTED 
   #######################################################################
      
   conditionalPanel(condition = "input.tabs == 'Report'", 
                    tags$div(HTML("<hr style = \"background-color: red; height:4px\">")),
                    downloadButton('downloadData', 'Download Report')
   ), # end of report conditional panel
   
   ############################################################
   # Navigation Bar
   ############################################################
   navbarPage("GEO-AWS", id = "tabs", inverse = TRUE, position = "fixed-top",
              collapsible = TRUE,
              header = list(
                ############################################################
                # GSE selection
                ############################################################
                tags$style(type = "text/css",
                           ".well{color: gray; background-color: black}"
                ),
              
                div(style = "display:inline-block; width: 30%",
                    textInput("GSE", 
                              HTML("<span style = \"color:red;font-weight:bold\">GEO Accession Number </span>"), "")
                ),
                div(style = "display:inline-block; width: 30%",
                    actionButton("submitButton", "Submit")
                ),
                ## Hidden text box to indicate whether platform has been selected ##
                #conditionalPanel(condition = "input.GSE == 'HEY'",
                #                textOutput("displayPlatform")
                # ), 
                div(style = "display:inline-block; width: 30%",
                    uiOutput('platform')  
                ),
                
                uiOutput("test"),
                uiOutput("test2"),
                
                div(style = "position: relative; top: -20px", HTML("<hr style = \"background-color: black; height:3px;\">")),
                
                bsAlert("alert"),
                conditionalPanel(condition="$('html').hasClass('shiny-busy')",                   
                                 img(src="PleaseWait.gif", style = "width:50%")
                )
                
                
                
              ), # end navbarPage header 
              
              ############################################################
              # Expression Profiles
              ############################################################
              tabPanel("Expression Profiles", hr(), helpText("Determine if these samples are fair to compare by observing the graphical appearance of the stripchart and the boxplot values"), 
                       radioButtons("radio", label = "Apply log transformation to the data", 
                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3), 
                                    selected = 1), actionButton("exprAdd", "Append to Report"),
                       plotOutput("exProfiles") 
              ),
              ############################################################
              # Clinical Data
              ############################################################
              navbarMenu("Clinical Data", 
                         tabPanel("Clinical Data Summary", hr(), DT::dataTableOutput("clinicalDataSummary")),
                         tabPanel("Full Data Table", hr(), 
                                  actionButton("tabBut", "Edit Data Table"),
                                  DT::dataTableOutput("clinicalData"))
              ),
              
              ############################################################
              # Analyses
              ############################################################
              navbarMenu("Analyses",
                         conditionalPanel(condition=1, #"input.tabs == 'Differential Expression Analysis' | input.tabs == 'Survival Analysis'",                   
                                          #bsCollapse(id = "collapseGene", 
                                          #       bsCollapsePanel("Gene/Probe Selection",
                                          
                                          div(style = "display:inline-block; width:30%",
                                              uiOutput('selectGenes')
                                          ),
                                          div(style = "display:inline-block; width:30%",
                                              uiOutput('selectProbes')
                                          )
                                          
                         ),
                         tabPanel("Differential Expression Analysis",      
                                  uiOutput('selectedColumn'),
                                  uiOutput('selectedGroups'), 
                                  
                                  actionButton("formatDEButton", "Format Graph"),
                                  actionButton("DEadd", "Append to Report"),
                                  
                                  uiOutput("selectGroupsMessage"), 
                                  plotOutput("plot"),
                                  formatBSModal("Format", "Format", "formatDEButton", applyID = "applyFormatDE", size = "large",
                                                htmlOutput("formatDE")#, hr(),               
                                                #actionButton("applyFormatDE", "Apply Changes")
                                  )
                                  
                         ),
                         
                         tabPanel("Survival Analysis",
                                  bsCollapse(id = "SurvDataTable", open = "SurvDataTable",
                                      bsCollapsePanel("Select Time and Outcome From Clinical Data",
                                          div(style = "display:inline-block; width:30%",
                                                uiOutput('survTime')
                                          ),
                                          div(style = "display:inline-block; width:30%",
                                                uiOutput('survOutcome')
                                          ),
                                          actionButton("parseButton", "Format Columns"), 
                                            DT::dataTableOutput("clinicalDataForSurvival")
                                             )
                                  ),
                                  
                                  
                                  shinyBS::bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
                                                   uiOutput("dropModal"),
                                                   textInput("find", label = "Find", value = ""),
                                                   checkboxInput("checkbox", label = "Exact Match", value = FALSE),
                                                   textInput("replace", label = "Replace", value = ""),
                                                   checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
                                                   actionButton("Enter", label = "Submit")),
                                  
                                  uiOutput("SurvMessage"),
                                  actionButton("Survadd", "Append to Report"),
                                  tags$div(HTML("<hr style = \"background-color: red; height:4px\">")),   
                                  shinyBS::bsModal("parseModal", "Selected Survival Analysis Parameters", "parseButton", size = "large",
                                                           fluidRow(
                                                             column(4, textInput("survfind", label = "Find", value = "")),
                                                             column(4, textInput("survreplace", label = "Replace", value = "")),
                                                             column(4, actionButton("parseEnter", label = "Submit"), 
                                                                    actionButton("undo", label="Revert Changes"))
                                                           ),
                                                           DT::dataTableOutput("selectedCols")
                                                           #print("in parseModal window...")
                                  ),
                                  plotOutput("kmSurvival")
                         ) # end tabPanel: Survival Analysis
                        
                         
              ),
              navbarMenu("Reproducible Research", 
                         tabPanel("Code", 
                                  aceEditor("myEditor", value = "", mode="r", theme="chrome",readOnly=T )), 
                         tabPanel("Report",
                                  fluidRow( 
                                    column(2, 
                                           h4("Reproducible Report for GEO-AWS"), htmlOutput("knitDoc") #,
                                    ),
                                    column(10, 
                                           aceEditor("rmd", mode="markdown", value='',readOnly=T, height="400px")
                                    ) 
                                    
                                  ) # fluid row
                         )), # end of tab report panel
              
              tabPanel("About",
                       h3("Gene Expression Omnibus Analysis with Shiny (GEO-AWS)"), 
                       p("Gene Expression Omnibus (GEO) is a public functional genomics data repository supporting MIAME-compliant data submissions.
                         Array- and sequence-based data are accepted. Tools are provided to help users query and download 
                         experiments and curated gene expression profiles."),
                       p("1) Choose your GEO Series (GSExxx) number which is an original submitter-supplied record that summarizes a study.",
                         a("Repository Series Browser", href="http://www.ncbi.nlm.nih.gov/geo/browse/?view=series")),
                       p("2) If necessary, select the Platform you would like to analyze"),
                       p("3) Select the groups you would like to compare"),
                       hr(),  
                       
                       h3("Authors"),
                       HTML("<span style = \"font-weight: bold\"> Jasmine Dumas </span>"),
                       HTML("is a MSc. Graduate Student in the Predictive Analytics program at DePaul University.
                            Jasmine has a BSE in Biomedical Engineering from the University of Hartford
                            and a Professional Certificate in Medical Product Develpment from UCI Extension."), 
                       a("Github Profile", href ="https://github.com/jasdumas"), HTML("<span class= \"label label-primary\">Package Maintainer</span>"),
                       br(), br(),
                       
                       HTML("<span style = \"font-weight: bold\"> Garrett M. Dancik, PhD </span>"),
                       HTML("is an Assistant Professor of Computer Science & Bioinformatics at Eastern
                            Connecticut State University (Wilimantic, CT). His research involves applying statistical, mathematical,
                            and computational models to answer biological questions and gain insight into biological systems.
                            His more recent work has focused on analyzing gene expression data and identifying prognostic tumor biomarkers."),
                       a("Lab Page.", href="http://bioinformatics.easternct.edu/"), HTML("<span class= \"label label-primary\">Package Maintainer</span>"),
                       br(), br(),
                       
                       HTML("<span style = \"font-weight: bold\"> Ken-Heng Henry Liao </span>"),
                       HTML("is currently an undergrad student in Computer Science at Eastern Connecticut State University (Wilimantic, CT). He also has a BS in Mathematics. 
                            His previous and continuing study includes automatic summarization and its related information management. In assistance to Dr. Dancik's bioinformatic research, 
                            he hopes to help create a public tool that enable researchers to efficiently analyze genetic data; and at some point, a genious will cure cancer with this tool."), HTML("<span class= \"label label-info\">Contributor</span>"),
                       br(), br(),
                       
                       HTML("<span style = \"font-weight: bold\"> Branden A. Spratt </span>"),
                       HTML("is currently pursing a B.S. in Computer Science at Eastern Connecticut State University."), HTML("<span class= \"label label-info\">Contributor</span>"),
                       br(), br(),
                       
                       HTML("<span style = \"font-weight: bold\"> Greg Harper </span>"),
                       HTML("is currently studying Computer Science at Eastern Connecticut State University planning to graduate with a B.S. in May 2015. 
            Currently working as a Computer and Network security intern at Cigna until July 2015 at which point he will be starting full time. 
            He has also earned his Craftsman for working Electrical and Enviromental systems on aircraft, as well as having an A.S. in Computer 
            Technology from Manchester Community College."), HTML("<span class= \"label label-info\">Contributor</span>")
                       )
                       ), # end NavBar Page
   
   conditionalPanel(condition="$('html').hasClass('shiny-busy')",                    
                    tags$div(br(), h1("Processing, please wait...", align = "center"),id="loadmessage")
   )
  
  
   ) # end fluidPage and shinyUI
)