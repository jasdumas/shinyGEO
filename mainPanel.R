library(shinyBS) # needs to be here also!

mainPanel(

  tags$style(type = "text/css",
             ".well{color: white; background-color: black}"
  ),
  
 
#tags$style(type = "text/css",
#".navbar, .navbar li a:link {color: white; background-color: blue}"
#),

## this effects drop down items
#tags$style(type = "text/css",
#".navbar li a:hover {background-color: red;}"
#),

## this effects direct navbar items
#tags$style(type = "text/css",
#".navbar li {background-color:red}"
#),


 conditionalPanel(condition="$('html').hasClass('shiny-busy')",                   
                   img(src="PleaseWait.gif")
                 ),

 conditionalPanel(condition="!$('html').hasClass('shiny-busy')", 

    conditionalPanel(condition = "output.displayPlatform == 'FALSE'",                   
      tabPanel("Overview",
           h1("Gene Expression Omnibus Analysis with Shiny (GEO-AWS)"), 
           p("Gene Expression Omnibus (GEO) is a public functional genomics data repository supporting MIAME-compliant data submissions.
                            Array- and sequence-based data are accepted. Tools are provided to help users query and download 
                           experiments and curated gene expression profiles."),
           p("1) Choose your GEO Series (GSExxx) number which is an original submitter-supplied record that summarizes a study.",
             a("Repository Series Browser", href="http://www.ncbi.nlm.nih.gov/geo/browse/?view=series")),
           p("2) If necessary, select the Platform you would like to analyze"),
           p("3) Select the groups you would like to compare"),
           hr()  
    )
  ),
  
  conditionalPanel(condition = "output.displayPlatform == 'TRUE'",
#      tabsetPanel(type = "pills", id = "tabs",
	navbarPage("GEO-AWS", id = "tabs", inverse = TRUE,
	      
          tabPanel("Expression Profiles", hr(), helpText("Determine if these samples are fair to compare: "), 
                   radioButtons("radio", label = "Apply log transformation to the data", 
                                                    choices = list("Auto-Detect" = 1, "Yes" = 2, "No" = 3), 
                                                    selected = 1), plotOutput("exProfiles")),
	
	navbarMenu("Clinical Data", 

          tabPanel("Clinical Data Summary", hr(), DT::dataTableOutput("clinicalDataSummary")),
          tabPanel("Full Data Table", hr(), 
                   actionButton("tabBut", "Edit Data Table"),
          bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
                  uiOutput("dropModal"),
                  textInput("find", label = "Find", value = ""),
                  checkboxInput("checkbox", label = "Exact Match", value = FALSE),
                  textInput("replace", label = "Replace", value = ""),
                  checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
                  actionButton("Enter", label = "Submit")),
                  DT::dataTableOutput("clinicalData"))
         ),

	navbarMenu("Analyses",
          tabPanel("Differential Expression Analysis", 
                   hr(), 
                   div(style = "display:inline-block; width:30%",
                      uiOutput('selectGenes')
                   ),
                   div(style = "display:inline-block; width:30%",
                     uiOutput('selectProbes')
                   ),

                   uiOutput("selectGroupsMessage"), 
                   plotOutput("plot")),
          tabPanel("Survival Analysis", 
                   hr(),
                   div(style = "display:inline-block; width:30%",
                       uiOutput('selectGenesSurv')
                   ),
                   div(style = "display:inline-block; width:30%",
                       uiOutput('selectProbesSurv')
                   ),
                   
                  plotOutput("kmSurvival"))
        ),
          tabPanel("About",
            h3("Authors"),
            HTML("<span style = \"font-weight: bold\"> Jasmine Dumas </span>"),
            HTML("is a MSc. Graduate Student in the Predictive Analytics program at DePaul University.
                  Jasmine has a BSE in Biomedical Engineering from the University of Hartford
                  and a Professional Certificate in Medical Product Develpment from UCI Extension.
                  She is currently a R&D Engineering Technician at Medtronic."), 
            a("Github Profile", href ="https://github.com/jasdumas"), 
            br(), br(),
            
            HTML("<span style = \"font-weight: bold\"> Garrett M. Dancik, PhD </span>"),
            HTML("is an Assistant Professor of Computer Science & Bioinformatics at Eastern
                  Connecticut State University (Wilimantic, CT). His research involves applying statistical, mathematical,
                  and computational models to answer biological questions and gain insight into biological systems.
                  His more recent work has focused on analyzing gene expression data and identifying prognostic tumor biomarkers."),
            a("Lab Page.", href="http://bioinformatics.easternct.edu/"),
            br(), br(),
                                      
            HTML("<span style = \"font-weight: bold\"> Ken-Heng Henry Liao </span>"),
            HTML("is currently an undergrad student in Computer Science at Eastern Connecticut State University (Wilimantic, CT). He also has a BS in Mathematics. 
                His previous and continuing study includes automatic summarization and its related information management. In assistance to Dr. Dancik's bioinformatic research, 
                he hopes to help create a public tool that enable researchers to efficiently analyze genetic data; and at some point, a genious will cure cancer with this tool."),
            br(), br(),
            
            HTML("<span style = \"font-weight: bold\"> Branden A. Spratt </span>"),
            HTML("is currently pursing a B.S. in Computer Science at Eastern Connecticut State University."),
            br(), br(),
                
            HTML("<span style = \"font-weight: bold\"> Greg Harper </span>"),
            HTML("is currently studying Computer Science at Eastern Connecticut State University planning to graduate with a B.S. in May 2015. 
            Currently working as a Computer and Network security intern at Cigna until July 2015 at which point he will be starting full time. 
            He has also earned his Craftsman for working Electrical and Enviromental systems on aircraft, as well as having an A.S. in Computer 
            Technology from Manchester Community College.")
          )
  )
 ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",                    
                   tags$div(br(), h1("Processing, please wait...", align = "center"),id="loadmessage")
  )
)
) 
