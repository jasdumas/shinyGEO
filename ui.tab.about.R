tab.about = tabPanel("About", icon = icon("info-circle"),
    h3("Directions on Usage"),
    #p("Gene Expression Omnibus (GEO) is a public functional genomics data repository supporting MIAME-compliant data submissions.
    #   Array- and sequence-based data are accepted. Tools are provided to help users query and download 
    #  experiments and curated gene expression profiles."),
    tags$ol(
        tags$li("Choose your GEO Accession Number (GSExxx) which is an original submitter-supplied record that summarizes a study from the ", 
            a("GEO Repository", href =" http://www.ncbi.nlm.nih.gov/geo/browse/?view=series")),
        tags$li("Select the Platform"),
        tags$li("View Expression Profiles and Determine Data Transfomation Method"),
        tags$li("View Clinical/Phenotypic Data and Select a Characteristic Column"), 
        tags$li("Select Gene and Probe"), 
        tags$li("Select Groups from the Selected Characteristic Column"), 
        tags$li("Format Differential Expression Analysis Plot"), 
        tags$li("Select Characteristic Column that contains info on 'Time' to Event"),
        tags$li("Select Characteristic Column that contains info on 'Outcome' at Event"),
        tags$li("Format 'Time' and 'Outcome' Columns"), 
        tags$li("View Survival Analysis Kaplan-Meier Plot"), 
        tags$li("Append Desired Plots to the Reproducible Report")
    ),
                    
    hr(),  
                       
    h3("Authors"),
    HTML("<span style = \"font-weight: bold\"> Jasmine Dumas </span>"),
    HTML("is a MSc. Graduate Student in the Predictive Analytics program at DePaul University (Chicago, IL, Online).
          Jasmine has a BSE in Biomedical Engineering from the University of Hartford
          and a Professional Certificate in Medical Product Develpment from UCI Extension."), 
          a("Github Profile", href ="https://github.com/jasdumas"), HTML("<span class= \"label label-primary\">Package Maintainer</span>"),
          br(), br(),
                       
    HTML("<span style = \"font-weight: bold\"> Garrett M. Dancik, PhD </span>"),
    HTML("is an Assistant Professor of Computer Science / Bioinformatics at Eastern
        Connecticut State University (Wilimantic, CT). His research involves applying statistical, mathematical,
        and computational models to answer biological questions and gain insight into biological systems.
        His more recent work has focused on analyzing gene expression data and identifying prognostic tumor biomarkers."),
        a("Lab Page.", href="http://bioinformatics.easternct.edu/"), HTML("<span class= \"label label-primary\">Package Maintainer</span>"),
        br(), br(),
                       
    HTML("<span style = \"font-weight: bold\"> Ken-Heng Henry Liao </span>"),
    HTML("has BS degrees in Computer Science and Mathematics from Eastern Connecticut State University (Wilimantic, CT). 
          He is currently pursuing a Masters in Bioinformatics at Boston University"), HTML("<span class= \"label label-info\">Contributor</span>"),
    br(), br(),
                       
    HTML("<span style = \"font-weight: bold\"> Branden A. Spratt </span>"),
    HTML("is currently pursing a B.S. in Computer Science at Eastern Connecticut State University."), HTML("<span class= \"label label-info\">Contributor</span>"),
    br(), br(),
                       
    HTML("<span style = \"font-weight: bold\"> Greg Harper </span>"),
    HTML("is currently studying Computer Science at Eastern Connecticut State University planning to graduate with a B.S. in May 2015. 
            Currently working as a Computer and Network security intern at Cigna until July 2015 at which point he will be starting full time. 
            He has also earned his Craftsman for working Electrical and Enviromental systems on aircraft, as well as having an A.S. in Computer 
            Technology from Manchester Community College."), 
    HTML("<span class= \"label label-info\">Contributor</span>")
)
 

