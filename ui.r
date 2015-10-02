library(shinyAce)
library(RCurl)
library(shinyBS)

source("ui.navbar.R")
source("ui.tab.expression.R")
source("ui.tab.analyses.R")
source("ui.tab.clinical.R")
source("ui.tab.reproducible.R")
source("ui.tab.about.R")
source("html.R")

#shinyUI(fluidPage(title = "GEO-AWS",
 shinyUI(bootstrapPage(    
 theme = "ecsu.css", 
  tags$style(type="text/css", "body {padding: 70px;}"),
       
   ############################################################
   # Navigation Bar
   ############################################################
   navbarPage(title ="shinyGEO", 
              id = "tabs", inverse = TRUE, position = "fixed-top",
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
