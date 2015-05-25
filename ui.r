
library(RCurl)

shinyUI(fluidPage(title = "GEO-AWS",
    sidebarLayout(
          # These files are kept within the GEO-AWS project folder
          source("sidebarPanel.R", local = TRUE)[1], 
          source("mainPanel.R", local = TRUE)[1], 
          
    )
)) # end fluidPage and shinyUI
