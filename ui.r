# Show/hide column feature on data tables
library(RCurl)

shinyUI(fluidPage(title = "GEO-AWS",
    sidebarLayout(
          source("sidebarPanel.R", local = TRUE)[1], 
          source("mainPanel.R", local = TRUE)[1]
    )
)) # end fluidPage and shinyUI
