
library(shinyAce)
library(RCurl)
library(shinyBS)
#library("shinydashboard")
require('shinydashboard')

source("misc/html.R")
source("misc/bsModal.R")
source("ui/ui.dashboard.header.R")
source("ui/ui.dashboard.sidebar.R")
source("ui/ui.dashboard.body.R")

shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
)


