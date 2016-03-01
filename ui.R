
library(shinyAce)
library(RCurl)
library(shinyBS)
library(shinydashboard)

source("html.R")
source("bsModal.R")
source("ui.dashboard.header.R")
source("ui.dashboard.sidebar.R")
source("ui.dashboard.body.R")

shinyUI(
  dashboardPage(
    header,
    sidebar,
    body
  )
)


