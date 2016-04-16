TRACE = FALSE 
shinycat <-function(...) {
	if (TRACE) cat(...)
}
shinyprint <-function(...) print(...)

shinycat("begin source server.R\n")
source("server/settings.R")

library(DT)  ## tested on development version 0.1.32
library(shiny)
library(GEOquery)
library(Biobase)
library(reshape2) ## needs to be loaded before GGally
library(survival)
library(shinyBS)
library(GGally)
library(ggplot2)
library(shinyAce)
library(knitr)
library(rmarkdown)
library(RCurl)
library(shinyjs)
library(shinydashboard)

source("misc/stripchart2.R")
source("misc/plot.shiny.km.R")

shinyServer(function(input, output, session){
  source("server/server-reactives.R", local = TRUE)
  source("server/server-clinical.R", local = TRUE)
  source("server/server-merge.R", local = TRUE)
  source("server/server-io.R", local = TRUE)
  source("server/server-report.R", local = TRUE)
  source("server/server-selection.R", local = TRUE)
  source("server/server-output.R", local = TRUE)
  source("server/server-survival.R", local = TRUE)
  source("server/formatDE.R", local = TRUE)
})
