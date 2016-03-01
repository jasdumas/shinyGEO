
TRACE = FALSE 
shinycat <-function(...) {
	if (TRACE) cat(...)
}
shinyprint <-function(...) print(...)

shinycat("begin source server.R\n")
source("settings.R")

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

source("stripchart2.R")
source("plot.shiny.km.R")

shinyServer(function(input, output, session){

  source("server-reactives.R", local = TRUE)
  source("server-clinical.R", local = TRUE)
  source("server-merge.R", local = TRUE)
  source("server-io.R", local = TRUE)
  source("server-output.R", local = TRUE)
  source("server-survival.R", local = TRUE)
  source("server-report.R", local = TRUE)
  source("formatDE.R", local = TRUE)
#  output$test <-renderText(paste0("hi there: ", input$tabs))

})
shinycat("end source server.R\n")
