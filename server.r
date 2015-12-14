STD.ERR = FALSE 
if (STD.ERR) {
   cat <-function(...) {
     l = list(...)
     lapply(l, write, stderr())
   }
}


cat("begin source server.r\n")
source("settings.R")

library(DT)  ## tested on development version 0.1.32
library(shiny)
library(GEOquery)
library(Biobase)
library(reshape2) ## needs to be loaded before GGally
library(survival)
library(affy)
library(limma)
library(shinyBS)
library(GGally)
library(ggplot2)
library(shinyAce)
library(knitr)
library(rmarkdown)
library(RCurl)

source("stripchart2.R")
source("plot.shiny.km.R")

#options(shiny.deprecation.messages=FALSE)


shinyServer(function(input, output, session){
  
  source("server-reactives.R", local = TRUE)
  source("server-clinical.R", local = TRUE)
  source("server-merge.R", local = TRUE)
  source("server-io.R", local = TRUE)
  source("server-output.R", local = TRUE)
  source("formatDE.R", local = TRUE)

#  output$test <-renderText(paste0("hi there: ", input$tabs))

})
cat("end source server.r\n")
