TRACE = TRUE

library(DT)
library(shiny)
library(GEOquery)
library(Biobase)
library(reshape2)
library(survival)
library(affy)
library(limma)
library(shinyBS)
library(GGally)
library(ggplot2)

source("stripchart2.R")
source("plot.shiny.km.R")
#options(shiny.deprecation.messages=FALSE)

shinyServer(function(input, output, session){
  
  source("server-reactives.R", local = TRUE)
  source("server-output.R", local = TRUE)
  
  observe({ 
      if (length(input$Group1Values) == 0) {
          output$selectGroupsMessage <-renderUI({HTML("<h1>Please Choose The Groups to Compare</h1>")})
          output$plot <-renderPlot({NULL})
      } else  {
          output$selectGroupsMessage <-renderText({""})
          output$plot <- renderPlot({
                        
              x = profiles()[selectedProbe(),] # effected by data transformation
              iv = input$selectedColumn
              m = match(as.character(iv), colnames(clinicalInput()))  # GD: change grep to match
              clinical = as.character(clinicalInput()[,m])  # clinicalInput() should be the new edited table once fixed
              selected = c(as.character(input$Group1Values))
              k = clinical%in% selected
    
              y = clinical
              y[!k] = NA
              stripchart2(x,y, col = NULL)
          })
    }
  })  # end observe
  
  observe({
    if (length(input$Group1Values) == 0) {  # same condition as DE for now
      output$SurvMessage <-renderUI({HTML("<h1>Please Choose The Columns for Survival Analysis</h1>")})
      output$kmSurvival <-renderPlot({NULL})
    } else  {
      output$SurvMessage <-renderText({""})
      output$kmSurvival <- renderPlot({
        a = 1:25
        boxplot(a) # placeholder!
        # plot.shiny.km(time = time(), death = outcome(), x = x())
        ## create options for title/labels/colors etc!
        
      })
    }
  }) # end of second observe
  
})