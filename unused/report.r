
#####################################
# removed from ui.tab.reproducible.R
#####################################
tab.report = tabItem("Report",
        div(style = "display:inline-block; width:30%", 
            downloadButton('downloadData', 'Download Report')
        ),
             h3("Report"), 
             htmlOutput("knitDoc")
        
    )

#####################################
# removed from server-output.R
#####################################

###################
# Knitr Report
###################
output$knitDoc <- renderPlot(
  #input$exprAdd
  #input$DEadd
  #input$Survadd
  #return(isolate(HTML(knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))))
  #cat("knitDoc\n")
  print(input$exProfiles)
  )  
   
# a reactive to supply the content function with the text from the aceEditor
knit.report <- reactive({
  knit2html(text = input$rmd, quiet = TRUE)
})
### Download knitr report ###
output$downloadData <- downloadHandler(
  filename = function() { 
    paste("report", "html", sep=".") 
  },
  
  content = function(file) {
    #input$knitDoc ## trial to connect knitr with download button
    
    src <- normalizePath('report.Rmd')
    
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    file.copy(src, 'report.Rmd')
  
    library(rmarkdown)
    #out <- render('report.Rmd', output_format = html_document())
    #a <- knit(input$rmd)
    out <- render(input$rmd, output_format = html_document())
    
    file.rename(out, file)
  
  }
)


