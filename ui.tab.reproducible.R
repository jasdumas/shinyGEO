############################################################
# Reproducible Research
############################################################
tab.reproducible = navbarMenu("Reproducible Research", icon = icon("book"),
    tabPanel("Code", icon = icon("code"),
             h3("Editor"),
             aceEditor("rmd", mode="markdown", value='',readOnly=T, height="500px")),         
    #aceEditor("myEditor", value = "", mode="r", theme="chrome",readOnly=T, height ="500px" )), 
    tabPanel("Report",  icon = icon("file-text"),
        div(style = "display:inline-block; width:30%", 
            downloadButton('downloadData', 'Download Report')
        ),
             h3("Report"), 
             htmlOutput("knitDoc")
        
    )
) # end of tab report panel

