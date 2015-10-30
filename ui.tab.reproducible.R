############################################################
# Reproducible Research
############################################################

tab.code =  tabItem("Code",
             h3("Editor"),
             aceEditor("rmd", mode="markdown", value='',readOnly=T, height="500px")
) 

tab.report = tabItem("Report",
        div(style = "display:inline-block; width:30%", 
            downloadButton('downloadData', 'Download Report')
        ),
             h3("Report"), 
             htmlOutput("knitDoc")
        
    )

#tab.reproducible = navbarMenu("Reproducible Research", icon = icon("book"),
#	tab.code, tab.report 
# 
#) # end of tab report panel

