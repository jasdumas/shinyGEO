############################################################
# Reproducible Research
############################################################
tab.reproducible = navbarMenu("Reproducible Research", icon = icon("book"),
    tabPanel("Code", icon = icon("code"),
             
    aceEditor("myEditor", value = "", mode="r", theme="chrome",readOnly=T, height ="500px" )), 
    tabPanel("Report",  icon = icon("file-text"),
        div(style = "display:inline-block; width:30%", 
            downloadButton('downloadData', 'Download Report')
        ),
        fluidRow( 
            column(7, 
                HTML("<span style = \"color: blue; font-size: 22px\"> shinyGEO Report </span>"),
                htmlOutput("knitDoc") #,
            ),
            column(5, HTML("<span style = \"color: blue; font-size: 22px\"> Ace Editor </span>"),
                aceEditor("rmd", mode="markdown", value='',readOnly=T, height="500px")
            ) 
        ) # fluid row
    )
) # end of tab report panel

