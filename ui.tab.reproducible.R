############################################################
# Reproducible Research
############################################################

tab.code =  tabItem("Code",
             h3("R Code"),
             aceEditor("rmd", mode="markdown", value='',readOnly=T, height="500px")
) 
