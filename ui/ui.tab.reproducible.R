############################################################
# Reproducible Research
############################################################

tab.code =  tabItem("Code",
             h3("R Code"),
 	     actionButton("reportBtn", "Generate Report"),p(),
    	     bsModalNoClose("reportModal", "Report Generation", "reportBtn", size = "large",
		bsAlert("reportAlert")
    	     ),
             aceEditor("rmd", mode="markdown", value='',readOnly=T, height="500px")
) 
