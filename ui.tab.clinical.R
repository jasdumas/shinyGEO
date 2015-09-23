############################################################
# Clinical Data
############################################################
tab.clinical =  navbarMenu("Clinical Data", icon = icon("table"), 
    tabPanel("Clinical Data Summary", DT::dataTableOutput("clinicalDataSummary"), icon = icon("plus-square-o")),
    tabPanel("Full Data Table",  icon = icon("plus-square"),
        actionButton("tabBut", "Edit Data Table"),
        DT::dataTableOutput("clinicalData"), 
        shinyBS::bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
            uiOutput("dropModal"),
            textInput("find", label = "Find", value = ""),
            checkboxInput("checkbox", label = "Exact Match", value = FALSE),
            textInput("replace", label = "Replace", value = ""),
            checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
             actionButton("Enter", label = "Submit")))
)
