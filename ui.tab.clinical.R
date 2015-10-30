############################################################
# Clinical Data
############################################################



tab.data.full = tabItem("FullDataTable", h3("Clinical Data"), icon = icon("plus-square"),
        actionButton("tabBut", "Edit Data Table"),
        DT::dataTableOutput("clinicalData"), 
        shinyBS::bsModal("modalExample", "Edit Data Table", "tabBut", size = "small",
            uiOutput("dropModal"),
            textInput("find", label = "Find", value = ""),
            checkboxInput("checkbox", label = "Exact Match", value = FALSE),
            textInput("replace", label = "Replace", value = ""),
            checkboxInput("survCheckbox", label = "Partial Replace", value = FALSE),  ### for survival analysis
             actionButton("Enter", label = "Submit"))
)


tab.data.summary = tabItem("ClinicalDataSummary", h3("Clinical Data Summary"), DT::dataTableOutput("clinicalDataSummary"), icon = icon("plus-square-o"))


#tab.clinical =  navbarMenu("Clinical Data", icon = icon("table"), tab.data.summary, tab.data.full) 

