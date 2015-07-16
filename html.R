## html.R -- functions for generating html code 

##########################################################################
## formatBSModal - equivalent to shinyBS::bsModal except we add a Save 
## Changes button with id = applyID next to the Close button
## shinyBSDep is not found, so last line is commented out. This does
## not appear to have an effect
##########################################################################
formatBSModal<-function (id, title, trigger, applyID, ..., size) 
{
  if (!missing(size)) {
    if (size == "large") {
      size = "modal-lg"
    }
    else if (size == "small") {
      size = "modal-sm"
    }
    size <- paste("modal-dialog", size)
  }
  else {
    size <- "modal-dialog"
  }
  bsTag <- shiny::tags$div(class = "modal sbs-modal fade", 
      id = id, tabindex = "-1", `data-sbs-trigger` = trigger, 
      shiny::tags$div(class = size, 
      shiny::tags$div(class = "modal-content", 
      shiny::tags$div(class = "modal-header", 
            shiny::tags$button(type = "button",  class = "close", `data-dismiss` = "modal", shiny::tags$span(shiny::HTML("&times;"))), 
            shiny::tags$h4(class = "modal-title", title)), 
      shiny::tags$div(class = "modal-body", list(...)), 
      shiny::tags$div(class = "modal-footer", 
        shiny::tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Close"),
        actionButton(applyID, "Save Changes", class = "btn-primary")    
      )      
      )))
  #htmltools::attachDependencies(bsTag, shinyBSDep)
}



###################################################################
## cell_ and row_html - creates HTML table rows
## modified from http://stackoverflow.com/questions/15512510/in-rstudio-shiny-wishing-to-customize-a-table
###################################################################
cell_html <- function(table_cell, header = FALSE) {
  if (!header) return(paste0('<td>', table_cell, '</td>') ) 
  return(paste0('<th>', table_cell, '</th>'))
}
row_html <- function(table_row, header = FALSE) {
  cells <- sapply(table_row, cell_html, header = header)
  collapse_cells <- paste0(cells, collapse='')
  paste0('<tr>', collapse_cells, '</tr>')
}


