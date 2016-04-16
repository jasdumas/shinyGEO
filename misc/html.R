## html.R -- functions for generating html code 

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



