##############################################################
# DEBUG SETTINGS
##############################################################

STD.ERR = TRUE

TEST.DATA = TRUE 
if (TEST.DATA) {
  ## load RData/GSE13.RData or RData/GSE19915.RData here
  #load("RData/GSE19915.RData")
  load("RData/GSE13.RData")
}

TRACE = TRUE

EXPRESSION.PLOT =  TRUE 
DE.PLOT = TRUE 
AUTOSELECT.SURVIVAL = TRUE


cat("in settings.R")
##############################################################


##############################################################
# DEBUG FUNCTIONS 
##############################################################

TABS = 0

add.tab <-function() TABS <<- TABS + 1
subtract.tab <-function() TABS <<- TABS - 1

######################################################################
## tabbed output, appends \t's to the start of each cat, based on 
## value of global variable TABS
######################################################################
cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL, 
          append = FALSE) 
{
  if (is.character(file)) 
    if (file == "") 
      file <- stdout()
  else if (substring(file, 1L, 1L) == "|") {
    file <- pipe(substring(file, 2L), "w")
    on.exit(close(file))
  }
  else {
    file <- file(file, ifelse(append, "a", "w"))
    on.exit(close(file))
  }  
   l = list(...)
  tabs = NULL
  if (TABS >0) {
    for (i in 1:TABS) {
       tabs = paste0(tabs, "\t")
     }
    }
   l[[1]] = paste0(tabs, l[[1]])
  .Internal(cat(l, file, sep, fill, labels, append))
}


