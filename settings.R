##############################################################
# DEBUG SETTINGS
##############################################################

TEST.DATA = FALSE 
if (TEST.DATA) {
  shinycat("loading test data...\n")
  #load RData/GSE13.RData or RData/GSE19915.RData here
  load("RData/GSE19915.RData")
  #load("RData/GSE13.RData")
  #load("RData/GSE13507.RData")
}

