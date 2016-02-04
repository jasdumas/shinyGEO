
#####################################################################
## Download testdata using first platform for given GSE
#####################################################################

GSE = "GSE13507"
SAVE.FILE = paste0(GSE, ".RData")


library(GEOquery)
GEO.test = getGEO(GSE)

GPL = sapply(GEO.test, annotation)[1]

CLINICAL.test = pData(GEO.test[[1]])

GPL.test = Table(getGEO(GPL))

save(CLINICAL.test, GEO.test, GPL.test, file = SAVE.FILE)


