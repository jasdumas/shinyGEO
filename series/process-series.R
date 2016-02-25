#############################################################################
# Creates a 'database' of valid accession numbers
# download series from: http://www.ncbi.nlm.nih.gov/geo/browse/?view=series
# and move series*.csv files into this directory
#############################################################################

#files = Sys.glob("series*.csv")
#series = NULL
#for (f in files) {
#  tmp = read.table(f, header = TRUE, sep = ",")
#  series = rbind(series, tmp)
#}

series = read.table("series.csv", header = TRUE, sep = ",")

g = grep("Expression profiling by array", series$Series.Type)
series = series[g,]
series.accession = as.character(series$Accession)
series.description = as.character(series$Title)

o = order(series.accession)
series.accession = series.accession[o]
series.description = series.description[o]
save(series.accession, series.description, file = "series.RData")

