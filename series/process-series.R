#############################################################################
# Creates a 'database' of valid accession numbers
# download series from: http://www.ncbi.nlm.nih.gov/geo/browse/?view=series
# and move series*.csv files into this directory
#############################################################################

files = Sys.glob("series*.csv")
series = NULL
for (f in files) {
  tmp = read.table(f, header = TRUE, sep = ",")
  series = rbind(series, tmp)
}

g = grep("Expression profiling by array", series$Series.Type)
series = series[g,]
series.gse = as.character(series$Accession)
series.name = as.character(series$Title)

o = order(series.gse)
series.gse = series.gse[o]
series.name = series.name[o]
save(series.gse, series.name, file = "series.RData")

