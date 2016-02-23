#############################################################################
# Creates a 'database' of valid accession numbers
# download platforms from: http://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms
# and move platforms*.csv files into this directory
#############################################################################

#files = Sys.glob("platform*.csv")
#platforms = NULL
#for (f in files) {
#  tmp = read.table(f, header = TRUE, sep = ",")
#  platforms = rbind(platforms, tmp)
#}


platforms = read.table("platform.csv", header = TRUE, sep = ",")

g1 = grep("oligonucleotide", platforms$Technology)
g2 = grep("spotted DNA", platforms$Technology)
g = c(g1,g2)

platforms = platforms[g,]

platforms.accession = as.character(platforms$Accession)
platforms.description =  paste0(platforms$Title, " (", platforms$Data.Rows, " probes)")

o = order(platforms.accession)
platforms.accession = platforms.accession[o]
platforms.description = platforms.description[o]
save(platforms.accession, platforms.description, file = "platforms.RData")

