# GEO-AWS
The Gene Expression Omnibus Analysis with Shiny (Project Name: GEO-AWS) is a web application to download gene expression datasets from the [GEO Repository](http://www.ncbi.nlm.nih.gov/geo/browse/) and perform differential expression and survival analyses. This project is currently in **development** as part of the [2015 Google Summer of Code program](http://www.google-melange.com/gsoc/homepage/google/gsoc2015).


## Installation
1. Fork a copy!

2. Clone to your desktop
3. if you don't have these packages already, put the following into your [RStudio](http://www.rstudio.com/) console `install.packages(c("DT", "shiny", "reshape2", "survival", "shinyBS", "GGally", "ggplot2", "shinyAce"))`
  4.  Bioconductor package are downloaded differently. Place the following into the console
  `source("http://bioconductor.org/biocLite.R")`                                                                       
    `biocLite(c("GEOquery", "Biobase", "affy", "limma"))`

5. In the console `library(shiny)`then `runApp("GEO-AWS")`


## Find out more
_Feel free to check out my project [wiki](https://github.com/jasdumas/GEO-AWS/wiki)_


## Contact
Jasmine Dumas (@jasdumas)


