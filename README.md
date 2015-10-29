# shinyGEO
shinyGEO (Project Name: GEO-AWS / Gene Expression Omnibus Analysis with Shiny) is a web application to parse, transform, model, and visualize microarray gene expression data sets from the [Gene Expression Omnibus Repository](http://www.ncbi.nlm.nih.gov/geo/browse/) and performs differential expression and survival analyses. This project was formally developed in part of the [2015 Google Summer of Code program](http://www.google-melange.com/gsoc/homepage/google/gsoc2015). The current (post-GSoC version) development version as of 21 August 2015 19:00 UTC to the present can be found [here](https://github.com/jasdumas/GEO-AWS) as per the program code submission guidlines.

## Website
Shinyapps.io: https://jasminedumas.shinyapps.io/shinyGEO

GitHub Page: http://jasdumas.github.io/shinyGEO/

## Installation
1. Fork a copy!

2. Clone to your desktop

3. if you don't have these packages already, put the following into your [RStudio](http://www.rstudio.com/) console: 
`install.packages(c("shiny", "reshape2", "survival", "shinyBS", "GGally", "ggplot2", "shinyAce", "knitr"))`

4.  Bioconductor package are downloaded differently. Place the following into the console:
  `source("http://bioconductor.org/biocLite.R")`                                                                       
    `biocLite(c("GEOquery", "Biobase", "affy", "limma"))`
  
5. This project depends on some development versions of pacakge(s): 
`devtools::install_github('rstudio/DT')`

6. In the console `library(shiny)` then `runApp("shinyGEO")`

7. Feel free to test out the tool functionality by entering the GEO Accession number: [GSE19915](http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE19915) which includes survival characterstics to generate Kaplan-Meier curves under the Analyses tab menu.

8. `library(devtools)`

  `devtools::install_github("jasdumas/shinyGEO")`

  `library(shinyGEO)`

  `runGitHub("shinyGEO", "jasdumas")`
  
9. `runUrl('https://github.com/jasdumas/shinyGEO/archive/master.zip')`

## Find out more
_Feel free to check out my project [wiki](https://github.com/jasdumas/GEO-AWS/wiki)_

## Updates
* 11 SEPTEMBER 2015: This GitHub Repo and corresponding links have be updated to a 
new name to conform with CRAN naming conventions and for deployment guidelines

* 2 OCTOBER 2015: The application has been deployed to shinyapps.io cloud service at https://jasminedumas.shinyapps.io/shinyGEO Report bugs, glitches, and general comments as this is the first release designated for testing and configuration. 

## Contribute
This project is in beta/demonstration only mode and is scheduled to be launched in early Fall 2015. A considerable amount of the features are functional but if you discover a bug or non-working application features with certain data sets, please feel free to submit an [issue](https://github.com/jasdumas/GEO-AWS/issues) or contact the pacakge maintainer.

## Contact
Jasmine Dumas [@jasdumas](https://twitter.com/jasdumas)

## Status
[![Build Status](https://travis-ci.org/jasdumas/GEO-AWS.svg)](https://travis-ci.org/jasdumas/GEO-AWS)


