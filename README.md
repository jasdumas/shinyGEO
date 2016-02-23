# shinyGEO

Analyzing gene expression and correlating phenotypic data is an important method to discover insights about disease outcomes and prognosis. There are currently several web-based tools designed to address these analyses but are limited in usability, data pipeline access, and reproducibility. We describe an R Shiny web application, shinyGEO that can download gene expression data sets directly from the Gene Expression Omnibus, and perform differential expression and survival analysis across selected genes and probes.

shinyGEO (Project Name: GEO-AWS / Gene Expression Omnibus Analysis with Shiny) is a web application to parse, transform, model, and visualize microarray gene expression data sets from the [Gene Expression Omnibus Repository](http://www.ncbi.nlm.nih.gov/geo/browse/) and performs differential expression and survival analyses. This project was formally developed in part of the [2015 Google Summer of Code program](http://www.google-melange.com/gsoc/homepage/google/gsoc2015). The current (post-GSoC version) development version as of 21 August 2015 19:00 UTC to the present can be found [here](https://github.com/jasdumas/GEO-AWS) as per the program code submission guidlines.

## Website

http://bioinformatics.easternct.edu/shinyGEO/

## Running shinyGEO locally 
1. Download R, a langauge for statistical computing and grpahics, from https://cran.r-project.org

2. From within R, install 'shiny' by typing
`install.packages("shiny")` 

3. Type the following from within R:
  `runGitHub("shinyGEO", "gdancik")`


## Contributors
Main contributors: Jasmine Dumas, Michael Gargano, Garrett M. Dancik, PhD
Additional contributors: Ken-Heng Liao, Gregory Harper, Brandon Spratt

## Acknowledgements
This work was supported, in part, by Google Summer of Code funding to JD in 2015. MG, KHL, GH, and BS contributed as part of an independent study in Computer Science / Bioinformatics while undergraduate students at Eastern Connecticut State University, Willimantic, CT,  USA.

