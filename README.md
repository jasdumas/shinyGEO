# shinyGEO
shinyGEO is a web-based tool that allows a user to download the expression and sample data from a [Gene Expression Omnibus](http://www.ncbi.nlm.nih.gov/geo/browse/) dataset, select a gene of interest, and perform a survival or differential expression analysis using the available data. For both analyses, shinyGEO produces publication-ready graphics and generates R code ensuring that all analyses are reproducible. The tool is developed using shiny, a web application framework for R, a language for statistical computing and graphics.

## Official Website
http://bioinformatics.easternct.edu/shinyGEO/

## Running shinyGEO locally 
1. Download R from https://cran.r-project.org

2. From within R, install 'shiny' by typing
`install.packages("shiny")` 

3. Type the following from within R:
  `runGitHub("shinyGEO", "gdancik")`

## Contributors
Main contributors: Jasmine Dumas, Michael Gargano, Garrett M. Dancik, PhD

Additional contributors: Ken-Heng Liao, Gregory Harper, Brandon Spratt

## Acknowledgements
This work was supported, in part, by Google Summer of Code funding to JD in 2015. MG, KHL, GH, and BS contributed as part of an independent study in Computer Science / Bioinformatics while undergraduate students at Eastern Connecticut State University, Willimantic, CT,  USA.

