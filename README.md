# shinyGEO
shinyGEO is a web-based tool that allows a user to download the expression and sample data from a [Gene Expression Omnibus](http://www.ncbi.nlm.nih.gov/geo/browse/) dataset, select a gene of interest, and perform a survival or differential expression analysis using the available data. For both analyses, shinyGEO produces publication-ready graphics and generates R code ensuring that all analyses are reproducible. The tool is developed using shiny, a web application framework for R, a language for statistical computing and graphics.

## Official Website
http://bioinformatics.easternct.edu/shinyGEO/

## Running shinyGEO locally 
1. Download and install *R* from https://cran.r-project.org. (Note: *shinyGEO* requires *R*>=3.23)

2. Open *R*. Then type the following (you may copy and paste) into the *R* console in order to install the CRAN packages required by *shinyGEO*:

	`install.packages(c("shiny","survival","shinyBS","GGally","ggplot2","shinyAce","knitr","rmarkdown","RCurl","shinyjs","shinydashboard"))` 

        `install.packages('devtools')`
        `devtools::install_github('rstudio/DT', ref = "f3e86a6")`

3. Type the following to install the Bioconductor Packages required by *shinyGEO*:

	`source("http://bioconductor.org/biocLite.R")`		
	`biocLite(c("Biobase","GEOquery"))`

4. Type the following to run *shinyGEO*:

	`library(shiny)`	 
	`runUrl("https://github.com/gdancik/shinyGEO/archive/master.zip")`

## Contributors
- Main contributors: Jasmine Dumas, Michael Gargano, Garrett M. Dancik, PhD
- Additional contributors: Ken-Heng Liao, Gregory Harper, Brandon Spratt

## Acknowledgements
This work was supported, in part, by Google Summer of Code funding to JD in 2015. MG, KHL, GH, and BS contributed as part of an independent study in Computer Science / Bioinformatics while undergraduate students at Eastern Connecticut State University, Willimantic, CT,  USA.

