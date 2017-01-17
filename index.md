# shinyGEO
shinyGEO is a web-based tool that allows a user to download the expression and sample data from a [Gene Expression Omnibus](http://www.ncbi.nlm.nih.gov/geo/browse/) dataset, select a gene of interest, and perform a survival or differential expression analysis using the available data. For both analyses, shinyGEO produces publication-ready graphics and generates R code ensuring that all analyses are reproducible. The tool is developed using shiny, a web application framework for R, a language for statistical computing and graphics.

## Webpages

Primary Application Hosting at Eastern Connecticut State University: http://bioinformatics.easternct.edu/shinyGEO/

Primary Homepage: http://gdancik.github.io/shinyGEO/


***


Shinyapps.io (Mirror site): https://jasminedumas.shinyapps.io/shinyGEO

Shinyapps.io (Mirror site): https://gdancik.shinyapps.io/shinyGEO/

GitHub Repo Page: http://github.com/jasdumas/shinyGEO/

Github Homepage: http://jasdumas.github.io/shinyGEO/

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
Main contributors: Jasmine Dumas, Michael Gargano, Garrett M. Dancik, PhD

Additional contributors: Ken-Heng Liao, Gregory Harper, Brandon Spratt

## Acknowledgements
This work was supported, in part, by [Google Summer of Code](http://www.google-melange.com/gsoc/homepage/google/gsoc2015) funding to JD in 2015. MG, KHL, GH, and BS contributed as part of an independent study in Computer Science / Bioinformatics while undergraduate students at Eastern Connecticut State University, Willimantic, CT,  USA.

## Find out more
Feel free to check out the project [wiki](https://github.com/jasdumas/shinyGEO/wiki) creating during Google Summer of Code.

## Updates
25 AUGUST 2015: The current (post-GSoC version) development version as of 21 August 2015 19:00 UTC to the present can be found [here](https://github.com/jasdumas/shinyGEO) as per the program code submission guidelines and the submitted GSoC release can be found [here](https://github.com/jasdumas/shinyGEO/releases/tag/v0.1).

11 SEPTEMBER 2015: This GitHub Repo and corresponding links have be updated to a new name to conform with CRAN naming conventions and for deployment guidelines

2 OCTOBER 2015: The application has been deployed to shinyapps.io cloud service at https://jasminedumas.shinyapps.io/shinyGEO Report bugs, glitches, and general comments as this is the first release designated for testing and configuration.

7 JANUARY 2016: The application will also be deployed on the Eastern CT State University Shiny Server

15 APRIL 2016: The abstract for shinyGEO was accepted to two conferences: [AACR Annual Meeting 2016 April 16 - 20, 2016 Ernest N. Morial Convention Center New Orleans, Louisiana, USA](http://www.abstractsonline.com/Plan/ViewAbstract.aspx?mID=4017&sKey=b710c4a6-fafb-4546-a4ef-94ef72d93639&cKey=0243e952-bd00-4008-84b0-53222a594ee9&mKey=1d10d749-4b6a-4ab3-bcd4-f80fb1922267), and [The R User (useR 2016) Conference June 27 - June 30 2016 Stanford University, Stanford, California](http://user2016.org/)

...

## Contact
Jasmine Dumas [@jasdumas](https://twitter.com/jasdumas) / [@shiny_GEO](https://twitter.com/shiny_geo)

