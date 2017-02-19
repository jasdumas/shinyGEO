[![Build Status](https://travis-ci.org/jasdumas/shinyGEO.svg?branch=master)](https://travis-ci.org/jasdumas/shinyGEO)

[![Coverage Status](https://img.shields.io/codecov/c/github/jasdumas/shinyGEO/master.svg)](https://codecov.io/github/jasdumas/shinyGEO?branch=master)

# shinyGEO
shinyGEO is a web-based tool that allows a user to download the expression and sample data from a [Gene Expression Omnibus](http://www.ncbi.nlm.nih.gov/geo/browse/) dataset, select a gene of interest, and perform a survival or differential expression analysis using the available data. For both analyses, shinyGEO produces publication-ready graphics and generates R code ensuring that all analyses are reproducible. The tool is developed using shiny, a web application framework for R, a language for statistical computing and graphics.

## Official Website
http://gdancik.github.io/shinyGEO/

## How to install this package?

Install the development version from Github:

```r
library(devtools)
install_github("jasdumas/shinyGEO")
library(shinyGEO)
```

## How to use this package?

Update the series and platform meta-data:

```r
shinyGEO::updateGEOdata()
```

Launch the shiny application locally:

```r
shinyGEO::runshinyGEO()
```

Or launch the shiny application in a web browser:

[http://bioinformatics.easternct.edu/shinyGEO/](http://bioinformatics.easternct.edu/shinyGEO/)

## Contributors
- Main contributors: Jasmine Dumas, Michael Gargano, Garrett M. Dancik, PhD
- Additional contributors: Ken-Heng Liao, Gregory Harper, Brandon Spratt

Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.

## Acknowledgements
This work was supported, in part, by [Google Summer of Code funding to JD in 2015](https://www.google-melange.com/archive/gsoc/2015/orgs/rproject/projects/jasdumas.html). MG, KHL, GH, and BS contributed as part of an independent study in Computer Science / Bioinformatics while undergraduate students at Eastern Connecticut State University, Willimantic, CT,  USA.

## Learn more
- Read the abstract about `shinyGEO` published in [*Bioinformatics*](https://academic.oup.com/bioinformatics/article-abstract/32/23/3679/2525634/shinyGEO-a-web-based-application-for-analyzing).

## Meta
- Please [report any issues or bugs](https://github.com/jasdumas/shinyGEO/issues).
- License: MIT
- Get citation information for `shinyGEO` in R doing `r citation(package = 'shinyGEO')`




