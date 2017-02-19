#' Update series and platform data from GEO
#'
#' @return Series and platform metadata are updated from the Gene Expression
#' Omnibus and stored as R data objects to create a 'database' of
#' valid accession numbers.
#'
#' @examples \dontrun{ updateGEOdata() }
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom readr read_csv write_csv
#' @importFrom magrittr "%>%"
#' @export
updateGEOdata <- function(){

  #### series update ####
  series_count <- read_html('https://www.ncbi.nlm.nih.gov/geo/browse/')

  ## tests if the data is able to be downloaded
  if (is.list(series_count)) {
  series_count <- series_count  %>%
    html_nodes(xpath="//*[(@id = 'count')]") %>%
    html_text() %>%
    as.character()

  series_count <- as.numeric(gsub("series", "", series_count) )
  series_count <- as.integer(ceiling(series_count/5000.0))

  cat("Getting GEO Data...\n")
  cat("Using count = ", series_count, "\n")

  # this will hold the binded series meta data
  series <- data.frame()

  for(i in 1:series_count) {
    if(i != 1) {
      url <- paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=series&zsort=date&mode=csv&page=", i, "&display=5000")
      series_data <- read_csv(url)
      series <- rbind(series, series_data)
    }
  }
  # save raw data in external folder
  write_csv(series, "inst/extdata/series.csv")
  message("Finished update...raw data saved to -> inst/extdata/series.csv")

  #### process series data ####
  series <- read_csv("inst/extdata/series.csv")

  g <- grep("Expression profiling by array", series$`Series Type`)
  series <- series[g,]
  series.accession <- as.character(series$Accession)
  series.description <- as.character(series$Title)

  o <- order(series.accession)
  series.accession <- series.accession[o]
  series.description <- series.description[o]
  save(series.accession, series.description, file = "data/series.RData")
  } else {
    message("Please connect to the internet to download updated data")
  }

  #### platform update ####
  platform_count <- read_html("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms")

  if (is.list(platform_count)){
  platform_count <- platform_count  %>%
    html_nodes(xpath="//*[(@id = 'count')]") %>%
    html_text() %>%
    as.character()

  platform_count <- as.numeric(gsub("platforms", "", platform_count) )
  platform_count <- as.integer(ceiling(platform_count/5000.0))

  cat("Getting GEO Data...\n")
  cat("Using count = ", platform_count, "\n")

  # this will hold the binded series meta data
  platform <- data.frame()

  for(i in 1:platform_count) {
    if(i != 1) {
      url <- paste0("https://www.ncbi.nlm.nih.gov/geo/browse/?view=platforms&zsort=date&mode=csv&page=", i, "&display=5000")
      platform_data <- read_csv(url)
      platform <- rbind(platform, platform_data)
    }
  }
  # save raw data in external folder
  write_csv(platform, "inst/extdata/platform.csv")
  message("Finished update...raw data saved to -> inst/extdata/platform.csv")

  #### process platform data ####
  platforms <- read_csv("inst/extdata/platform.csv")

  g1 <- grep("oligonucleotide", platforms$Technology)
  g2 <- grep("spotted DNA", platforms$Technology)
  g <- c(g1,g2)

  platforms <- platforms[g,]

  platforms.accession <- as.character(platforms$Accession)
  platforms.description <-  paste0(platforms$Title, " (", platforms$`Data Rows`, " probes)")

  o <- order(platforms.accession)
  platforms.accession <- platforms.accession[o]
  platforms.description <- platforms.description[o]
  save(platforms.accession, platforms.description, file = "data/platforms.RData")

  #### Compress the data files ####
  tools::resaveRdaFiles("data/series.RData")
  tools::resaveRdaFiles("data/platforms.RData")
  } else {
    message("Please connect to the internet to download updated data")
  }

}

