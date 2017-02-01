#' Update series and platform data from GEO
#'
#' @return Series and platform data are updated from the Gene Expression
#' Omnibus and stored as R data objects.
#'
#' @examples \dontrun{ update_GEOdata() }
#' @export
update_GEOdata <- function(){
  # platform update
  system("inst/shiny-apps/shinyGEO/platforms/geo-platform-update.sh")
  source("inst/shiny-apps/shinyGEO/platforms/process-platforms.R")
  cat("Platforms data updated ...\n")

  # series update
  system("inst/shiny-apps/shinyGEO/series/geo-series-update.sh")
  source("inst/shiny-apps/shinyGEO/series/process-series.R")
  cat("Series data updated")
}
