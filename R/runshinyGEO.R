#' Run shinyGEO application
#'
#' @return Nothing is returned except a shiny app launched locally showing
#' shinyGEO.
#' @examples \dontrun{ runshinyGEO() }
#' @export
runshinyGEO <- function() {
  appDir <- system.file("shiny-apps", "shinyGEO", package = "shinyGEO")
  if (appDir == "") {
    stop("Could not find apps directory. Try re-installing `shinyGEO`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
