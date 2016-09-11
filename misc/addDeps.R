

### modify function from shinydashboard to use local adminLTE files
addDeps <- function(x) {
  if (getOption("shiny.minified", TRUE)) {
    adminLTE_js <- "app.min.js"
    adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
  } else {
    adminLTE_js <- "app.js"
    adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
  }

  dashboardDeps <- list(
    htmlDependency("AdminLTE", "2.0.6",
      "www/AdminLTE",
      script = adminLTE_js,
      stylesheet = adminLTE_css
    ),
    htmlDependency("shinydashboard",
      as.character(utils::packageVersion("shinydashboard")),
      c(file = system.file(package = "shinydashboard")),
      script = "shinydashboard.js",
      stylesheet = "shinydashboard.css"
    )
  )

  appendDependencies(x, dashboardDeps)
}



e=environment(getFromNamespace("addDeps", "shinydashboard"))
environment(addDeps) = e
assignInNamespace("addDeps", addDeps, "shinydashboard")

