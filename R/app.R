#' Launch Shiny App
#'
#' @param omicsFixID
#'
#' @export
#'
app <- function(name = "apps", ...) {
  appDir <- system.file(name, package = "omicsFixID")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir, ...)
}
