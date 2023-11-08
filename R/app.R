#' Launch "omicsFixID" Shiny App
#'
#'OmicsFixID provides two key functions.
#'(1)FixID: provides statistics on the percentage of secondary identifiers in the dataset and converts outdated secondary identifiers to current primary identifiers, if available. The FixID functionality currently covers secondary identifiers from HGNC , HMDB , ChEBI and Wikidata which can be converted to the corresponding primary identifier from the initial database. After this step, the CrossMapID can be used to convert the primary-ID-enhanced dataset to any other database currently supported by BridgeDb. (2) CrossMapID: uses BridgeDb's REST-API to convert identifiers.
#'
#' @export
#'
omicsFixID_app <- function(...) {
  appDir <- system.file("apps", package = "omicsFixID")
  if (appDir == "") stop("The shiny app does not exist")
  shiny::runApp(appDir, ...)
}
