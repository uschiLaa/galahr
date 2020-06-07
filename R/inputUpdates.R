#' Read input data and update UI accordingly.
#'
#' @param file1 fileInput from shiny
#' @param rv Reactive value container
#' @param output shiny output container
#' @param session shiny session
#' @keywords internal
readInput <- function(file1, rv, output, session){
  paramFile <- file1$datapath
  inData <- utils::read.csv(paramFile, stringsAsFactors = FALSE)
  rv$npoint <- nrow(paramDF)
  rv$numVars <- purrr::map_lgl(inData, is.numeric)
  rv$groupVars <- purrr::map_lgl(inData, is.character)
  rv$npoint <- nrow(inData)
  nparam <- min(length(rv$numVars), 6)
  rv$init <- FALSE
  output$messages <- shiny::renderText(
    shiny::validate(
      shiny::need(
        nparam > 2,
        "Error: Can only display tour for more than 2 parameters!"
        )
      )
    )
  shiny::updateCheckboxGroupInput(session,
                                  "parameters",
                                  choices = rv$numVars,
                                  selected = rv$numVars[1:nparam])
  if (sum(rv$groupVars)) {
    shiny::updateSelectInput(session,
                             "groupVar",
                             choices = names(rv$groups))}
  else {
    shiny::updateSelectInput(session,
                             "groupVar",
                             choices = c("None"))
  }
}
