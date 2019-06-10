#' Split input data frame into numeric and factor columns.
#'
#' @param inData Input dataframe
#' @param rv Reactive value container.
#' @keywords internal
splitInput <- function(inData, rv){
  rv$numVars <- purrr::map_lgl(inData, is.numeric)
  rv$groupVars <- purrr::map_lgl(inData, is.character)
  rv$d <- inData[rv$numVars]
  rv$groups <- inData[rv$groupVars]
}

#' Read input data and update UI accordingly.
#'
#' @param file1 fileInput from shiny
#' @param rv Reactive value container
#' @param output shiny output container
#' @param session shiny session
#' @keywords internal
readInput <- function(file1, rv, output, session){
  paramFile <- file1$datapath
  splitInput(utils::read.csv(paramFile, stringsAsFactors = FALSE), rv)
  rv$npoint <- nrow(rv$d)
  nparam <- min(ncol(rv$d), 6)
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
                                  choices = names(rv$d),
                                  selected = names(rv$d[1:nparam]))
  if (sum(rv$groupVars)) {
    shiny::updateSelectInput(session,
                             "groupVar",
                             choices = names(rv$groups))}
  else {
    shiny::updateSelectInput(session,
                             "groupVar",
                             choices = c("None"))
  }
  shiny::updateNumericInput(session,
                            "sampleSize",
                            value = rv$npoint,
                            max = rv$npoint)
}
