#' Split input data frame into numeric and factor columns.
#'
#' @param inData Input dataframe
#' @param rv Reactive value container.
#' @keywords internal
splitInput <- function(inData, rv){
  rv$numVars <- sapply(inData, is.numeric)
  rv$groupVars <- sapply(inData, is.character)
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
  splitInput(read.csv(paramFile, stringsAsFactors = FALSE), rv)
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
  shiny::updateNumericInput(session,
                            "sampleSize",
                            value = rv$npoint,
                            max = rv$npoint)
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
