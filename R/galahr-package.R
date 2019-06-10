#' galahr: A GUI to assist learning about high dimensions in R
#'
#' A graphical interface to tourr using Shiny and
#'    plotly to explore high dimensional distributions through
#'    dynamic 2-d projections.
#'
#' @seealso
#'
#' Launch the app by calling [launchApp()]
#' @name galahr
#' @docType package
#'
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
globalVariables(c("curveNumber",
                  "tsfeatureData"))
