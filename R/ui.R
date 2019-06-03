#' Generating layout for the input tab panel.
#'
#' @param params The input parameters for parameter selection checkbox input.
#' @return The complete shiny tabPanel.
#' @keywords internal
tabInput <- function(params){
  shiny::tabPanel("input", shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::fileInput(
          "file1",
          "Parameter values (CSV format)",
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv"
            )
          ),
        shiny::checkboxGroupInput(
          "parameters",
          label = "Choose parameters to display",
          choices = params,
          selected = params[1:min(length(params),6)]
          ),
        shiny::checkboxInput("rescale", "Rescale", value = TRUE),
        shiny::selectInput("tourType", "Select tour type",
                           choices = c("Grand tour", "Guided tour",
                                       "Planned tour", "Little tour", "Local tour"),
                           selected = "Grand tour"),
        shiny::conditionalPanel('input.tourType=="Guided tour"',
                                shiny::selectInput("tourIndex", "Select index function",
                                            choices = guidedTourOptions)),
        shiny::conditionalPanel('input.tourType=="Grand tour" || input.tourType=="Little tour"
                                || input.tourType=="Local tour"',
                                shiny::numericInput(
                                  "nPlanes",
                                  "Number of planes to generate:",
                                  10,
                                  min = 1,
                                  max = 1000
                                  )
                                ),
        shiny::conditionalPanel('input.tourType=="Planned tour"',
                                shiny::fileInput(
                                  "file2",
                                  "Anchor planes (RDS format)",
                                  accept = ".RDS"
                                  )
                                ),
        shiny::numericInput("angle", "Angular step size", 0.05, min = 0.01, max = 1),
        shiny::actionButton("updateTour", "Update results")
        ),
      shiny::column(
        width = 2,
        shiny::selectInput("displayType", "Select display type",
                           choices = c("groups", "density", "linked brushing"),
                           selected = "density"),
        shiny::conditionalPanel('input.displayType=="groups"',
                                shiny::selectInput("groupVar", "Grouping variable", choices = c("None")))
        ),
      shiny::column(width = 5,
                    shiny::textOutput("messages"))

)))
}

#' Generating layout for the results tab panel.
#'
#' @param npoint The number of points in the input data set.
#' @return The complete shiny tabPanel.
#' @keywords internal
tabResults <- function(npoint){
  shiny::tabPanel("results", shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::actionButton("play", "Play"),
        shiny::actionButton("save", "Save"),
        shiny::actionButton("print", "Print"),
        shiny::actionButton("saveAll", "Save anchor planes"),
        shiny::conditionalPanel('input.displayType=="density"',
                                shiny::numericInput(
                                  "alpha",
                                  label = "Select alpha",
                                  value = 1,
                                  min = 0.01,
                                  max = 1,
                                  step = 0.1
                                  )
                                ),
        shiny::conditionalPanel('input.displayType=="linked brushing"',
                                shiny::checkboxInput("selectedOnly",
                                                     label = "Show selected points only",
                                                     value = FALSE)
                                ),
        shiny::numericInput(
          "sampleSize",
          "Sample size (random selection)",
          value = npoint,
          min = 1,
          max = npoint
          ),
        shiny::conditionalPanel('input.displayType=="linked brushing"',
                                shiny::selectInput(
                                  "selectionType",
                                  "Update with selection as",
                                  choices = c("New only", "Both selections", "Overlapping set")
                                  )
                                ),
        htmltools::div(style = "display:inline-block",
                       plotly::plotlyOutput("axes", width = 200, height = 200)
                       ),
        htmltools::div(style = "display:inline-block",
                       plotly::plotlyOutput("coverageDisplay", width = 200, height = 200)
        ),
        shiny::conditionalPanel('input.displayType=="linked brushing"',
                                shiny::verbatimTextOutput("range")),
        shiny::verbatimTextOutput("projPrint")
        ),
      shiny::column(
        width = 7,
        htmltools::div(style = "display:inline-block",
                       plotly::plotlyOutput("tour", width = 750, height = 750)
                       ),
        htmltools::div(style = "display:inline-block",
                       plotly::plotlyOutput("ggtimeline", width = 750, height = 50)
        )
        ),
      shiny::column(
        width = 3,
        htmltools::div(style = "display:inline-block",
                       plotly::plotlyOutput("params", width = 300, height = 750)
                       )
        )
      )
    ))
}


#' Generating the shiny ui.
#'
#' @param params The input parameters for parameter selection checkbox input.
#' @param npoint The number of points in the input data set.
#' @return The complete shiny fluidPage UI.
#' @keywords internal
ui <- function(params, npoint){
  shiny::fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  shiny::navbarPage(
    "galahr",
    tabInput(params),
    tabResults(npoint)))
}
