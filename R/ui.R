#' Generating the shiny ui.
#'
#' @param params The input parameters for parameter selection checkbox input.
#' @return The complete shiny fluidPage UI.
#' @keywords internal
ui <- function(params, grps){
  shiny::fluidPage(
  theme = shinythemes::shinytheme("simplex"),
  shiny::fluidRow(
      shiny::column(
        width = 2,
        shiny::br(),
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
          selected = params[1:min(length(params),6)],
          inline = TRUE
        ),
        shiny::checkboxInput("rescale", "Rescale", value = TRUE),
        shiny::selectInput("tourType", "Select tour type",
                           choices = c("Grand tour", "Guided tour",
                                       "Planned tour", "Little tour",
                                       "Local tour"),
                           selected = "Grand tour"),
        shiny::conditionalPanel('input.tourType=="Guided tour"',
                                shiny::selectInput("tourIndex",
                                                   "Select index function",
                                                   choices = guidedTourOptions)),
        shiny::conditionalPanel('input.tourType=="Grand tour"
                                || input.tourType=="Little tour"
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
        shiny::numericInput("angle", "Angular step size",
                            0.05, min = 0.01, max = 1),
        shiny::actionButton("updateTour", "Update results")
    ),
  shiny::column(
    width = 7,
    htmltools::div(style = "display:inline-block",
                   plotly::plotlyOutput("tour", width = 700, height = 700)
    ),
    htmltools::div(style = "display:inline-block",
                   plotly::plotlyOutput("ggtimeline",
                                        width = 700,
                                        height = 50)
    )
  ),
  shiny::column(
    width = 2,
    shiny::br(),
    shiny::fluidRow(shiny::actionButton("play", "Play / Pause")),
    shiny::br(),
    shiny::actionButton("save", "Save"),
    shiny::actionButton("print", "Print"),
    shiny::actionButton("saveAll", "Save anchor planes"),
    shiny::selectInput("displayType", "Select display type",
                       choices = c("groups", "density"),
                       selected = "density"),
    shiny::conditionalPanel('input.displayType=="groups"',
                            shiny::selectInput(
                              "groupVar",
                              "Grouping variable",
                              choices = grps
                            )
    ),
    shiny::numericInput(
      "alpha", label = "Select alpha",
      value = 1, min = 0.01, max = 1, step = 0.1
    ),
    htmltools::div(style = "display:inline-block",
                   plotly::plotlyOutput("axes", width = 300, height = 300)
    ),
    shiny::verbatimTextOutput("projPrint")
  )))
}
