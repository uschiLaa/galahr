#' Shiny app for exploring high dimensional data
#'
#' Opening the GUI with the specified dataset.
#' If no input data is specified the app will launch
#' using the [tsfeatureData] example data.
#'
#' @param paramDF input data to load the app with
#' @export
#' @examples \dontrun{
#' galahr()
#' galahr(paramDF = GW170817)
#' }
galahr <- function(paramDF = NULL) {
  if (is.null(paramDF)) {
    paramDF <- tsfeatureData
  }

  params <- names(paramDF)[purrr::map_lgl(paramDF, is.numeric)]

  server <- function(input, output, session) {

    rv <- initializeReactive(paramDF)

    shiny::observeEvent(input$file1, {
      if (is.null(input$file1)) {
        return()
      }
      readInput(input$file1, rv, output, session)
    })

    shiny::observeEvent(c(input$updateTour, rv$update),
                        {
                          #get data, list of projections and set reactive values
                          output$messages <- shiny::renderText(shiny::validate(
                            shiny::need(
                              length(input$parameters) > 2,
                              "Error: Can only display tour for more than 2 parameters!"
                            )
                          ))
                          if (length(input$parameters) < 3) {
                            return()
                          }

                          if (input$rescale){
                            rv$dataMatrix <-
                              tourr::rescale(as.matrix(rv$d[input$parameters]))
                          }
                          else {
                            rv$dataMatrix <-
                              as.matrix(rv$d[input$parameters])
                          }
                          if (sum(rv$groupVars)) {
                            shiny::updateSelectInput(session,
                                                     "groupVar",
                                                     choices = c("None", names(rv$groups)))}
                          else {
                            shiny::updateSelectInput(session,
                                                     "groupVar",
                                                     choices = c("None"))
                          }

                          rv$tourPlanes <-
                              tourr::save_history(rv$dataMatrix,
                                                  tourr::grand_tour(),
                                                  max_bases = input$nPlanes,
                                                  rescale = FALSE)
                          rv$pathIndex <- NULL
                          fullTour <- tourr::interpolate(
                            rv$tourPlanes, angle = input$angle
                          )
                          rv$anchors <- which(attributes(fullTour)$new_basis)
                          rv$fullTour <- as.list(fullTour)
                          rv$tmax <- length(rv$fullTour)
                          rv$t <- 1
                          rv$timelineAxis <- pretty(c(1, rv$tmax))
                          output$ggtimeline <-
                            plotly::renderPlotly(
                              ggtimeline(
                                rv$anchors, 1, rv$tmax, rv$timelineAxis, rv$pathIndex
                              )
                            )
                          # get points on hypercube
                          rv$cubePoints <-
                            cubePoints(length(input$parameters), rv$dataMatrix)
                          # calculate projected data and cube points
                          # for first projection
                          updateReactiveData(rv)
                          #hover text should contain all function and parameter values
                          hoverTextDf <- hoverText(rv$d, input$parameters)
                          rv$halfRange <-
                            compute_half_range(NULL, rv$dataMatrix, TRUE) * 1.3
                          plotlyTour <-
                              plotlyTourF(rv$cdata, rv$cubeLine,
                                          hoverTextDf, rv$halfRange)
                          output$tour <- plotly::renderPlotly(plotlyTour)
                          # final step: draw axes display
                          xVec <- rv$fullTour[[1]][, 1]
                          yVec <- rv$fullTour[[1]][, 2]
                          plotlyAxes <-
                            plotlyAxesF(xVec, yVec, input$parameters)
                          output$axes <- plotly::renderPlotly(plotlyAxes)
                          rv$init <- TRUE
                          output$messages <- shiny::renderText("Results updated with current selection")
                        })

    shiny::observeEvent(input$play, {
      rv$on <- !rv$on
      if(rv$t==rv$tmax){rv$t <- 1}
    })

    shiny::observeEvent(plotly::event_data("plotly_click", source = "TL"), {
      d <- plotly::event_data("plotly_click", source = "TL")
      rv$t <- d$x
      updatePlots(rv, session, input, output)
    })

    shiny::observeEvent(input$groupVar, {
      updateGroups(rv, session, input)
      # function that changes point colors and adds legend as annotation
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$alpha, {
      updateAlpha(session, input)
      # function that changes alpha
    }, ignoreInit = TRUE)

    shiny::observe({
      if (!rv$on) {
        return()
      }
      if (rv$stop) {
        shiny::isolate({
          rv$stop <- FALSE
          rv$on <- FALSE
          return()})
      }
      shiny::isolate({
        updatePlots(rv, session, input, output)
        # keeping track of projection index
        # reset to 1 when reaching final projection
        if (rv$t == rv$tmax) {rv$stop <- TRUE}
        else{rv$t <- rv$t + 1}
      })
      shiny::invalidateLater(125)

    })

  }

  shiny::shinyApp(ui(params, c("None")), server)
}
