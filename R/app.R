#' Shiny app for exploring high dimensional data
#'
#' If no input data is specified the app will launch
#' using the [tsfeatureData] example data.
#'
#' @param paramDF input data to load the app with
#' @export
#' @examples \dontrun{
#' launchApp()
#' launchApp(paramDF = GW170817)
#' }
launchApp <- function(paramDF = NULL) {
  if (is.null(paramDF)) {
    paramDF <- tsfeatureData
  }

  params <- names(paramDF)[purrr::map_lgl(paramDF, is.numeric)]
  npoint <- nrow(paramDF)


  server <- function(input, output, session) {

    rv <- initializeReactive(paramDF)

    shiny::isolate({
      if (sum(rv$groupVars)) {
        shiny::updateSelectInput(session,
                               "groupVar",
                               choices = names(rv$groups))}
      else {
        shiny::updateSelectInput(session,
                                 "groupVar",
                                 choices = c("None"))
      }})

    shiny::observeEvent(input$file1, {
      if (is.null(input$file1)) {
        return()
      }
      readInput(input$file1, rv, output, session)
    })

    shiny::observeEvent(c(input$displayType,input$groupVar), {
      if ((input$displayType != "groups") | (input$groupVar=="None")){
        shiny::updateSelectInput(
          session, "tourIndex", choices = guidedTourOptions
        )
        }
      else {
        guidedTourOptionsNew <- c(guidedTourOptions,"lda_pp", "pda_pp")
        shiny::updateSelectInput(
          session, "tourIndex", choices = guidedTourOptionsNew
          )
      }
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

                   if(input$tourType == "Grand tour"){
                     rv$tourPlanes <-
                       tourr::save_history(rv$dataMatrix,
                                           tourr::grand_tour(),
                                           max_bases = input$nPlanes,
                                           rescale = FALSE)
                     rv$pathIndex <- NULL
                   }
                   else if(input$tourType == "Little tour"){
                     rv$tourPlanes <-
                       tourr::save_history(rv$dataMatrix,
                                           tourr::little_tour(),
                                           max_bases = input$nPlanes,
                                           rescale = FALSE)
                     rv$pathIndex <- NULL
                   }
                   else if(input$tourType == "Guided tour"){
                     if (input$tourIndex %in% groupedIndex){
                       grId <- as.factor(rv$groups[[input$groupVar]])
                     }
                     else grId <- NA
                     guidedTour <- getGuidedTour(input$tourIndex, grId)
                     rv$tourPlanes <-
                       tourr::save_history(
                         rv$dataMatrix, guidedTour, rescale = FALSE
                         )
                   }
                   else if(input$tourType == "Planned tour"){
                     pathFile <- input$file2$datapath
                     rv$tourPlanes <- readRDS(pathFile)
                     # input could be either history array
                     # in which case we don't need to do anything
                     # or could be a list of matrices defining the planes
                     # in which case we need to plan tour path
                     if(is.null(attr(rv$tourPlanes, "class"))){
                       # since planned tour skips first two entries
                       # we add random ones
                       ndim <- nrow(rv$tourPlanes[[1]])
                       r1 <- tourr::basis_random(ndim)
                       r2 <- tourr::basis_random(ndim)
                       rv$tourPlanes <- append(list(r1, r2), rv$tourPlanes)
                       rv$tourPlanes <- tourr::save_history(
                         rv$dataMatrix,
                         tourr::planned_tour(rv$tourPlanes),
                         rescale = FALSE
                         )
                   }
                   }
                   else if(input$tourType == "Local tour"){
                     start <- rv$fullTour[[rv$t]]
                     if (nrow(start) != length(input$parameters)){
                       start <- tourr::basis_init(length(input$parameters), 2)
                     }
                     rv$tourPlanes <-
                       tourr::save_history(rv$dataMatrix,
                                           tourr::local_tour(start),
                                           max_bases = input$nPlanes,
                                           rescale = FALSE)
                   }
                   fullTour <- tourr::interpolate(
                     rv$tourPlanes, angle = input$angle
                     )
                   if(input$tourType == "Guided tour"){
                     #FIXME this is not currently displayed
                     #fix display or remove computation
                     rv$pathIndex <- getPathIndex(
                       fullTour, input$tourIndex, grId
                       )
                   }
                   rv$anchors <- which(attributes(fullTour)$new_basis)
                   rv$fullTour <- as.list(fullTour)
                   rv$tourPCA <-
                     fullTourPCA(rv$fullTour, length(input$parameters))
                   output$coverageDisplay <-
                     plotly::renderPlotly(
                       coveragePlot(rv$tourPCA, length(input$parameters), 1)
                       )
                   rv$selection <- rv$dataMatrix
                   rv$resetSelection <- rv$dataMatrix
                   rv$resetSample <- FALSE
                   rv$tmax <- length(rv$fullTour)
                   rv$t <- 1
                   rv$timelineAxis <- pretty(c(1, rv$tmax))
                   output$ggtimeline <-
                     plotly::renderPlotly(
                       ggtimeline(
                         rv$anchors, 1, rv$tmax, rv$timelineAxis, rv$pathIndex
                         )
                       )
                   # for use when selecting samples, define separate data frames
                   rv$inSample <- rv$d
                   rv$outOfSample <-rv$d[0,]
                   # get points on hypercube
                   rv$cubePoints <-
                     cubePoints(length(input$parameters), rv$dataMatrix)
                   # 1-d parameter values
                   pPlots <-
                     plotly1d(rv$d, rv$outOfSample)
                   rv$h1d <- min(0.05, 1/length(pPlots))
                   output$params <- plotly::renderPlotly({
                     plotly::subplot(
                       pPlots,
                       nrows = length(pPlots),
                       heights = rep(rv$h1d, length(pPlots)),
                       margin = 0.03
                     ) %>%
                       plotly::layout(title = "Parameter values")
                   })
                   # calculate projected data and cube points
                   # for first projection
                   updateReactiveData(rv)
                   #hover text should contain all function and parameter values
                   hoverTextDf <- hoverText(rv$d, input$parameters)
                   rv$halfRange <-
                     compute_half_range(NULL, rv$dataMatrix, TRUE) * 1.3
                   # now can draw tour display
                   # different function used when drawing grouped data
                   # (mapping group to color)
                   if (
                     (input$displayType == "groups") &&
                     (input$groupVar !=" None")
                     ){
                     plotlyTour <-
                       plotlyTourGrouped(rv$cdata, rv$cubeLine,
                                           hoverTextDf, rv$halfRange,
                                           rv$groups[[input$groupVar]]
                                           )
                   }
                   else{
                     plotlyTour <-
                       plotlyTourF(rv$cdata, rv$cubeLine,
                                   hoverTextDf, rv$halfRange)
                   }
                   output$tour <- plotly::renderPlotly({
                     plotlyTour
                   })
                   # final step: draw axes display
                   xVec <- rv$fullTour[[1]][, 1]
                   yVec <- rv$fullTour[[1]][, 2]
                   plotlyAxes <-
                     plotlyAxesF(xVec, yVec, input$parameters)
                   output$axes <- plotly::renderPlotly(plotlyAxes)
                   rv$init <- TRUE
                 })

    shiny::observeEvent(input$play, {
      rv$on <- !rv$on
      if(rv$t==rv$tmax){rv$t <- 1}
    })

    shiny::observeEvent(input$alpha, {
      markerCol <- plotly::toRGB("black", input$alpha)
      plotly::plotlyProxy("tour", session) %>%
        plotly::plotlyProxyInvoke(
          "restyle", marker.color = list(markerCol), list(1)
          )
    })

    shiny::observeEvent(input$save, {
      if (is.null(rv$fullTour)) {
        return()
      }
      pMat <- rv$fullTour[[rv$t]]
      saveRDS(pMat, file = paste0("tour_projection_", rv$t, ".rds"))
    })

    shiny::observeEvent(input$saveAll, {
      if(is.null(rv$tourPlanes)){return()}
      anchorPlanes <- rv$tourPlanes
      saveRDS(anchorPlanes, file = "anchor_planes.rds")
    })

    shiny::observeEvent(input$print, {
      if (is.null(rv$fullTour)) {
        return()
      }
      pMat <- rv$fullTour[[rv$t]]
      t <- rv$t
      output$projPrint <-
        shiny::renderText(formatProj(pMat, input$parameters, t))
    })

    shiny::observeEvent(plotly::event_data("plotly_click"), {
      d <- plotly::event_data("plotly_click")
      rv$t <- d$x
      updatePlots(rv, session, input, output)
    })

    shiny::observeEvent(input$sampleSize, {
      if (is.null(rv$npoint) | is.null(rv$inSample) | !rv$init) {
        return()
      }
      if (input$sampleSize < 1 | is.na(input$sampleSize)) {
        return()
      }
      if (input$sampleSize != rv$npoint |
          nrow(rv$inSample) < rv$npoint) {
        if(rv$resetSample) {
          rv$selection <- rv$resetSelection
        }
        selection <- sample(rv$npoint, size = input$sampleSize)
        if(input$sampleSize==1){
          rv$inSample <- dplyr::as_tibble(t(rv$selection[selection, ]))
        }
        else  rv$inSample <- dplyr::as_tibble(rv$selection[selection, ])
        if(rv$npoint-input$sampleSize==1){
          rv$outOfSample <- dplyr::as_tibble(t(rv$selection[-selection, ]))
        }
        else rv$outOfSample <- dplyr::as_tibble(rv$selection[-selection, ])
        rv$dataMatrix <- as.matrix(rv$inSample[input$parameters])
        rv$selection <-
          rv$dataMatrix #FIXME do I need two copies of this??
        hoverTextDf <- hoverText(rv$inSample, input$parameters)
        updateReactiveData(rv)
        if ((input$displayType == "groups") && (input$groupVar!="None")){
          plotlyTour <-
            plotlyTourGrouped(rv$cdata, rv$cubeLine, hoverTextDf,
                                rv$halfRange,
                                rv$groups[[input$groupVar]][selection])
        }
        else{
          plotlyTour <-
            plotlyTourF(rv$cdata, rv$cubeLine, hoverTextDf, rv$halfRange)
        }
        output$tour <- plotly::renderPlotly(plotlyTour)
        pPlots <-
          plotly1d(rv$inSample, rv$outOfSample)
        output$params <- plotly::renderPlotly({
          plotly::subplot(
            pPlots,
            nrows = length(pPlots),
            heights = rep(rv$h1d, length(pPlots)),
            margin = 0.03
          ) %>%
            plotly::layout(title = "Parameter values")
        })
        rv$resetSample <- TRUE
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(
      c(plotly::event_data("plotly_selected"), input$selectedOnly),
      {
      if (!input$displayType=="linked brushing")
        return()
      if (is.null(plotly::event_data("plotly_selected")))
        return()
      # only even curve numbers are in current data sample
      newSelection <- plotly::event_data("plotly_selected") %>%
        dplyr::filter(isEven(curveNumber))
      oldSelection <- rv$s
      rv$s <- newSelection$pointNumber + 1
      if (input$selectionType == "Both selections") {
        rv$s <- unique(c(rv$s, oldSelection))
      }
      if (input$selectionType == "Overlapping set") {
        rv$s <- dplyr::intersect(rv$s, oldSelection)
      }
      rangeStr <- paste0("Number of selected points: ", length(rv$s))
      output$range <- shiny::renderText(rangeStr)
      #now assign color, use low alpha or remove non-selected points
      # FIXME should alpha be free parameter passed in through input value?
      markers <- rep(plotly::toRGB("black", alpha = 0.1), rv$npoint)
      markers[rv$s] <- plotly::toRGB("red")
      if (input$selectedOnly) {
        markers <- rep(plotly::toRGB("black", alpha = 0), rv$npoint)
        markers[rv$s] <- plotly::toRGB("red")
        rv$reloadScatter <- TRUE
        rv$selection <- rv$dataMatrix[rv$s, ]
        updateReactiveData(rv)
        hoverC <- hoverText(rv$d[rv$s, ], input$parameters)
        plotlyTour <-
          plotlyTourF(rv$cdata, rv$cubeLine, hoverC, rv$halfRange, red = TRUE)
        output$tour <- plotly::renderPlotly(plotlyTour)
      }
      else{
        if (rv$reloadScatter) {
          rv$selection <- rv$dataMatrix
          cHoverT <- hoverText(rv$inSample, input$parameters)
          updateReactiveData(rv)
          pTemp <-
            plotlyTourF(rv$cdata, rv$cubeLine, cHoverT, rv$halfRange)
          output$tour <- plotly::renderPlotly(pTemp)
          rv$reloadScatter <- FALSE
        }
        plotly::plotlyProxy("tour", session) %>%
          plotly::plotlyProxyInvoke("restyle", marker.color = list(markers))
      }
      pPlots <-
        plotly1d(rv$inSample,
                  rv$outOfSample,
                  list(color = markers))
      output$params <- plotly::renderPlotly({
        plotly::subplot(
          pPlots,
          nrows = length(pPlots),
          heights = rep(rv$h1d, length(pPlots)),
          margin = 0.03
        ) %>%
          plotly::layout(title = "Parameter values")
      })
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
      shiny::invalidateLater(20)
      shiny::isolate({
        updatePlots(rv, session, input, output)
        # keeping track of projection index
        # reset to 1 when reaching final projection
        if (rv$t == rv$tmax) {rv$stop <- TRUE}
        else{rv$t <- rv$t + 1}
      })

    })


  }

  shiny::shinyApp(ui(params, npoint), server)
}

