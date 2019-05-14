#' Generating marker styles for plotly traces.
#'
#' @param col Marker color as string (black, red).
#' @param a Transperancy (default is not transperant.)
#' @return List formatted as required for plotly marker style.
#' @keywords internal
getMarker <- function(col, a=NULL){
  if(is.null(a)) return(list(color = toRGB(col)))
  list(color = toRGB(col, a))
}

#' Mapping grouping to color.
#'
#' @param gr Vector containing group assignment for each entry.
#' @return Named list containing assigned colors ("color")
#'     and list of colors ("col")
#' @keywords internal
colorList <- function(gr){
  n <- length(unique(gr))
  col <- RColorBrewer::brewer.pal(n, "Dark2")
  colL <- col[as.numeric(as.factor(gr))]
  list(color=colL, col=col)
}

#' Formatted line style for drawing the cube outline for plotly.
#'
#' @keywords internal
cubeLineStyle <- list(color= toRGB("gray"),
                 width = 0.5)

#' Formatted empty axis style for plotly.
#'
#' @keywords internal
noAxis = list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#' Formatted empty axis style with fixed range used for tour plotting.
#'
#' @param halfRange Axis range will be between -/+ halfRange.
#' @return List formatted as required for plotly axis style.
#' @keywords internal
tourAxis <- function(halfRange){
  tAxis <- list(
    range = c(-halfRange,halfRange),
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  return(tAxis)
}

#' Build a legend of colored label text.
#'
#' @param labs Text vector containing all labels.
#' @param col Color vector containing corresponding colors.
#' @param halfRange Half range parameter used in the plot (for positioning)
#' @return Vector of list to use as annotation in a plotly layout call for
#'     generating the custom legend.
#' @keywords internal
customLegend <- function(labs, col, halfRange){
  a <- c()
  x = halfRange*0.7
  y = halfRange*0.9
  for (i in 1:length(labs)){
    a[[i]] <- list(text=paste0("<b>",labs[i],"</b>"), x=x, y=y,
                   font=list(size=14,color=col[i]), showarrow = FALSE)
    y <- y - halfRange*0.05
  }
  a
}


#' Generating the plotly tour display.
#'
#' @param scatterData Projected data points (matrix)
#' @param cubeData Projected hypercube points (matrix)
#' @param hoverData Data frame containing hover text in "paramT" column
#' @param halfRange Half range for fixing axis lenght
#' @param red Logical, if TRUE use red markers for the scatter points
#' @return plotly visualisation
#' @export
plotly_tour <- function(scatterData, cubeData, hoverData, halfRange, red=FALSE){
  if(red){scatterM <- getMarker("red")}
  else{scatterM <- getMarker("black")}
  tAxis <- tourAxis(halfRange)
  pRet <- plotly::plot_ly(type = "scatter") %>%
    # first trace is line connecting cube points
    plotly::add_trace(data = cubeData, x=~V1, y=~V2, inherit = FALSE,
                      type = "scatter", mode="lines", line=cubeLineStyle) %>%
    #second trace is scatter plot of projected data points
    plotly::add_trace(data = scatterData, x=~V1, y=~V2,
                      marker=scatterM, mode="markers", inherit = FALSE,
                      text = paste(hoverData$paramT, sep="\n"),
                      hoverinfo = 'text') %>%
    plotly::layout(dragmode = "select", xaxis=tAxis, yaxis=tAxis,
                   showlegend = FALSE) %>%
    plotly::toWebGL()
  return(pRet)
}

#' Generating the plotly tour display for grouped data.
#'
#' @param scatterData Projected data points (matrix)
#' @param cubeData Projected hypercube points (matrix)
#' @param hoverData Data frame containing hover text in "paramT" column
#' @param halfRange Half range for fixing axis lenght
#' @param gr Vector containing group assignment for each data entry
#' @return plotly visualisation
#' @export
plotly_tour_grouped <- function(scatterData, cubeData, hoverData, halfRange, gr){
  tAxis <- tourAxis(halfRange)
  labs <- unique(gr)
  markers <- colorList(gr)
  colrs <- markers$col # store colors for writing legend
  a <- customLegend(labs, colrs, halfRange)
  markers$col <- NULL # remove color list, now markers only contains marker color for each point
  pRet <- plotly::plot_ly(type = "scatter") %>%
    # first trace is line connecting cube points
    plotly::add_trace(data = cubeData, x=~V1, y=~V2, inherit = FALSE,
                      type = "scatter", mode="lines", line=cubeLineStyle,
                      showlegend = FALSE) %>%
    #second trace is scatter plot of projected data points with custom marker list for grouping
    plotly::add_trace(data = scatterData, x=~V1, y=~V2,
                      mode="markers", inherit = FALSE, marker=markers,
                      text = paste(hoverData$paramT, sep="\n"),
                      hoverinfo = 'text', showlegend = FALSE) %>%
    plotly::layout(dragmode = "select", xaxis=tAxis, yaxis=tAxis, annotations=a) %>%
    plotly::toWebGL()

  return(pRet)
}

#' Generating the plotly 1-d parameter display.
#'
#' This function generates a list of 1-d plotly visualisations
#' of the parameter values found in the sample. Data points outside
#' the sample are plotted as invisible markers ensuring that we span the
#' full range for each parameter. Custom markers can be passed in for
#' data points inside the selected sample. This plot is used in the app
#' for linked brushing, highlighting points in the tour display based on
#' selection on parameter values.
#'
#' @param d1 Data points in currently selected sample
#' @param d2 Data points outside the currently selected sample
#' @param markerD1 Custom marker (list) for d1, if NULL use default markers.
#' @return List of plotly visualisations
#' @export
plotly_1d <- function(d1, d2, markerD1=NULL){
  if(nrow(d2)>0){y2 = c(0)}
  else{y2 = NULL}
  if(is.null(markerD1)){markerD1 <- getMarker("black")}
  varList <- colnames(d1)
  cplots <- lapply(varList, function(var) {
    plotly::plot_ly(d1, y = c(0), x = stats::as.formula(paste0("~", var)),
                    mode = "markers", marker = markerD1, type = "scatter") %>%
      plotly::add_trace(data=d2, y=y2, x=stats::as.formula(paste0("~", var)),
                marker=getMarker("black",0)) %>%
      plotly::layout(dragmode = "select", yaxis = noAxis, showlegend=FALSE,
                     annotations =list(
                       text = paste0(var),
                       xref = "paper",
                       yref = "paper",
                       yanchor = "bottom",
                       xanchor = "center",
                       align = "center",
                       x = -0.05,
                       y = 0,
                       showarrow = FALSE
                       )
                     )
  })
  return(cplots)
}

#' Generating the plotly axes display.
#'
#' @param xVec Vector definig x direction of the projection
#' @param yVec Vector definig y direction of the projection
#' @param paramList Vector of parameter names
#' @return plotly visualisation of axes
#' @export
plotly_axes <- function(xVec, yVec, paramList){
  plotlyAxes <- plotly::plot_ly(type="scatter", mode = "lines")
  for(i in 1:length(xVec)){
    plotlyAxes <- plotlyAxes %>%
      plotly::add_trace(x=c(0,xVec[i]), y=c(0,yVec[i]), mode='lines', line=m)
  }
  plotlyAxes <- plotlyAxes %>%
    plotly::layout(xaxis=noAxis, yaxis=noAxis, showlegend = FALSE, shapes = list(
      list(type = 'circle',
           xref = 'x', x0 = -1, x1 = 1,
           yref = 'y', y0 = -1, y1 = 1,
           line = getMarker("black")),
      list(type = 'rect',
           xref = 'x', x0 = -1.5, x1 = 1.5,
           yref = 'y', y0 = -1.5, y1 = 1.5,
           line = getMarker("black")))) %>%
    plotly::add_annotations(x = 1.1*xVec,
                            y = 1.1*yVec,
                            text = paramList,
                    xref = "x",
                    yref = "y",
                    showarrow = FALSE)
  return(plotlyAxes)
}

#' Generating the ggplot timeline display.
#'
#' @param anchors Anchor plane indexes in the timeline
#' @param current Current projection index in the timeline.
#' @param maxT Final projection index limiting the timeline axis.
#' @param breaks Breaks for labelling timeline axis.
#' @param indexVals Projection pursuit index value as function of time.
#'     (default is NULL)
#' @return ggplot visualisation of timeline
#' @export
ggtimeline <- function(anchors, current, maxT, breaks, indexVals=NULL){
  timelinePlot <- ggplot2::ggplot() +
    ggplot2::geom_point(mapping = ggplot2::aes(x=anchors, y=0), color="red") +
    ggplot2::geom_point(mapping = ggplot2::aes(x=current, y=0)) +
    ggplot2::xlim(0,maxT) +
    ggplot2::theme_void() +
    ggplot2::geom_text(mapping = ggplot2::aes(x=breaks, y=0, label=breaks))
    if(!is.null(indexVals)){
      timelinePlot <- timelinePlot +
        ggplot2::geom_line(mapping = ggplot2::aes(x=c(0,maxT), y=c(0,0)), color="gray") +
        ggplot2::geom_line(mapping = ggplot2:aes(x=c(0,maxT), y=c(1,1)), color="gray") +
        ggplot2::geom_line(mapping = ggplot2::aes(x=1:maxT, y=as.vector(indexVals))) +
        ggplot2::ylim(-0.1,1.1)
    }
  return(timelinePlot)
}

#' Generating the ggplot coverage display.
#'
#' @param pcaRes Results from \code{\link{fullTourPCA}}
#' @param n Number of input parameters
#' @param i Index of current projection
#' @return ggplot visualisation of coverage display.
#' @export
coveragePlot <- function(pcaRes, n, i){
  ntot <- nrow(pcaRes$x)
  x <- dplyr::as_tibble(pcaRes$x) %>%
    dplyr::mutate(t = "data")
  x$t[ntot-n:ntot] <- "anchor"
  ret <- ggplot2::ggplot(x, aes(PC1, PC2, color=t)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(ggplot2::aes(x=x$PC1[2*i], y=x$PC2[2*i]), color="black") +
    ggplot2::geom_point(ggplot2::aes(x=x$PC1[2*i-1], y=x$PC2[2*i-1]), color="black") +
    ggplot2::theme_void() +
    ggplot2::guides(color=FALSE)
  return(ret)
}

#' Update all plots to current projection.
#'
#' @param rv Reactive value container
#' @param session shiny session
#' @param input shiny input container
#' @param output shiny output container
#' @keywords internal
updatePlots <- function(rv, session, input, output){
  updateReactiveData(rv)
  plotly::plotlyProxy("tour",session) %>%
    plotly::plotlyProxyInvoke("restyle", list(x = list(rv$cdata$V1), y = list(rv$cdata$V2)),list(2)) %>%
    plotly::plotlyProxyInvoke("restyle", list(x = list(rv$cubeLine$V1), y = list(rv$cubeLine$V2)),list(1))
  output$ggtimeline <- shiny::renderPlot(ggtimeline(rv$anchors, rv$t, rv$tmax, rv$timelineAxis, rv$pathIndex))
  # redraw axes
  xVec <- rv$fullTour[[rv$t]][,1]
  yVec <- rv$fullTour[[rv$t]][,2]
  plotlyAxes <- plotly_axes(xVec, yVec, input$parameters)
  output$axes <- plotly::renderPlotly(plotlyAxes)
  output$coverageDisplay <-
    shiny::renderPlot(coveragePlot(rv$tourPCA, length(input$parameters), rv$t))
}
