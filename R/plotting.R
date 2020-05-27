#' Generating marker styles for plotly traces.
#'
#' @param col Marker color as string (black, red).
#' @param a Transperancy (default is not transperant.)
#' @return List formatted as required for plotly marker style.
#' @keywords internal
getMarker <- function(col, a=NULL){
  if (is.null(a)){
    return(
      list(color = plotly::toRGB(col))
      )
  }
  list(color = plotly::toRGB(col, a))
}

#' Generating smaller marker styles for plotly traces.
#'
#' @param col Marker color as string (black, red).
#' @param a Transperancy (default is not transperant.)
#' @return List formatted as required for plotly marker style.
#' @keywords internal
getSmallMarker <- function(col, a=NULL){
  if(is.null(a)) return(list(color = plotly::toRGB(col), size=5))
  list(color = plotly::toRGB(col, a))
}

#' Formatted margins for the timeline in plotly.
#'
#' @keywords internal
plotlyMargin <- list(l= 5,
                     r= 15,
                     t= 0,
                     b= 20
                     )

#' Formatted margins for the coverage display in plotly.
#'
#' @keywords internal
coverageDispMargin <- list(l= 5,
                         r= 5,
                         t= 5,
                         b= 5
)

#' Mapping grouping to color.
#'
#' @param gr Vector containing group assignment for each entry.
#' @return Named list containing assigned colors ("color")
#'     and list of colors ("col")
#' @keywords internal
colorList <- function(gr){
  allColors <- TRUE
  for (c in unique(gr)){
    if (inherits(try(plotly::toRGB(c), silent=TRUE), "try-error")) {
      allColors <- FALSE
    }
  }
  if (allColors) return(list(color=gr, col=unique(gr)))
  n <- length(unique(gr))
  col <- RColorBrewer::brewer.pal(n, "Dark2")
  colL <- col[as.numeric(as.factor(gr))]
  list(color=colL, col=col)
}

#' Formatted line style for drawing the cube outline for plotly.
#'
#' @keywords internal
cubeLineStyle <- list(color= plotly::toRGB("gray"),
                 width = 0.5)

#' Formatted empty axis style for plotly.
#'
#' @keywords internal
noAxis <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

#' Formatted empty axis style with fixed range used for timeline plotting.
#'
#' @param axmin Lower limit of axis
#' @param axmax Upper limit of axis
#' @return List formatted as required for plotly axis style.
#' @keywords internal
noAxisRange <- function(axmin, axmax){
  list(
    range = c(axmin, axmax),
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
    )
}

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

#' Formatted axis style with fixed range (0, xMax+1) and breaks.
#'
#' @param xMax Upper limit for axis range
#' @param breaks Values for axis ticks
#' @return List formatted as required for plotly axis style.
#' @keywords internal
timelineAxis<- function(xMax, breaks){
  tAxis <- list(
    range = c(0, xMax+1),
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    tickvals = breaks,
    ticks = "inside",
    tickfont = list(size = 10),
    showgrid = FALSE,
    automargin = TRUE
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
  x <- halfRange*0.7
  y <- halfRange*0.9
  for (i in seq_along(labs)){
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
#' @return Plotly visualisation
#' @export
plotlyTourF <- function(scatterData, cubeData, hoverData, halfRange, red=FALSE){
  if(red){scatterM <- getMarker("red")}
  else{scatterM <- getMarker("black")}
  tAxis <- tourAxis(halfRange)
  pRet <- plotly::plot_ly(type = "scatter", mode = "markers") %>%
    # first trace is line connecting cube points
    plotly::add_trace(data = cubeData, x=~V1, y=~V2, inherit = FALSE,
                      type = "scatter", mode="lines", line=cubeLineStyle) %>%
    #second trace is scatter plot of projected data points
    plotly::add_trace(data = scatterData, x=~V1, y=~V2, type = "scatter",
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
#' @return Plotly visualisation
#' @export
plotlyTourGrouped <- function(scatterData, cubeData, hoverData, halfRange, gr){
  tAxis <- tourAxis(halfRange)
  labs <- unique(gr)
  markers <- colorList(gr)
  colrs <- markers$col # store colors for writing legend
  a <- customLegend(labs, colrs, halfRange)
  markers$col <- NULL # remove color list, now markers only contains marker color for each point
  pRet <- plotly::plot_ly(type = "scatter", mode = "markers") %>%
    # first trace is line connecting cube points
    plotly::add_trace(data = cubeData, x=~V1, y=~V2, inherit = FALSE,
                      type = "scatter", mode="lines", line=cubeLineStyle,
                      showlegend = FALSE) %>%
    #second trace is scatter plot of projected data points with custom marker list for grouping
    plotly::add_trace(data = scatterData, x=~V1, y=~V2, type = "scatter",
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
#' @param d Data points in currently selected sample
#' @param markerD1 Custom marker (list) for d1, if NULL use default markers.
#' @return List of plotly visualisations
#' @export
plotly1d <- function(d, marker=NULL){
  if(is.null(marker)){marker <- getMarker("black")}
  varList <- colnames(d)
  cplots <- lapply(varList, function(var) {
    plotly::plot_ly(d, y = c(0), x = stats::as.formula(paste0("~", var)),
                    mode = "markers", marker = marker, type = "scatter") %>%
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
#' @return Plotly visualisation of axes
#' @export
plotlyAxesF <- function(xVec, yVec, paramList){
  ret <- plotly::plot_ly(type="scatter", mode = "lines")
  for(i in seq_along(xVec)){
    ret <- ret %>%
      plotly::add_trace(
        x=c(0,xVec[i]), y=c(0,yVec[i]), mode='lines', line=getMarker("black")
        )
  }
  ann <- list(x = 1.1*xVec,
              y = 1.1*yVec,
              text = paramList,
              xref = "x",
              yref = "y",
              showarrow = FALSE)
  ret <- ret %>%
    plotly::layout(xaxis=noAxis, yaxis=noAxis, showlegend = FALSE,
                   shapes = list(
                     list(type = 'circle',
                          xref = 'x', x0 = -1, x1 = 1,
                          yref = 'y', y0 = -1, y1 = 1,
                          line = getMarker("black")),
                     list(type = 'rect',
                          xref = 'x', x0 = -1.5, x1 = 1.5,
                          yref = 'y', y0 = -1.5, y1 = 1.5,
                          line = getMarker("black"))),
                   annotations = ann) %>%
    plotly::config(displayModeBar=FALSE)

  return(ret)
}



#' Generating the timeline display.
#'
#' @param anchors Anchor plane indexes in the timeline
#' @param current Current projection index in the timeline.
#' @param maxT Final projection index limiting the timeline axis.
#' @param breaks Breaks for labelling timeline axis.
#' @param indexVals Projection pursuit index value as function of time.
#'     (default is NULL)
#' @return Visualisation of timeline
#' @export
ggtimeline <- function(anchors, current, maxT, breaks, indexVals=NULL){
  breaks <- breaks[breaks<maxT] # throw out breaks above maxT
  timelinePlot <- plotly::plot_ly(type = "scatter", mode = "markers", source = "TL") %>%
    plotly::add_trace(y = c(0.5), x = 1:maxT, #invisible markers for click events
                      mode = "markers", marker = getSmallMarker("black", a = 0),
                      type = "scatter") %>%
    plotly::add_trace(y = c(0.5), x = anchors,
                      mode = "markers", marker = getSmallMarker("red"),
                      type = "scatter") %>%
    plotly::add_trace(y = c(0.5), x = c(current,current), #duplicating point to make restyle work
                      mode = "markers", marker = getSmallMarker("black"),
                      type = "scatter") %>%
    plotly::layout(xaxis=timelineAxis(maxT, breaks), yaxis = noAxisRange(0,1),
                   showlegend = FALSE, margin = plotlyMargin) %>%
    plotly::config(displayModeBar=FALSE)
  return(timelinePlot)
}

#' Generating the coverage display.
#'
#' @param pcaRes Results from [fullTourPCA()]
#' @param n Number of input parameters
#' @param i Index of current projection
#' @return Visualisation of coverage display.
#' @export
coveragePlot <- function(pcaRes, n, i){
  ntot <- nrow(pcaRes$x)
  x <- dplyr::as_tibble(pcaRes$x) %>%
    dplyr::mutate(t = "data")
  x$t[ntot-n:ntot] <- "anchor"
  dpoints <- dplyr::filter(x, t=="data")
  apoints <- dplyr::filter(x, t=="anchor")
  ret <- plotly::plot_ly(type = "scatter", mode = "markers") %>%
    plotly::add_trace(x=dpoints$PC1, y=dpoints$PC2,
                      mode = "markers", marker = getMarker("darkorchid"),
                      type = "scatter") %>%
    plotly::add_trace(x=apoints$PC1, y=apoints$PC2,
                      mode = "markers", marker = getMarker("chartreuse3"),
                      type = "scatter") %>%
    plotly::add_trace(x = c(apoints$PC1[2*i], apoints$PC1[2*i-1]),
                      y = c(apoints$PC2[2*i], apoints$PC2[2*i-1]),
                      mode = "markers", marker = getMarker("black"),
                      type = "scatter") %>%
    plotly::layout(xaxis=noAxis, yaxis = noAxis,
                   showlegend = FALSE, margin=coverageDispMargin) %>%
    plotly::config(displayModeBar=FALSE)
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
    plotly::plotlyProxyInvoke("restyle",
                              list(x = list(rv$cdata$V1),
                                   y = list(rv$cdata$V2)),
                              list(2)) %>%
    plotly::plotlyProxyInvoke("restyle",
                              list(x = list(rv$cubeLine$V1),
                                   y = list(rv$cubeLine$V2)),
                              list(1))

  #reminder: restyle only works for more than one point in the trace
  plotly::plotlyProxy("ggtimeline",session) %>%
    plotly::plotlyProxyInvoke("restyle",
                              list(x = list(c(rv$t, rv$t))),
                              list(3))

  # redraw axes
  xVec <- rv$fullTour[[rv$t]][,1]
  yVec <- rv$fullTour[[rv$t]][,2]
  plotlyAxes <- plotlyAxesF(xVec, yVec, input$parameters)
  output$axes <- plotly::renderPlotly(plotlyAxes)


  pc1 <- rv$tourPCA$x[,1]
  pc2 <- rv$tourPCA$x[,2]
  plotly::plotlyProxy("coverageDisplay",session) %>%
    plotly::plotlyProxyInvoke("restyle", list(
      x = list(c(pc1[2*rv$t], pc1[2*rv$t-1])),
      y = list(c(pc2[2*rv$t], pc2[2*rv$t-1]))),
      list(3))

}
