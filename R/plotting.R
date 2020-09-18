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

#' Update all plots to current projection.
#'
#' @param rv Reactive value container
#' @param session shiny session
#' @param input shiny input container
#' @param output shiny output container
#' @keywords internal
updateGroups <- function(rv, session, input){
  if(input$groupVar == "None") {
    markers <- "black"
    a <- NULL
  }
  else {
    gr <- rv$groups[[input$groupVar]]
    labs <- unique(gr)
    markers <- colorList(gr)
    colrs <- markers$col # store colors for writing legend
    a <- customLegend(labs, colrs, rv$halfRange)
    markers$col <- NULL # remove color list, now markers only contains marker color for each point
  }
  plotly::plotlyProxy("tour",session) %>%
    plotly::plotlyProxyInvoke("restyle", marker.color = list(markers$color),
                              list(2)) %>%
    plotly::plotlyProxyInvoke("relayout", annotations=a)

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
                      text = 1:maxT, hoverinfo = 'text',
                      type = "scatter") %>%
    plotly::add_trace(y = c(0.5), x = anchors,
                      mode = "markers", marker = getSmallMarker("red"),
                      text = anchors, hoverinfo = 'text',
                      type = "scatter") %>%
    plotly::add_trace(y = c(0.5), x = c(current,current), #duplicating point to make restyle work
                      mode = "markers", marker = getSmallMarker("black"),
                      text = "Current position", hoverinfo = 'text',
                      type = "scatter") %>%
    plotly::layout(xaxis=timelineAxis(maxT, breaks), yaxis = noAxisRange(0,1),
                   showlegend = FALSE, margin = plotlyMargin) %>%
    plotly::config(displayModeBar=FALSE)
  return(timelinePlot)
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

}
