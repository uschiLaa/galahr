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
