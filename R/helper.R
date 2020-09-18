#' Simple helper to test if number is even.
#'
#' @param x Numeric input to be tested.
#' @return Logical vector
#' @keywords internal
isEven <- function(x){
  x %% 2 == 0
}

#' Centering for projected data points
#'
#' @param dPoints Projected data points (matrix)
#' @export
centerData <- function(dPoints){
  m1 <- mean(dPoints[,1])
  m2 <- mean(dPoints[,2])
  x <- dPoints[,1] - m1
  y <- dPoints[,2] - m2
  dplyr::tibble(V1 = x, V2 = y)
}

#' Initialising the reactive values used in the app.
#'
#' @param paramDF Dataframe containing the input data.
#' @return Reactive value container.
#' @keywords internal
initializeReactive <- function(paramDF){
  rv <- shiny::reactiveValues()
  rv$on <- FALSE # only play tour after "play" button is pushed
  rv$stop <- FALSE # use this to stop when reaching final projection
  rv$t <- 1 # tour index
  rv$s <- NULL # plotly selected points
  rv$reloadScatter <- FALSE # when resetting from "selected only" need to reload scatter plot
  rv$update <- NULL
  rv$d <- paramDF
  rv$npoint <- nrow(paramDF)
  shiny::isolate(splitInput(paramDF, rv))
  return(rv)
}

#' Generate hover text for each data point.
#'
#' @param d Data frame containing input data
#' @param p Parameters to include in hover text.
#' @return Data frame with new column paramT containing the hover text.
#' @keywords internal
hoverText <- function(d, p){
  hoverTextDf <- d %>%
    tibble::add_column(paramT = "Parameters:")
  for(cP in p){
    cV <- signif(hoverTextDf[[cP]], digits=3)
    cT <- paste0(cP,": ",format(cV, scientific=TRUE))
    hoverTextDf$paramT <- paste(hoverTextDf$paramT,cT,sep="\n")
  }
  return(hoverTextDf)
}

#' Formatting projection matrix for screen printout.
#'
#' @param proj Projection matrix to be formatted
#' @param params Parameter names
#' @param idx Index value of this projection in the tour sequence
#' @return Formatted text for screen printout.
#' @export
formatProj <- function(proj, params, idx){
  txt <- paste0("Projection ", idx)
  txt <- paste(txt, "  x    y", sep="\n")
  for(i in seq_along(params)){
    cline <- paste(params[i], format(proj[i,1], digits=2),
                   format(proj[i,2], digits=2), sep=" ")
    txt <- paste(txt, cline, sep="\n")
  }
  return(txt)
}

#' Updating relevant reactive values.
#'
#' @param rv Reactive value container
#' @keywords internal
updateReactiveData <- function(rv){
  rv$cdata <- centerData(rv$dataMatrix %*% rv$fullTour[[rv$t]])
}

#' PCA over the projection vectors in the tour sequence.
#'
#' @param fullTour List of projections in interpolated tour path.
#' @param n Number of parameters.
#' @return prcomp output over all vectors in the path.
#' @export
fullTourPCA <- function(fullTour, n){
  allVectors <- NULL
  # split all projection matrices appearing in the path into vectors
  # collect in allVectors
  for(tourStep in fullTour){
    allVectors <- rbind(allVectors,tourStep[,1])
    allVectors <- rbind(allVectors,tourStep[,2])
  }
  # adding unit vectors in each parameter direction as anchor points
  allVectors <- rbind(allVectors, diag(1, n))
  # running the PCA on the resulting set of vectors
  tourPCA <- stats::prcomp(allVectors)
  return(tourPCA)
}


###################
# functions below are transforming existing functions for variable selection
# into tour index functions

#' Scagnostics based tour index
#'
#' Available scagnostics indexes are: Skinny, Striated, Convex, Clumpy
#'
#' @param scagMetricIndex Scagnostics index name to be used
#' @return Function taking matrix input data (projected data),
#'     returning value of selected scagnostics metric.
#' @export
scags <- function(scagMetricIndex) {
  function(mat) {
    return (binostics::scagnostics(mat)[scagMetricIndex])
  }
}

#' Spline based tour index
#'
#' @return Function taking matrix input data (projected data),
#'     returning value of splines2d metric.
#' @export
splineIndex <- function(){
  function(mat){
    return(mbgraphic::splines2d(mat[,1], mat[,2]))
  }
}

#' Distance correlation based tour index
#'
#' @return Function taking matrix input data (projected data),
#'     returning value of dcor2d metric.
#' @export
dcorIndex <- function(){
  function(mat){
    return(mbgraphic::dcor2d(mat[,1], mat[,2]))
  }
}

#' MINE based tour index
#'
#' Available indexes are MIC, TIC
#'
#' @param mineIndex MINE index name to be used
#' @return Function taking matrix input data (projected data),
#'     returning value of selected MINE metric.
#' @export
mineIndex <- function(mineIndex){
  function(mat){
    return(minerva::mine(mat[,1], mat[,2])[[mineIndex]])
  }
}

