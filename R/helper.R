#' Simple helper to test if number is even.
#'
#' @param x Numeric input to be tested.
#' @return Logical vector
#' @keywords internal
isEven <- function(x){
  x %% 2 == 0
}

#' Combined centering for projected data and cube points
#'
#' For each view we center all points to be drawn.
#'
#' @param dPoints Projected data points (matrix)
#' @param cPoints Projected hypercube points (matrix)
#' @return Named list containint the centered data points ("data")
#'     and cube points ("cube")
#' @export
centerAll <- function(dPoints, cPoints){
  allPoints <- dplyr::as_tibble(dPoints) %>%
    tibble::add_column(t="data") %>%
    dplyr::bind_rows(tibble::add_column(
      dplyr::as_tibble(cPoints), t="cube"))
  centeredPoints <- dplyr::as_tibble(
    center(dplyr::select(allPoints, -t))) %>%
    tibble::add_column(t=allPoints$t)
  dPoints <- centeredPoints %>%
    dplyr::filter(t=="data") %>%
    dplyr::select(-t)
  cPoints <- centeredPoints %>%
    dplyr::filter(t=="cube") %>%
    dplyr::select(-t)
  centeredPoints <- list("data" = dPoints, "cube" = cPoints)
  return(centeredPoints)
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
  rv$subset <- FALSE # true when only sample of data points should be plotted
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

#' Generate vertices on hypercube enclosing all data points.
#'
#' @param n Number of parameters in the input data.
#' @param d Input data (numeric matrix).
#' @return Data frame containing all vertices of the hypercube
#' @export
cubePoints <- function(n, d){
  nCube <- geozoo::cube.iterate(n) #initialise
  cubeSidesLow <- apply(d,2,min)
  cubeSidesUp <- apply(d,2,max)
  cubeSideLength <- cubeSidesUp - cubeSidesLow
  cubePoints <- nCube$points %*%
    diag(cubeSideLength) %>%
    sweep(2,as.matrix(cubeSidesLow),"+",check.margin = FALSE)
  return(cubePoints)
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
  rv$cdata <- (rv$selection %*% rv$fullTour[[rv$t]])
  rv$cubePointProjOrig <- (rv$cubePoints %*% rv$fullTour[[rv$t]])
  centeredPoints <- centerAll(rv$cdata, rv$cubePointProjOrig)
  rv$cdata <- centeredPoints$data
  cubePointProj <- centeredPoints$cube
  chidx <- grDevices::chull(cubePointProj)
  rv$cubeLine <- (cubePointProj[c(chidx, chidx[1]),])

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

