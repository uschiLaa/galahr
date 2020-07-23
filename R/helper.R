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
  m1 <- mean(c(dPoints[,1], cPoints[,1]))
  m2 <- mean(c(dPoints[,2], cPoints[,2]))
  dPoints[,1] <- dPoints[,1] - m1
  dPoints[,2] <- dPoints[,2] - m2
  cPoints[,1] <- cPoints[,1] - m1
  cPoints[,2] <- cPoints[,2] - m2
  centeredPoints <- list("data" = dplyr::as_tibble(dPoints),
                         "cube" = dplyr::as_tibble(cPoints))
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
  return(rv)
}

#' Initialising the data list used in the app.
#'
#' @param paramDF Dataframe containing the input data.
#' @return Reactive value container.
#' @keywords internal
initializeData <- function(paramDF, numVars, groupVars){
  d <- list()
  d$npoint <- nrow(paramDF)
  d$numVars <- purrr::map_lgl(paramDF, is.numeric)
  d$groupVars <- purrr::map_lgl(paramDF, is.character)
  d$d <- paramDF[d$numVars]
  d$groups <- paramDF[d$groupVars]
  return(d)
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
plotData <- function(d, rv){
  ret <- list()
  pdata <- (d$dataMatrix %*% d$fullTour[[rv$t]])
  cubePointProjOrig <- (rv$cubePoints %*% d$fullTour[[rv$t]])
  centeredPoints <- centerAll(pdata, cubePointProjOrig)
  ret$cdata <- centeredPoints$data
  cubePointProj <- centeredPoints$cube
  chidx <- grDevices::chull(cubePointProj)
  ret$cubeLine <- (cubePointProj[c(chidx, chidx[1]),])
  ret
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

