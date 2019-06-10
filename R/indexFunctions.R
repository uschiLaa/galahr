#' List of names for all available guided tour index functions.
#'
#' @keywords internal
guidedTourOptions <- c("cmass", "holes")
if (requireNamespace("binostics", quietly = TRUE)) {
  guidedTourOptions <- c(
    guidedTourOptions, "Skinny", "Striated", "Convex", "Clumpy"
    )
}
if (requireNamespace("mbgraphic", quietly = TRUE)) {
  guidedTourOptions <- c(guidedTourOptions, "splines2d", "dcor2d")
}
if (requireNamespace("minerva", quietly = TRUE)) {
  guidedTourOptions <- c(guidedTourOptions, "MIC", "TIC")
}

#' List of names for all available guided tour index functions with grouping.
#'
#' @keywords internal
groupedIndex <- c("lda_pp", "pda_pp")

#' Taking guided tour index name and returning corresponding guided tour
#'
#' @param indexName Index name from [guidedTourOptions()]
#' @param grId Class to be used when using [groupedIndex()]
#' @return guided_tour with corresponding index function
#' @export
getGuidedTour <- function(indexName, grId=NA){
  if (indexName=="cmass") {
    return(
      tourr::guided_tour(tourr::cmass())
      )
    }
  if (indexName=="holes") {
    return(
      tourr::guided_tour(tourr::holes())
    )
    }
  if (indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){
    return(
      tourr::guided_tour(scags(indexName))
      )
    }
  if (indexName=="splines2d"){
    return(
      tourr::guided_tour(splineIndex())
    )
    }
  if (indexName=="dcor2d"){
    return(
      tourr::guided_tour(dcorIndex())
    )
    }
  if (indexName %in% c("MIC","TIC")){
    return(
      tourr::guided_tour(mineIndex(indexName))
      )
    }
  if (indexName=="lda_pp"){
    return(tourr::guided_tour(tourr::lda_pp(grId))
    )
    }
  if (indexName=="pda_pp"){
    return(
      tourr::guided_tour(tourr::pda_pp(grId))
    )
  }
  # default behaviour if index not found
  return(
    tourr::guided_tour(tourr::holes())
    )
}

#' Taking interpolated tour path and index name and returning path index
#'
#'
#'
#' @param fullTour Interpolated tour path
#' @param indexName Index name from [guidedTourOptions()]
#' @param grId Class to be used when using [groupedIndex()]
#' @return Index values over the tour history
#' @export
getPathIndex <- function(fullTour, indexName, grId=NA){
  if (indexName=="cmass"){
    return(
      tourr::path_index(fullTour, tourr::cmass())
    )
    }
  if (indexName=="holes"){
    return(
      tourr::path_index(fullTour, tourr::holes())
    )
    }
  if (indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){
    return(
      tourr::path_index(fullTour, scags(indexName))
      )
    }
  if (indexName=="splines2d"){
    return(
      tourr::path_index(fullTour, splineIndex())
    )
    }
  if (indexName=="dcor2d"){
    return(tourr::path_index(fullTour, dcorIndex())
    )
    }
  if (indexName %in% c("MIC","TIC")){
    return(
      tourr::path_index(fullTour, mineIndex(indexName))
      )
    }
  if (indexName=="lda_pp"){
    return(
      tourr::path_index(fullTour, tourr::lda_pp(grId))
      )
    }
  if (indexName=="pda_pp"){
    return(
      tourr::path_index(fullTour, tourr::pda_pp(grId))
      )
    }
  return(NULL)
}
