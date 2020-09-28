#' List of names for all available guided tour index functions.
#'
#' @keywords internal
guidedTourOptions <- c("cmass", "holes", "splines2d", "dcor2d", "lda_pp", "pda_pp")
if (requireNamespace("binostics", quietly = TRUE)) {
  guidedTourOptions <- c(
    guidedTourOptions, "Skinny", "Striated", "Convex", "Clumpy"
    )
}
if (requireNamespace("minerva", quietly = TRUE)) {
  guidedTourOptions <- c(guidedTourOptions, "MIC", "TIC")
}

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
  if (indexName=="splines2d") {
    return(
      tourr::guided_tour(tourr::splines2d())
    )
  }
  if (indexName=="dcor2d") {
    return(
      tourr::guided_tour(tourr::dcor2d())
    )
  }
  if (indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){
    return(
      tourr::guided_tour(scags(indexName))
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
