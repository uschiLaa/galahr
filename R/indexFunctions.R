#for easy editing we define tour options and matching to functions for guided tour indices here
#if index name is not found in getGuidedTour, return default (set to holes index)
guidedTourOptions <- c("cmass", "holes", "Skinny", "Striated", "Convex", "Clumpy",
                       "splines2d", "dcor2d", "MIC", "TIC")
groupedIndex <- c("lda_pp", "pda_pp")
getGuidedTour <- function(indexName, grId=NA){
  if(indexName=="cmass"){return(guided_tour(cmass()))}
  if(indexName=="holes"){return(guided_tour(holes()))}
  if(indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){return(guided_tour(scags(indexName)))}
  if(indexName=="splines2d"){return(guided_tour(splineIndex()))}
  if(indexName=="dcor2d"){return(guided_tour(dcorIndex()))}
  if(indexName %in% c("MIC","TIC")){return(guided_tour(mineIndex(indexName)))}
  if(indexName=="lda_pp"){return(guided_tour(lda_pp(grId)))}
  if(indexName=="pda_pp"){return(guided_tour(pda_pp(grId)))}
  return(guided_tour(holes))
}
getPathIndex <- function(fullTour, indexName){
  if(indexName=="cmass"){return(path_index(fullTour, cmass()))}
  if(indexName=="holes"){return(path_index(fullTour, holes()))}
  if(indexName %in% c("Skinny", "Striated", "Convex", "Clumpy")){return(path_index(fullTour, scags(indexName)))}
  if(indexName=="splines2d"){return(path_index(fullTour, splineIndex()))}
  if(indexName=="dcor2d"){return(path_index(fullTour, dcorIndex()))}
  if(indexName %in% c("MIC","TIC")){return(path_index(fullTour, mineIndex(indexName)))}
  return(NULL)
}
