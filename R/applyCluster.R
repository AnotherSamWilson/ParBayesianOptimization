#' @title Find Clusters
#'
#' @description
#' Applies DBSCAN algorithm to find local optimum candidate parameter sets
#'
#' @param e the entire parent environment is made available to this function
#' @importFrom dbscan dbscan
#' @importFrom data.table fintersect
#' @return the number of values that are outside the bounds/
#' @export

applyCluster <- function(e = parent.frame()) {

  Lo2 <- copy(e$LocalOptims[,-"gradCount"])
  Lo2[,"GP_Utility" := get("GP_Utility")/max(get("GP_Utility"))]

  if (is.null(e$minClusterUtility)) {

    clusterPoints <- data.table(Lo2[order(-get("GP_Utility"))][1,])
    drop <- "GP_Utility"

  } else{

    Clust <- dbscan(Lo2,eps = length(Lo2)*sqrt(2)/e$convThresh, minPts = 1)

    Lo2[,"Cluster" := Clust$cluster]
    clusterPoints <- copy(Lo2[Lo2[,.I[which.max(get("GP_Utility"))], by = get("Cluster")]$V1])

    # Filter out non-promising local optimums:
    clusterPoints <- head(clusterPoints[get("GP_Utility") >= e$minClusterUtility,
                                      ][order(-get("GP_Utility"))]
                         ,e$runNew)
    drop <- c("GP_Utility","Cluster") # So data.table doesn't throw an error.

  }

  newPoints <- nrow(clusterPoints)

  if (e$runNew > newPoints) {

    ScaleDT <- data.table(sapply(e$ParamNames,MinMaxScale,e$ScoreDT,e$boundsDT))
    newP <- clusterPoints[rep(1:newPoints,length.out = e$runNew-newPoints)]
    tries <- 1

    # Gaussian process will fail if it is fed duplicate parameter-score pairs.
    # This adds noise until unique values are found. If no unique values are
    # found after 100 tries, the process stops.
    while(tries < 100) {

      noisyP <- sapply( e$ParamNames
                      , applyNoise
                      , table = newP
                      , boundsDT = e$boundsDT
                      , noiseAdd = e$noiseAdd
                      , scaled = TRUE
                      )

      if (nrow(fintersect(ScaleDT,data.table(noisyP))) == 0) {
        newSet <- rbind(clusterPoints[,(drop) := NULL],noisyP)
        break
      }

      tries <- tries + 1

    }

    if (tries == 100) stop("Could not procure required number of unique parameter sets to run next scoring funciton. Stopping process.")


  } else {
    newSet <- clusterPoints[,(drop) := NULL]
  }

return(list(newSet = newSet, clusterPoints = clusterPoints))

}
