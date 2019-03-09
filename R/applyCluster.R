#' @title Find Clusters
#'
#' @description
#' Applies DBSCAN algorithm to find local optimum candidate parameter sets
#'
#' @param e the entire parent environment is made available to this function
#' @importFrom dbscan dbscan
#' @importFrom data.table fintersect uniqueN
#' @return The next parameter sets to try
#' @keywords internal

applyCluster <- function(e = parent.frame()) {

  Lo2 <- copy(e$LocalOptims[,-"gradCount"])
  Lo2[,"GP_Utility" := get("GP_Utility")/max(get("GP_Utility"))]

  # If minClusterUtility is NULL, only select best optimum.
  # Else select the best N optimums that fall above minClusterUtility
    if (is.null(e$minClusterUtility)) {
  
      clusterPoints <- data.table(Lo2[order(-get("GP_Utility"))][1,])
      # drop <- c("GP_Utility","Duplicate")
  
    } else{
  
      Clust <- dbscan(Lo2,eps = length(Lo2)*sqrt(2)/1e3, minPts = 1)
  
      Lo2[,"Cluster" := Clust$cluster]
      clusterPoints <- copy(Lo2[Lo2[,.I[which.max(get("GP_Utility"))], by = get("Cluster")]$V1])
  
      # Filter out non-promising local optimums:
      clusterPoints <- head(clusterPoints[get("GP_Utility") >= e$minClusterUtility,
                                        ][order(-get("GP_Utility"))]
                           ,e$runNew)
      # drop <- c("GP_Utility","Cluster","Duplicate") # So data.table doesn't throw a warning
  
    }
  
  # Only keep clusters we haven't tried yet
    clusterPoints <- unMMScale(clusterPoints, e$boundsDT)
    clustN <- nrow(clusterPoints)
    
    clusterPoints$Duplicate <-  sapply(1:clustN, function(x) {
      nrow(fintersect(e$ScoreDT[,e$boundsDT$N, with = FALSE],clusterPoints[x,e$boundsDT$N, with = F]))
    })
  
    n2Clust <- 1
    while(sum(clusterPoints$Duplicate) == clustN & n2Clust <= 100) {
      
      if (n2Clust == 100) stop("Noise could not be added to find unique parameter set. Stopping process.")
      
      clusterPoints <- applyNoise(clusterPoints, e$boundsDT, e$noiseAdd)
      
      clusterPoints$Duplicate <-  sapply(1:clustN, function(x) {
        nrow(fintersect(e$ScoreDT[,e$boundsDT$N, with = FALSE],clusterPoints[x,e$boundsDT$N, with = F]))
      })
      
      n2Clust <- n2Clust + 1
      
    }
      
    clusterPoints <- clusterPoints[get("Duplicate") == 0,][,-c("Duplicate")]
      

  # Gaussian process will fail if it is fed duplicate parameter-score pairs.
  # This adds noise until unique values are found. If no unique values are
  # found after 1000 tries, the process stops. Selecting pairs from remaining
  # unexplored values becomes intractable when searching in high dimensions,
  # so random iterations are compared to pre-searched parameter sets.
    tries <- 1
    
    while(tries <= 1000) {
      
      if (e$runNew > clustN) { #Runs if there are additional parameter sets we need to find by adding noise.
        
        newP <- clusterPoints[rep(1:clustN,length.out = e$runNew-clustN),e$boundsDT$N, with = FALSE]
    
        noisyP <- applyNoise(newP, e$boundsDT, e$noiseAdd)
    
        if (nrow(fintersect(e$ScoreDT[,e$boundsDT$N, with = FALSE],noisyP)) == 0) {
            
          newSet <- rbind(clusterPoints[,e$boundsDT$N, with = F],noisyP)
          break
            
        }
          
      } else { # If we recieved all of our new parameter sets from local maximums..
          
        newSet <- clusterPoints[,e$boundsDT$N, with = F]
        break
        
      }
      
      if (tries == 1000) stop("Could not procure required number of unique parameter sets to run next scoring funciton. Stopping process.")
      
      tries <- tries + 1
    }

return(list(newSet = newSet, clusterPoints = clusterPoints))

}
