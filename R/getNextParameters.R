#' @importFrom dbscan dbscan
#' @importFrom data.table fintersect uniqueN setorder
getNextParameters <- function(
    LocalOptims
  , boundsDT
  , scoreSummary
  , runNew
  , acq
  , kappa
  , eps
  , acqThresh
  , acqN
  , scoreGP
  , timeGP
) {

  LocalOptims <- LocalOptims[get("relUtility") >= acqThresh,]
  LocalOptims <- LocalOptims[,c(boundsDT$N,"gpUtility"),with=FALSE]
  setorder(LocalOptims,-"gpUtility")
  LocalOptims$acqOptimum <- TRUE

  # Mark clusters as duplicates if they have already been attempted. Note that
  # parameters must match exactly. Whether or not we should eliminate 'close'
  # parameters is experimental, and could cause problems as the parameter space
  # becomes more fully explored.
  LocalOptims$Duplicate <- checkDup(
      LocalOptims[,boundsDT$N,with=FALSE]
    , scoreSummary[,boundsDT$N,with=FALSE]
  )

  # If we already have runNew non-duplicate local optims, use the best of those.
  if (sum(!LocalOptims$Duplicate) >= runNew) {

    LocalOptims$Duplicate <- NULL
    return(head(LocalOptims,runNew))

  } else {

    # If there weren't runNew distinct local optimums...

    # Keep usable local optims
    returnParameters <- LocalOptims[!LocalOptims$Duplicate,]

    # Obtain required number of candidate parameter sets. We add noise to these.
    procure <- runNew - nrow(returnParameters)
    candidateParameters <- minMaxScale(LocalOptims[rep(1:nrow(LocalOptims),length.out=procure),],boundsDT)
    candidateParameters$acqOptimum <- FALSE

    # This is not expensive, so tries is large.
    # Attempt to obtain unique parameter sets by adding noise.
    tries <- 1
    while(procure > 0 & tries <= 1000) {

      if (tries >= 1000) {
        return(
          makeStopEarlyMessage("Noise could not be added to find unique parameter set. Stopping process and returning results so far.")
        )
      }

      # Only replace custers that are not duplicates.
      fromNoise <- applyNoise(
            tabl = candidateParameters
          , boundsDT = boundsDT
        )

      # Pass stopping message if that is what applyNoise returned
      if(any(class(fromNoise) == "stopEarlyMsg")) return(fromNoise)

      # Calculate the utility at these spots.
      fromNoise$gpUtility <- apply(
        fromNoise[,boundsDT$N,with=FALSE]
        , MARGIN = 1
        , calcAcq
        , scoreGP = scoreGP
        , timeGP = timeGP
        , acq = acq
        , y_max = 1
        , kappa = kappa
        , eps = eps
      )

      fromNoise$gpUtility <- fromNoise$gpUtility - acqN$base

      fromNoise <- unMMScale(fromNoise,boundsDT)

      # See if any of these have already been run
      fromNoise$Duplicate <- checkDup(
        fromNoise[,boundsDT$N,with=FALSE]
        , rbind(
            scoreSummary[,boundsDT$N,with=FALSE]
          , returnParameters[,boundsDT$N,with=FALSE])
      )

      # If we obtained any unique parameter sets:
      if (any(!fromNoise$Duplicate)) {
        returnParameters <- rbind(returnParameters,fromNoise,fill=TRUE)
        procure <- runNew - nrow(returnParameters)
      }

      tries <- tries + 1

    }

  returnParameters$Duplicate <- NULL

  return(returnParameters)

  }

}
