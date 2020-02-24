#' @title Get Local Optimums of the Acquisition Function From a bayesOpt Object
#'
#' Returns all local optimums of the acquisition function, no matter the utility.
#'
#' \code{gsPoints} points in the parameter space are randomly initialized, and
#' the L-BFGS-B method is used to find the closest local optimum to each point.
#' dbscan is then used to cluster points together which converged to the same
#' optimum - only unique optimums are returned.
#'
#' @param optObj an object of class bayesOpt. The following parameters are all defaulted to
#' the options provided in this object, but can be manually specified.
#' @param bounds Same as in \code{bayesOpt()}
#' @param acq Same as in \code{bayesOpt()}
#' @param kappa Same as in \code{bayesOpt()}
#' @param eps Same as in \code{bayesOpt()}
#' @param convThresh Same as in \code{bayesOpt()}
#' @param gsPoints Same as in \code{bayesOpt()}
#' @param parallel Same as in \code{bayesOpt()}
#' @param verbose Should warnings be shown before results are returned prematurely?
#' @return A data table of local optimums, including the utility (gpUtility), the
#' utility relative to the max utility (relUtility), and the steps taken in the
#' L-BFGS-B method (gradCount).
#' @importFrom stats optim
#' @importFrom data.table as.data.table
#' @import foreach
#' @export
getLocalOptimums <- function(
    optObj
  , bounds = optObj$bounds
  , acq = optObj$optPars$acq
  , kappa = optObj$optPars$kappa
  , eps = optObj$optPars$eps
  , convThresh = optObj$optPars$convThresh
  , gsPoints = optObj$optPars$gsPoints
  , parallel = FALSE
  , verbose = 1
) {

  # Set helper objects and initial conditions.
  boundsDT <- boundsToDT(bounds)
  `%op%` <- ParMethod(parallel)
  tryN <- 0
  reduceThresh <- function(x) if (x <= 100) return(x) else return(x/10)
  acqN <- getAcqInfo(acq)
  continue <- TRUE

  while(continue) {

    # Create random points to initialize local maximum search.
    localTries <- randParams(boundsDT, gsPoints, FAIL = FALSE)
    localTryMM <- minMaxScale(localTries, boundsDT)

    LocalOptims <- foreach(
        notI = 1:nrow(localTryMM)
      , .combine = 'rbind'
      , .inorder = TRUE
      , .errorhandling = 'pass'
      , .packages = c('DiceKriging','stats')
      , .multicombine = TRUE
      , .verbose = FALSE
      , .export = c('calcAcq')
    ) %op% {

      # global binding
      notI <- get("notI")

      optim_result <- optim(
          par = localTryMM[notI,]
        , fn = calcAcq
        , scoreGP = optObj$GauProList$scoreGP, timeGP = optObj$GauProList$timeGP, acq = acq, y_max = 1, kappa = kappa, eps = eps
        , method = "L-BFGS-B"
        , lower = rep(0, length(localTryMM))
        , upper = rep(1, length(localTryMM))
        , control = list(
            maxit = 1000
          , factr = convThresh
          , fnscale = -1
          )
      )

      # Sometimes optim doesn't actually cap the bounds at 0 and 1.
      Pars <- sapply(optim_result$par,function(x){pmin(pmax(x,0),1)})

      return(
        as.data.table(
          as.list(
            c(
                Pars
              , gpUtility = optim_result$value
              , gradCount = optim_result$counts[[2]]
            )
          )
        )
      )

    }

    tryN <- tryN + 1

    # Checking for convergence
    if (tryN >= 4) {
      if (verbose > 0) cat("\n     - Maximum convergence attempts exceeded - process is probably sampling random points.")
      continue <- FALSE
    } else if (max(LocalOptims$gpUtility) < acqN$base | !any(LocalOptims$gradCount > 2)) {
      if (verbose > 0) cat("\n     - Convergence Not Found. Trying again with tighter parameters...")
      gsPoints <- gsPoints * (tryN + 1)
      convThresh <- reduceThresh(convThresh)
      continue <- TRUE
    } else continue <- FALSE

  }

  # Adjustment for upper confidence bound.
  LocalOptims$gpUtility <- LocalOptims$gpUtility - acqN$base

  # Define relative Utility to compare to acqThresh
  LocalOptims$relUtility <- LocalOptims$gpUtility/max(LocalOptims$gpUtility)

  # run DBSCAN to determine which random points converged to the same place. If there are multiple
  # local optimums of the acquisition function present in the Gaussian process, this filters out the duplicates.
  Clust <- dbscan(
    LocalOptims[,boundsDT$N,with=FALSE]
    , eps = nrow(boundsDT)*sqrt(2)*1e-2
    , minPts = 1
  )
  LocalOptims$localOptimum <- Clust$cluster

  # Take the best parameter set from each local optimum
  LocalOptims <- LocalOptims[LocalOptims[,.I[which.max(get("relUtility"))], by = get("localOptimum")]$V1]
  LocalOptims <- unMMScale(LocalOptims, boundsDT)
  setcolorder(LocalOptims,c("localOptimum",boundsDT$N,"gpUtility","relUtility","gradCount"))

  return(LocalOptims)

}
