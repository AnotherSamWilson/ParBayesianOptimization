#' Update Gaussian Processes in a bayesOpt Object
#'
#' To save time, Gaussian processes are not updated after the last iteration
#' in \code{addIterations()}. The user can do this manually, using this function
#' if they wish.
#' @param optObj an object of class bayesOpt
#' @param bounds The bounds to scale the parameters within.
#' @param verbose Should the user be warned if the GP is already up to date?
#' @param ... passed to \code{DiceKriging::km()}
#' @importFrom DiceKriging km
#' @return a \code{bayesOpt} object with updated Gaussian Processes.
#' @export
updateGP <- function(optObj,bounds = optObj$bounds,verbose = 1,...) {

  if (optObj$GauProList$gpUpToDate) {
    if (verbose > 0) message("Gaussian Processes are already up to date.")
    return(optObj)
  } else {

    boundsDT <- boundsToDT(bounds)
    scoreSummary <- optObj$scoreSummary[get("inBounds"),]

    # Parameters are 0-1 scaled, as are the scores.
    X <- minMaxScale(scoreSummary[,boundsDT$N,with=FALSE], boundsDT)
    Z <- zeroOneScale(scoreSummary$Score)

    optObj$GauProList$scoreGP <- km(
        design = X
      , response = Z
      , control = list(trace = 0)
      , ...
    )

    if (optObj$optPars$acq == "eips") {
      optObj$GauProList$timeGP <- km(
        design = X
        , response = zeroOneScale(scoreSummary$Elapsed)
        , scaling = FALSE
        , control = list(trace = 0)
      )

    }

    optObj$GauProList$gpUpToDate <- TRUE

  }

  return(optObj)

}
