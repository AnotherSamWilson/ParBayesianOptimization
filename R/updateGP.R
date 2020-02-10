#' Update Gaussian Processes in a bayesOpt Object
#'
#' To save time, Gaussian processes are not updated after the last iteration
#' in \code{addIterations()}. The user can do this manually, using this function
#' if they wish.
#' @param optObj an object of class bayesOpt
#' @param bounds The bounds to scale the parameters within.
#' @param verbose Should the user be warned if the GP is already up to date?
#' @return a \code{bayesOpt} object with updated Gaussian Processes.
#' @export
updateGP <- function(optObj,bounds = optObj$bounds,verbose = TRUE) {

  if (optObj$GauProList$gpUpToDate) {
    if (verbose) message("Gaussian Processes are already up to date.")
    return(optObj)
  } else {

    boundsDT <- boundsToDT(bounds)
    scoreSummary <- optObj$scoreSummary[get("inBounds"),]

    # Parameters are 0-1 scaled, as are the scores.
    X = data.matrix(minMaxScale(scoreSummary, boundsDT))
    Z <- if(optObj$optPars$acq == "eips") {
      data.matrix(scoreSummary[,.(zeroOneScale(scoreSummary$Score),scoreSummary$Elapsed/max(scoreSummary$Elapsed))])
    } else {
      data.matrix(zeroOneScale(scoreSummary$Score))
    }

    optObj$GauProList$scoreGP <- GauPro_kernel_model$new(
        X
      , matrix(Z[,1])
      , kernel = optObj$GauProList$scoreKernel
      , parallel = FALSE
      , useC = FALSE
    )

    if (optObj$optPars$acq == "eips") {
      optObj$GauProList$timeGP <- GauPro_kernel_model$new(
          X
        , matrix(Z[,2])
        , kernel = optObj$GauProList$timeKernel
        , parallel = FALSE
        , useC = FALSE
      )
    }

    optObj$GauProList$gpUpToDate <- TRUE

  }

  return(optObj)

}
