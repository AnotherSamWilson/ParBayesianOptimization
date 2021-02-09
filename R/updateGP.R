#' Update Gaussian Processes in a bayesOpt Object
#'
#' To save time, Gaussian processes are not updated after the last iteration
#' in \code{addIterations()}. The user can do this manually, using this function
#' if they wish. This is not necessary to continue optimization using \code{addIterations}.
#' @param optObj an object of class bayesOpt
#' @param bounds The bounds to scale the parameters within.
#' @param verbose Should the user be warned if the GP is already up to date?
#' @param ... passed to \code{DiceKriging::km()}
#' @importFrom DiceKriging km
#' @return An object of class \code{bayesOpt} with updated Gaussian processes.
#' @examples
#' # Create initial object
#' scoringFunction <- function(x) {
#'   a <- exp(-(2-x)^2)*1.5
#'   b <- exp(-(4-x)^2)*2
#'   c <- exp(-(6-x)^2)*1
#'   return(list(Score = a+b+c))
#' }
#'
#' bounds <- list(x = c(0,8))
#'
#' Results <- bayesOpt(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 3
#'   , iters.n = 2
#'   , gsPoints = 10
#' )
#'
#' # At this point, the Gaussian Process has not been updated
#' # with the most recent results. We can update it manually:
#' Results <- updateGP(Results)
#' @export
updateGP <- function(optObj,bounds = optObj$bounds,verbose = 1, ...) {

  if (optObj$GauProList$gpUpToDate) {
    if (verbose > 0) message("Gaussian Processes are already up to date.")
    return(optObj)
  } else {

    boundsDT <- boundsToDT(bounds)
    scoreSummary <- optObj$scoreSummary[get("inBounds") & is.na(get("errorMessage")),]
    tries <- 1

    # We would like to set the trace to 0 by default in km.
    # The user can change this if they wish.

    # Parameters are 0-1 scaled, as are the scores.
    X <- minMaxScale(scoreSummary[,boundsDT$N,with=FALSE], boundsDT)
    Z <- zeroOneScale(scoreSummary$Score)

    # Attempt to get a GP with nonzero lengthscale parameters

    while(TRUE) {

      sgp <- tryCatch(
        {
          km(
            design = X
            , response = Z
            , control = list(trace = 0)
            , ...
          )
        }
        , error = function(e) {
          msg <- makeStopEarlyMessage(
            paste0(
                "Returning results so far. Error encountered while training GP: <"
              , conditionMessage(e)
              , ">"
            )
          )
          return(msg)
        }
      )

      if (class(sgp) == "stopEarlyMsg") {
        optObj$stopStatus <- sgp
        return(optObj)
      } else {
        optObj$GauProList$scoreGP <- sgp
      }

      if (all(optObj$GauProList$scoreGP@covariance@range.val >= 1e-4)) break

      if (tries >= 10) {
        cat("     - Could not obtain meaningful lengthscales.\n")
        break
      }

      tries <- tries + 1

    }

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
