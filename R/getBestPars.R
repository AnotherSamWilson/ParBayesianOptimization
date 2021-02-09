#' Get the Best Parameter Set
#'
#' Returns the N parameter sets which resulted in the maximum scores from \code{FUN}.
#'
#' @param optObj An object of class \code{bayesOpt}
#' @param N The number of parameter sets to return
#' @return A list containing the \code{FUN} inputs which resulted in the highest returned Score.
#' If N > 1, a \code{data.table} is returned. Each row is a result from \code{FUN}, with results ordered by
#' descending Score.
#' @examples
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
#' print(getBestPars(Results))
#' @export
getBestPars <- function(
    optObj
  , N = 1
) {

  if (N > nrow(optObj$scoreSummary)) stop("N is greater than the iterations that have been run.")

  if (N == 1) {
    return(as.list(head(optObj$scoreSummary[order(-get("Score"))],1))[names(optObj$bounds)])
  } else {
    head(optObj$scoreSummary[order(-get("Score"))],N)[,names(optObj$bounds),with=FALSE]
  }

}
