#' Get the Best Parameter Set
#'
#' Returns the parameter set which resulted in the maximum score from \code{FUN}.
#'
#' If N > 1, a data.table with N rows is returned, order by score decreasing.
#' If N = 1, a list of parameters is returned.
#'
#' @param optObj An object of class bayesOpt
#' @param N The number of parameter sets to return
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
