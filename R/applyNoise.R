#' @title Apply Noise
#'
#' @description
#' Adds noise to a list of parameter sets
#'
#' @param x Parameter Name
#' @param Table A data.table of parameters to add noise to
#' @param bounds the original bounds list
#' @param saveFirst keep the first row as the original parameter set
#' @param noiseAdd Fraction of the range of the bounds that samples will be pulled from
#' @importFrom stats rbeta
#' @return a data.table with the same number of rows as Table with noise added
#' @keywords internal
#' @export
#'
ApplyNoise <- function(x, Table, bounds, saveFirst = TRUE, noiseAdd = 0.15) {

  if (class(bounds[[x]]) == "integer")  {Orig <- as.integer(round(Table[[x]][1],0))
  } else Orig <- Table[[x]][1]
  Range <- bounds[[x]][[2]] - bounds[[x]][[1]]
  betas <- (rbeta(nrow(Table), shape1 = 4,shape2 = 4)-0.5)
  Vec <- betas*noiseAdd*Range+Table[[x]]
  if (class(bounds[[x]]) == "integer") Vec <- as.integer(round(Vec,0))
  Vec <- pmin(pmax(Vec,bounds[[x]][[1]]),bounds[[x]][[2]])

  if (saveFirst) return(c(Orig,head(Vec,-1)))
  else return(Vec)

}
