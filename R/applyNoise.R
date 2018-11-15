#' @title Apply Noise
#'
#' @description
#' Adds noise to a list of parameter sets
#'
#' @param x Parameter Name
#' @param table A data.table of parameters to add noise to
#' @param bounds the original bounds list
#' @param noiseAdd Fraction of the range of the bounds that samples will be pulled from
#' @importFrom stats rbeta
#' @return a data.table with the same number of rows as \code{table} with noise added
#' @keywords internal
#' @export
#'
applyNoise <- function( x
                      , table
                      , boundsDT
                      , noiseAdd
                      , scaled = FALSE) {

  B <- boundsDT[get("N") == x,]

  if (scaled) {
    Range <- 1
    B$L <- 0
    B$U <- 1
  } else {
    Range <-B$U - B$L
  }

  betas <- (rbeta(nrow(table), shape1 = 4,shape2 = 4)-0.5)
  Vec <- betas*noiseAdd*Range+table[[x]]

  Vec <- pmin(pmax(Vec,B$L),B$U)

  return(Vec)

}

