#' @title Apply Noise
#'
#' @description
#' Adds noise to a list of parameter sets
#'
#' @param x Parameter Name
#' @param tabl A data.table of parameters to add noise to
#' @param bounds the original bounds list
#' @param noiseAdd Fraction of the range of the bounds that samples will be pulled from
#' @importFrom stats rbeta
#' @return a data.table with the same number of rows as \code{table} with noise added
#' @keywords internal

applyNoise <- function(
    tabl
  , boundsDT
  , noiseAdd
) {

  # Try 100 times to get unique values by adding noise.
  tries <- 1

  while(tries <= 100) {

    noiseList <- lapply(boundsDT$N, function(x) {

      B <- boundsDT[get("N") == x,]
      betas <- (rbeta(nrow(tabl), shape1 = 4,shape2 = 4)-0.5)
      Vec <- betas*noiseAdd*B$R+tabl[[x]]
      Vec <- pmin(pmax(Vec,B$L),B$U)

      if (B$C == "integer") Vec <- round(Vec)

      return(Vec)

    })

    setDT(noiseList)

    if (uniqueN(noiseList) == nrow(noiseList)) break

    if (tries >= 100) return("\n\nCould not apply noise to get enough random new parameter sets. Increase noiseAdd or decerase bulkNew. Stopping process and returning results so far.")

    tries <- tries + 1

  }

  if(!identical(names(tabl),boundsDT$N)) noiseList <- cbind(noiseList, tabl[,-boundsDT$N, with = F])
  setnames(noiseList, names(tabl))
  return(noiseList)

}
