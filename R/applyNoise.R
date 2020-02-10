#' @importFrom stats rnorm
applyNoise <- function(
    tabl
  , boundsDT
) {

  # Try 100 times to get unique values by adding noise.
  # Increase noise at each try.
  tries <- 1
  noiseAdd <- 0.04

  while(tries <= 95) {

    noiseAdd <- noiseAdd + 0.01

    noiseList <- lapply(
        boundsDT$N
      , function(x) {
        B <- boundsDT[get("N") == x,]
        betas <- rnorm(nrow(tabl),sd=noiseAdd)
        Vec <- betas+tabl[[x]]
        Vec <- pmin(pmax(Vec,0),1)
        if (B$C == "integer") Vec <- round(Vec)
        return(Vec)
      }
    )

    setDT(noiseList)

    if (uniqueN(noiseList) == nrow(noiseList)) break

    # If we have tried enough times, return a message to stop the process early and return results so far.
    if (tries >= 100) {
      msg <- "\n\nCould not apply noise to get enough random new parameter sets. Increase noiseAdd or decerase bulkNew. Stopping process and returning results so far."
      class(msg) <- "stopEarlyMsg"
      return(msg)
    }

    tries <- tries + 1

  }

  if(!identical(names(tabl),boundsDT$N)) noiseList <- cbind(noiseList, tabl[,-boundsDT$N, with = F])
  setnames(noiseList, names(tabl))
  return(noiseList)

}
