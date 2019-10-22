

# Helper Functions



#' @title Check Bounds
#'
#' @description
#' Checks if a list of parameters is within the supplied bounds
#'
#' @param x Parameter Name
#' @param Table A data.table of parameter values to check
#' @param bounds the bounds list
#' @importFrom data.table between
#' @return the number of values that are outside the bounds/
#' @keywords internal
checkBounds <- function(x, Table, bounds) {

  sum(!between(Table[[x]], lower = bounds[[x]][[1]], upper = bounds[[x]][[2]]))

}


#' @title Generate Random Parameters
#'
#' @description
#' Generates a set of rPoints parameter sets from latin hypercube sampling.
#'
#' @param boundsDT original bounds list
#' @param rPoints number of parameter sets to create.
#' @param FAIL Should the function fail if it cannot find enough distinct parameter sets?
#' @importFrom stats runif
#' @return a data.table of random parameters
#' @keywords internal
randParams <- function(boundsDT, rPoints, FAIL = TRUE) {

  # Attempt to procure rPoints unique parameter sets by lhs.
  attempt <- 1
  newPars <- data.table()
  poi <- rPoints

  while(attempt <= 100) {

    latinCube <- data.table(lhs::improvedLHS(n = poi, k = nrow(boundsDT)))

    setnames(latinCube, boundsDT$N)

    newPars <- unique(rbind(unMMScale(latinCube, boundsDT),newPars))

    if (nrow(newPars) == rPoints) break else poi <- rPoints-nrow(newPars)

    if (attempt >= 100 & FAIL) stop("Latin Hypercube Sampling could not produce the required distinct parameter sets. \nTry decreasing gsPoints or initPoints.")

    attempt <- attempt + 1

  }

  setnames(newPars, boundsDT$N)
  return(newPars)

}

#' @title Min-Max Scale
#'
#' @description
#' Scales a data.table of parameter sets to a 0-1 range
#' @param tabl A data.table of parameter sets
#' @param boundsDT the original bounds list
#' @return a data.table the same length as \code{table} with scaled parameters
#' @keywords internal
minMaxScale <- function(tabl, boundsDT) {

  # tabl <- newD

  mms <- lapply(boundsDT$N, function(x) (tabl[[x]]-boundsDT[get("N")==x,]$L)/boundsDT[get("N")==x,]$R)

  setDT(mms)
  setnames(mms, boundsDT$N)
  return(mms)

}


#' @title Undo Min-Max Scale
#'
#' @description
#' Un-scales a data.table of parameter sets from a 0-1 range
#'
#' @param tabl A data.table of scaled parameter sets
#' @param boundsDT the original bounds list
#' @return a data.table the same length as \code{table} with un-scaled parameters
#' @keywords internal
unMMScale <- function(tabl, boundsDT) {

  umms <- lapply(boundsDT$N, function(x) {

    B <- boundsDT[get("N")==x,]

    n <- tabl[[x]]*B$R+B$L

    if (B$C == "integer") n <- round(n)

    return(n)

  })

  setDT(umms)
  if(!identical(names(tabl),boundsDT$N)) umms <- cbind(umms, tabl[,-boundsDT$N, with = F])
  setnames(umms, names(tabl))
  return(umms)

}

#' @title ZeroOneScale
#'
#' @description
#' Scales a vector to be between 0 and 1 without supplied bounds.
#'
#' @param vec a vector of numbers
#' @return the vector re-scaled between 0 and 1.
#' @keywords internal
zeroOneScale <- function(vec) {

  r <- max(vec) - min(vec)

  vec <- (vec - min(vec))/r

  return(vec)

}

#' @title Check for Pre-Existing Parameters
#'
#' @description
#' Check if rows from tab1 exist in tab2.
#'
#' @param tab1 The table to check
#' @param tab2 The table to compare.
#' @return A vector of booleans of length \code{nrow(tab1)} representing whether each row
#' of tab1 already exists in tab2 or in proceeding rows of tab1.
#' @keywords internal
checkDup <- function(tab1,tab2) {

  sapply(1:nrow(tab1), function(i) {
    tab2 <- rbind(tab2,tab1[0:(i-1),])
    nrow(fintersect(tab2,tab1[i,])) > 0
  })

}

#' @title Assign Kernel
#'
#' @description
#' This function exists so GauPro doesn't have to be loaded to run BayesianOptimization
#'
#' @param kern a kernel
#' @return an GauPro_kernel_beta R6 class
#' @keywords internal
assignKern <- function(kern,params) {

  betas <- rep(0,params)

  if(kern == "Matern32"){kern <- Matern32$new(betas)

  } else if (kern == "Matern52") { kern <- Matern52$new(betas)

  } else if (kern == "Exponential") { kern <- Exponential$new(betas)

  } else if (kern == "Gaussian") { kern <- Gaussian$new(betas)}

  return(kern)

}
