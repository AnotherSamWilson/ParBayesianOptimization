

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
CheckBounds <- function(x, Table, bounds) sum(!between(Table[[x]], lower = bounds[[x]][[1]], upper = bounds[[x]][[2]]))


#' @title Generate Random Parameters
#'
#' @description
#' Generates a list of random parameters within bounds
#'
#' @param x Parameter Name
#' @param Rpoints number of parameter sets to create.
#' @param bounds original bounds list
#' @importFrom stats runif
#' @return a data.table of random parameters
#' @keywords internal
RandParams <- function(x, Rpoints, boundsDT) {

  B <- boundsDT[get("N") == x,]

  if (B$C == "integer"){

    base::sample(B$L:B$U, size = Rpoints, replace = TRUE)

  } else {

    runif(Rpoints, min = B$L, max =B$U)

  }
}

#' @title Min-Max Scale
#'
#' @description
#' Scales a data.table of parameter sets to a 0-1 range
#'
#' @param x Parameter Name
#' @param table A data.table of parameter sets
#' @param bounds the original bounds list
#' @return a data.table the same length as \code{table} with scaled parameters
#' @keywords internal
MinMaxScale <- function(x, table, boundsDT) {
  B <- boundsDT[get("N") == x,]
  (table[[x]]-B$L) / (B$U-B$L)
}


#' @title Undo Min-Max Scale
#'
#' @description
#' Un-scales a data.table of parameter sets from a 0-1 range
#'
#' @param x Parameter Name
#' @param table A data.table of scaled parameter sets
#' @param bounds the original bounds list
#' @return a data.table the same length as \code{table} with un-scaled parameters
#' @keywords internal
UnMMScale <- function(x, table, boundsDT) {
  B <- boundsDT[get("N") == x,]

  if (B$C == "integer") {
    return(round((B$U-B$L)*table[[x]]+B$L,0))
  } else  return((B$U-B$L)*table[[x]]+B$L)
}


#' @title Assign Kernel
#'
#' @description
#' This function exists so GauPro doesn't have to be loaded to run BayesianOptimization
#'
#' @param kern a kernel
#' @param beta the log10(theta) the lengthscale parameter
#' @return an GauPro_kernel_beta R6 class
#' @keywords internal
assignKern <- function(kern,beta) {

  if(kern == "Matern32"){kern <- Matern32$new(beta)
  } else if (kern == "Matern52") { kern <- Matern52$new(beta)
  } else if (kern == "Exponential") { kern <- Exponential$new(beta)
  } else if (kern == "Gaussian") { kern <- Gaussian$new(beta)}

  return(kern)

}

























