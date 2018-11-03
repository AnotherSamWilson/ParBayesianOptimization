

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
#' @export
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
#' @export
RandParams <- function(x, Rpoints, bounds) {

  if (class(bounds[[x]]) == "integer"){

    base::sample(bounds[[x]][[1]]:bounds[[x]][[2]], size = Rpoints, replace = TRUE)

  } else {

    runif(Rpoints, min = bounds[[x]][[1]], max = bounds[[x]][[2]])

  }
}

#' @title Min-Max Scale
#'
#' @description
#' Scales a data.table of parameter sets to a 0-1 range
#'
#' @param x Parameter Name
#' @param Table A data.table of parameter sets
#' @param bounds the original bounds list
#' @return a data.table the same length as Table with scaled parameters
#' @keywords internal
#' @export
MinMaxScale <- function(x, Table, bounds) {
  (Table[[x]]-bounds[[x]][[1]]) / (bounds[[x]][[2]]-bounds[[x]][[1]])
}


#' @title Undo Min-Max Scale
#'
#' @description
#' Un-scales a data.table of parameter sets from a 0-1 range
#'
#' @param x Parameter Name
#' @param Table A data.table of scaled parameter sets
#' @param bounds the original bounds list
#' @return a data.table the same length as Table with un-scaled parameters
#' @keywords internal
#' @export
UnMMScale <- function(x, Table, bounds) {
  (bounds[[x]][[2]]-bounds[[x]][[1]])*Table[[x]]+bounds[[x]][[1]]
}


#' @title Assign Kernel
#'
#' @description
#' This function exists so GauPro doesn't have to be loaded to run BayesianOptimization
#' @param kern a kernel
#' @param beta the log10(theta) the lengthscale parameter
#' @return a data.table the same length as Table with un-scaled parameters
#' @keywords internal
#' @export
assignKern <- function(kern,beta) {

  if(kern == "Matern32"){kern <- Matern32$new(beta)
  } else if (kern == "Matern52") { kern <- Matern52$new(beta)
  } else if (kern == "Exponential") { kern <- Exponential$new(beta)
  } else if (kern == "Gaussian") { kern <- Gaussian$new(beta)}

  return(kern)

}

























