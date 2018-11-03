#' @title Update Gaussian Process
#'
#' @description
#' Either initialize a gaussian process or update the current one.
#'
#' @param GP an object of class gp or list
#' @param kernel a \code{GauPro_kernel_beta} S6 class.
#' @param Data the data to either udpate or initialize the gaussian process
#' @param acq acquisition function type to be used
#' @param scaleList list of scaling parameters for the responses
#' @return A data table of local optimums.
#' @keywords internal
#' @export

updateGP <- function(GP, kern, X, Z, acq, scaleList, parallel) {


  if (is.null(GP)) {

    if (acq == "eips") {

      GPs <- GauPro_kernel_model$new(X
                                   , Z[,1]
                                   , kernel = kern
                                   , parallel = parallel
                                   , useC = FALSE)

      GPe <- GauPro_kernel_model$new(X
                                   , Z[,2]
                                   , kernel = kern
                                   , parallel = parallel
                                   , useC = FALSE)

      GP <- list(GPs,GPe)

    } else {

      GP <- GauPro_kernel_model$new(X
                                  , Z[,1]
                                  , kernel = kern
                                  , parallel = parallel
                                  , useC = FALSE)

    }

  } else{

    # If the acq has switched, only use the score GP
    if (class(GP)[[1]] == "list" & acq != "eips") GP <- GP[[1]]

    if (acq == "eips") {

      GPs <- GP[[1]]$update(Xnew = X, Znew = Z[,1])

      GPe <- GP[[2]]$update(Xnew = X, Znew = Z[,2])

      GP <- list(GPs,GPe)

    } else {

      GP <- GP$update(Xnew = X, Z[,1])

    }
  }
  return(GP)
}





