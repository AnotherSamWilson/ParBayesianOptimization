#' @title Maximize Acquisition Function
#'
#' @description
#' Uses optim function to find parameters that maximize Acquisition Function
#'
#' @param GP an object of class gp or gp.list
#' @param TryOver A data.table of initial points to try over.
#' @param acq Acquisition function type to be used
#' @param y_max The current maximum known value of the target utility function
#' @param kappa tunable parameter kappa to balance exploitation against exploration
#' @param eps tunable parameter epsilon to balance exploitation against exploration
#' @param parallel should the search be performed in parallel
#' @param convThresh convergence threshold. Passed to optim function "L-BFGS-B" factr parameter.
#' @return A data table of local optimums.
#' @importFrom stats optim
#' @importFrom data.table as.data.table
#' @import foreach
#' @keywords internal
#' @export

maxAcq <- function(GP, TryOver, acq = "ucb", y_max, kappa, eps, parallel, ParMethod, convThresh) {

  `%op%` <- ParMethod(parallel)

  LocalOptims <- foreach( i = 1:nrow(TryOver)
                          , .combine = 'rbind'
                          , .inorder = TRUE
                          , .errorhandling = 'remove'
                          , .packages = c('data.table','GauPro','stats')
                          , .multicombine = TRUE
                          , .verbose = FALSE
                          , .export = c('calcAcq')
  ) %op% {

    optim_result <- optim( par = TryOver[i,]
                           , fn = calcAcq
                           , GP = GP, acq = acq, y_max = y_max, kappa = kappa, eps = eps
                           , method = "L-BFGS-B"
                           , lower = rep(0, length(TryOver))
                           , upper = rep(1, length(TryOver))
                           , control = list( maxit = 1000
                                             , factr = convThresh
                                             , fnscale = -1
                           )
    )

    # Sometimes optim doesn't actually cap the bounds at 0 and 1.
    Pars <- sapply(optim_result$par,function(x){pmin(pmax(x,0),1)})

    as.data.table(as.list(c(Pars,GP_Utility = optim_result$value)))


  }
}
utils::globalVariables(c("i"))



