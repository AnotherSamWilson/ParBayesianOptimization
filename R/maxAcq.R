#' @title Maximize Acquisition Function
#'
#' @description
#' Uses optim function to find parameters that maximize Acquisition Function
#'
#' @param GPs an object of class gp
#' @param GPe an object of class gp
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

maxAcq <- function(GPs, GPe, TryOver, acq = "ucb", y_max, kappa, eps, parallel, ParMethod, convThresh) {

  `%op%` <- ParMethod(parallel)

  LocalOptims <- foreach(
      i = 1:nrow(TryOver)
    , .combine = 'rbind'
    , .inorder = TRUE
    , .errorhandling = 'pass'
    , .packages = c('data.table','GauPro','stats')
    , .multicombine = TRUE
    , .verbose = FALSE
    , .export = c('calcAcq')
  ) %op% {

    optim_result <- optim(
        par = TryOver[i,]
      , fn = calcAcq
      , GPs = GPs, GPe = GPe, acq = acq, y_max = y_max, kappa = kappa, eps = eps
      , method = "L-BFGS-B"
      , lower = rep(0, length(TryOver))
      , upper = rep(1, length(TryOver))
      , control = list(
          maxit = 1000
        , factr = convThresh
        , fnscale = -1
        , pgtol = .Machine$double.eps
        )
    )

    # Sometimes optim doesn't actually cap the bounds at 0 and 1.
    Pars <- sapply(optim_result$par,function(x){pmin(pmax(x,0),1)})

    as.data.table(as.list(c( Pars
                           , gpUtility = optim_result$value
                           , gradCount = optim_result$counts[[2]]
                           )
                          )
                  )
  }

}
utils::globalVariables(c("i"))
