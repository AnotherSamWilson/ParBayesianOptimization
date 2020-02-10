#' @title Calculate Acquisition Function
#'
#' @description
#' Function to be Maximized
#'
#' @param par Parameter set to predict
#' @param GPs an object of class gp
#' @param GPe an object of class gp
#' @param acq Acquisition function type to be used
#' @param y_max The current maximum known value of the target utility function
#' @param kappa tunable parameter kappa to balance exploitation against exploration
#' @param eps tunable parameter epsilon to balance exploitation against exploration
#' @importFrom stats dnorm pnorm
#' @return The acquisition function value.
#' @keywords internal

calcAcq <- function(par, scoreGP, timeGP, acq, y_max, kappa, eps) {

  # Excellent paper showing the derivation of each utility funciton.
  # https://www.cse.wustl.edu/~garnett/cse515t/spring_2015/files/lecture_notes/12.pdf

  if (acq == "ucb") {

    GP_Pred <- scoreGP$predict(par, se.fit = TRUE)
    return((GP_Pred$mean + kappa * (GP_Pred$se)))

  } else if (acq == "ei") {

    GP_Pred <- scoreGP$predict(par, se.fit = TRUE)
    z <- (GP_Pred$mean - y_max - eps) / (GP_Pred$se)
    return(((GP_Pred$mean - y_max - eps) * pnorm(z) + (GP_Pred$se) * dnorm(z)))

  } else if (acq == "eips") {

    GPs_Pred <- scoreGP$predict(par, se.fit = TRUE)
    GPe_Pred <- timeGP$predict(par, se.fit = TRUE)
    z <- (GPs_Pred$mean - y_max - eps) / (GPs_Pred$se)
    return(((GPs_Pred$mean - y_max - eps) * pnorm(z) + (GPs_Pred$se) * dnorm(z))/GPe_Pred$mean)

  } else if (acq == "poi") {

    GP_Pred <- scoreGP$predict(par, se.fit = TRUE)
    z <- (GP_Pred$mean - y_max - eps) / (GP_Pred$se)
    return((pnorm(z)))

  }

}
