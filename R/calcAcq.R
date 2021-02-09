#' @importFrom stats dnorm pnorm predict
#' @keywords internal
calcAcq <- function(par, scoreGP, timeGP, acq, y_max, kappa, eps) {

  # Excellent paper showing the derivation of each utility funciton.
  # https://www.cse.wustl.edu/~garnett/cse515t/spring_2015/files/lecture_notes/12.pdf

  # DiceKriging requires columns
  p <- matrix(par,ncol=length(par),dimnames = list(NULL,names(par)))

  GP_Pred <- predict(scoreGP,p,type="SK")

  if (acq == "ucb") {

    return((GP_Pred$mean + kappa * (GP_Pred$sd)))

  } else if (acq == "ei") {

    z <- (GP_Pred$mean - y_max - eps) / (GP_Pred$sd)
    return(((GP_Pred$mean - y_max - eps) * pnorm(z) + (GP_Pred$sd) * dnorm(z)))

  } else if (acq == "eips") {

    GPe_Pred <- predict(timeGP,p,type="SK")
    z <- (GP_Pred$mean - y_max - eps) / (GP_Pred$sd)
    return(((GP_Pred$mean - y_max - eps) * pnorm(z) + (GP_Pred$sd) * dnorm(z))/GPe_Pred$mean)

  } else if (acq == "poi") {

    z <- (GP_Pred$mean - y_max - eps) / (GP_Pred$sd)
    return((pnorm(z)))

  }

}
