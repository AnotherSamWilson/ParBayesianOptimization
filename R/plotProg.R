#' @title Plot Progress
#'
#' @description
#' This function simply creates 2 stacked ggplots.
#'
#' @param dt ScoreDT
#' @param acq acq
#' @importFrom gridExtra arrangeGrob
#' @importFrom ggplot2 ggplot geom_dotplot ggtitle xlab ylab theme aes_string
#' @return Nothing. Prints plot to display.
#' @keywords internal
plotProg <- function(dt,acq) {

  # dt <- ScoreDT
  dt$Iteration <- factor(dt$Iteration)
  acqDT <- data.table(
      a = c("ei","ucb","poi","eips")
    , n = c("Expected Improvement"
          , "Upper Confidence Bound"
          , "Probability of Improvement"
          , "Expected Improvement / Second")
  )
  acqN <- acqDT[acqDT$a == acq,]$n

  sBW <- (max(dt$Score) - min(dt$Score))/50
  uBW <- (max(dt$gpUtility) - min(dt$gpUtility))/50

  s <- ggplot(dt,aes_string(x="Iteration",y="Score",fill="acqOptimum")) +
    geom_dotplot(binaxis = "y",dotsize = 1,stackratio = 1,stackdir="centerwhole",binwidth = sBW) +
    ggtitle("Scores Obtained in Each Iteration") +
    xlab("Iteration") +
    ylab("Score (Returned From FUN)") +
    theme(legend.title.align=0.5)
  s$labels$fill <- "Acquisition\nOptimum"


  u <- ggplot(dt,aes_string(x="Iteration",y="gpUtility",fill="acqOptimum")) +
    geom_dotplot(binaxis = "y",dotsize = 1,stackratio = 1,stackdir="centerwhole",binwidth = uBW) +
    ggtitle(paste0("Scaled Acquisition Function at Each Iteration")) +
    xlab("Iteration") +
    ylab(acqN) +
    theme(legend.title.align=0.5)
  u$labels$fill <- "Acquisition\nOptimum"

  return(arrangeGrob(s,u))

}
