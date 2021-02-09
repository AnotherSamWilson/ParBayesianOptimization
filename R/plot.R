#' Plot a \code{bayesOpt} object
#'
#' Returns 2 stacked plots - the top shows the results from FUN at each iteration.
#' The bottom shows the utility from each point before the search took place.
#'
#' @param x An object of class bayesOpt
#' @param ... Passed to \code{ggarrange()} when plots are stacked.
#' @importFrom ggplot2 ggplot aes_string xlab scale_color_discrete geom_point theme guides guide_legend margin element_text unit xlim ylab
#' @importFrom ggpubr ggarrange annotate_figure text_grob
#' @importFrom graphics plot
#' @return an object of class \code{ggarrange} from the \code{ggpubr} package.
#' @examples
#' scoringFunction <- function(x) {
#'   a <- exp(-(2-x)^2)*1.5
#'   b <- exp(-(4-x)^2)*2
#'   c <- exp(-(6-x)^2)*1
#'   return(list(Score = a+b+c))
#' }
#'
#' bounds <- list(x = c(0,8))
#'
#' Results <- bayesOpt(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 3
#'   , iters.n = 2
#'   , gsPoints = 10
#' )
#' # This plot will also show in real time with parameter plotProgress = TRUE in bayesOpt()
#' plot(Results)
#' @export
plot.bayesOpt <- function(x,...) {

  acqN <- getAcqInfo(x$optPars$acq)
  scoreSummary <- x$scoreSummary[!is.na(get("Score")),]

  # Score Plot
  sc <- ggplot(scoreSummary,aes_string(x="Epoch",y="Score",color="acqOptimum")) +
    geom_point() +
    xlab("") +
    scale_color_discrete(drop=TRUE,limits=c(TRUE,FALSE)) +
    theme(
        legend.position = 'bottom'
      , legend.spacing.x = unit(0.6, 'cm')
      , legend.text = element_text(margin = margin(t = 1))
      , legend.margin = margin(t = 0,b=10)
      , plot.margin=unit(c(1,1,0,0), units="line")
    ) +
    guides(color = guide_legend(
        title = "Local\nOptimum"
      , label.position = "bottom"
      , title.position = "left"
      , title.hjust = 1
      )
    )

  # Utility Plot
  ut <- ggplot(scoreSummary[!is.na(get("gpUtility")),],aes_string(x="Epoch",y="gpUtility",color="acqOptimum")) +
    geom_point() +
    xlim(c(0,max(scoreSummary$Epoch))) +
    ylab("Utility") +
    scale_color_discrete(drop=TRUE,limits=c(TRUE,FALSE)) +
    theme(
      legend.position = 'bottom'
      , legend.spacing.x = unit(0.6, 'cm')
      , legend.text = element_text(margin = margin(t = 1))
      , legend.margin = margin(t = 0,b=10)
      , plot.margin=unit(c(0,1,1,0), units="line")
    ) +
    guides(color = guide_legend(
      title = "Local\nOptimum"
      , label.position = "bottom"
      , title.position = "left"
      , title.hjust = 1
      )
    )

  gga <- ggarrange(
      sc
    , ut
    , align = "v"
    , ncol=1
    , common.legend = TRUE
    , legend = "bottom"
    , ...
  )

  print(
    annotate_figure(
        gga
      , top = text_grob(label = "Bayesian Optimization Results")
    )
  )

}
