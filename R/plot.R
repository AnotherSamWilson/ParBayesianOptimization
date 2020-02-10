#' @title Plot Progress
#'
#' @description
#' This function simply creates 2 stacked ggplots.
#'
#' @param x An object of class bayesOpt
#' @param ... unused
#' @importFrom ggplot2 ggplot aes_string xlab scale_color_discrete geom_point theme guides guide_legend margin element_text unit xlim ylab
#' @importFrom ggpubr ggarrange annotate_figure text_grob
#' @importFrom graphics plot
#' @return an object of class ggarrange
#' @export
plot.bayesOpt <- function(x,...) {

  # x <- Results
  #
  # plot(x)

  acqN <- getAcqInfo(x$optPars$acq)

  # Score Plot
  sc <- ggplot(x$scoreSummary,aes_string(x="Epoch",y="Score",color="acqOptimum")) +
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
  ut <- ggplot(x$scoreSummary[!is.na(get("gpUtility")),],aes_string(x="Epoch",y="gpUtility",color="acqOptimum")) +
    geom_point() +
    xlim(c(0,max(x$scoreSummary$Epoch))) +
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
  )

  annotate_figure(
      gga
    , top = text_grob(label = "Bayesian Optimization Results")
  )

}
