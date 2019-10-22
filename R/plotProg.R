#' @title Plot Progress
#'
#' @description
#' This function simply creates 2 stacked ggplots.
#'
#' @param dt ScoreDT
#' @param acq acq
#' @importFrom plotly plot_ly subplot layout toRGB %>%
#' @return Nothing. Prints plot to display.
#' @keywords internal
plotProg <- function(dt,acq) {

  dt$desc <- ifelse(dt$acqOptimum,"Acquisition Optimum","Generated From Noise")
  scheme <- "Set1"
  acqN <- data.table(
    nam = c("ei","eips","poi","ucb")
    , disp = c("Expected Improvement","Expct. Imprvmt./Second", "Prob. of Improvement","Upper Conf. Bound")
  )

  axisFont <- list(
    size = 15
  )
  axisX <- list(
    title = "Iteration"
    , titlefont = axisFont
    , autotick = FALSE
    , ticks = "outside"
    , tick0 = 0
    , dtick = 1
    , ticklen = 5
    , tickwidth = 1
    , tickcolor = toRGB("blue")

  )
  axisYs <- list(
    title = "Result from FUN"
    , titlefont = axisFont
  )
  axisYu <- list(
    title = acqN[acqN$nam == acq,]$disp
    , titlefont = axisFont
  )
  titleFont <-

    s <- plot_ly(
      data = dt
      , x = ~Iteration
      , y = ~Score
      , type = 'scatter'
      , color = ~desc
      , colors = scheme
      , mode = "markers"
      , showlegend = TRUE
      , width = 500
      , height = 500
    ) %>%
    layout(
      xaxis = axisX
      , yaxis = axisYs
      , legend = list(orientation = 'h',x = 0.095,y = -0.16)
    )
  u <- plot_ly(
    data = dt
    , x = ~Iteration
    , y = ~gpUtility
    , type = 'scatter'
    , color = ~desc
    , colors = scheme
    , mode = "markers"
    , showlegend = FALSE
    , width = 500
    , height = 600
  ) %>%
    layout(
      xaxis = axisX
      , yaxis = axisYu
    )

  return(
    subplot(s,u
            , nrows = 2
            , shareX = TRUE
            , shareY = FALSE
            , titleY = TRUE
            , margin = 0.03
    ) %>%
      layout(
        title = "Bayesian Optimization Progress"
        #, showlegend=FALSE
        #, showlegend2=TRUE
        , plot_bgcolor='rgb(240, 240, 240)'
      )
  )

}
