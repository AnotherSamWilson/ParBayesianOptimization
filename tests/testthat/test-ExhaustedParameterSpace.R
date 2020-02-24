test_that(

  "Exhaust Parameter Space"

  , {

    skip_on_cran()
    set.seed(5)
    sf <- function(x) {
      y <- 1 - x^2
      return(y)
    }
    FUN <- function(x) {
      return(list(Score = sf(x)))
    }
    bounds = list(
      x = c(-4L,4L)
    )
    Results <- bayesOpt(
      FUN = FUN
      , bounds = bounds
      , saveFile = NULL
      , initPoints = 4
      , iters.n = 300
      , iters.k = 1
      , otherHalting = list(timeLimit = Inf,minUtility = 0)
      , acq = "ucb"
      , kappa = 2.576
      , eps = 0.0
      , parallel = FALSE
      , gsPoints = 10
      , convThresh = 1e8
      , acqThresh = 1.000
      , plotProgress = TRUE
      , verbose = 1
    )

    expect_equal(
        Results$stopStatus
      , ParBayesianOptimization:::makeStopEarlyMessage(
        paste0(
            "Noise could not be added to find unique parameter set. "
          , "Stopping process and returning results so far."
        )
      )
    )

  }

)
