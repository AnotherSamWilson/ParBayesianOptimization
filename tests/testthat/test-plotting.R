testthat::test_that(

  "Standard Plotting"

  , {

    skip_on_cran()
    set.seed(0)

    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      return(list(Score = sf(x,y)))
    }

    bounds = list(
      x = c(0,15)
      , y = c(-20,100)
    )

    optObj <- bayesOpt(
        FUN
      , bounds
      , initPoints = 6
      , iters.n = 12
      , iters.k = 2
      , plotProgress = TRUE
      , verbose = 0
    )

    optObj
    plot(optObj)
  }
)
