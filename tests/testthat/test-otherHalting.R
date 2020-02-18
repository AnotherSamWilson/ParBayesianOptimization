context('otherHalting')

set.seed(1991)

testthat::test_that(
  "timeLimit"

  , {
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
      , initPoints = 3
      , iters.n = 25
      , otherHalting = list(timeLimit = 5)
      , verbose = 0
    )

    expect_equal(optObj$stopStatus,"Time Limit - 5 seconds.")

  }

)

testthat::test_that(
  "minUtility"

  , {
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
      , initPoints = 3
      , iters.n = 25
      , otherHalting = list(minUtility = 0.1)
      , verbose = 0
    )

    expect_equal(optObj$stopStatus,"Could not meet minimum required (0.1) utility.")

  }

)
