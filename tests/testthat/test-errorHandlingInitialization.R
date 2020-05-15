context('errorHandling')

testthat::test_that(

  "Error in FUN - Initialization"

  , {

    skip_on_cran()
    set.seed(10)
    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      if (runif(1) > 0.25) stop("You foo'd when you should have bar'd.")
      return(list(Score = sf(x,y)))
    }

    bounds = list(
      x = c(0,15)
      , y = c(-20,100)
    )

    expect_error(
      bayesOpt(
        FUN
        , bounds
        , initPoints = 3
        , iters.n = 6
        , errorHandling = "continue"
        , verbose = 1
      )
      , "Errors encountered in initialization are listed above."
    )

  }

)

testthat::test_that(

  "NA Return - Initialization"

  , {

    skip_on_cran()
    set.seed(10)
    sf <- function(x,y) 1000 - (x-5)^2 - (y + 10)^2

    FUN <- function(x,y) {
      return(list(Score = if (runif(1) > 0.5) sf(x,y) else NA))
    }

    bounds = list(
      x = c(0,15)
      , y = c(-20,100)
    )

    expect_error(
      bayesOpt(
        FUN
        , bounds
        , initPoints = 3
        , iters.n = 8
        , errorHandling = 2
        , verbose = 1
      )
      , "Errors encountered in initialization are listed above."
    )

  }

)

testthat::test_that(

  "1D Error Handling"

  , {

    skip_on_cran()
    set.seed(11)
    sf <- function(x) 1000 - x^2

    FUN <- function(x) {
      if (runif(1) > 0.5) stop("You foo'd when you should have bar'd.")
      return(list(Score = sf(x)))
    }

    bounds = list(
      x = c(-1000,1000)
    )

    expect_error(
      bayesOpt(
        FUN
        , bounds
        , initPoints = 3
        , iters.n = 8
        , errorHandling = 2
        , verbose = 1
      )
      , "Errors encountered in initialization are listed above."
    )

  }

)

testthat::test_that(

  "Malformed FUN Return"

  , {

    skip_on_cran()
    set.seed(11)
    sf <- function(x) 1000 - x^2

    FUN <- function(x) {
      ot <- if (runif(1) > 0.75) c(0,1) else 1
      return(list(Score = sf(x), ot = ot))
    }

    bounds = list(
      x = c(-1000,1000)
    )

    expect_error(
      bayesOpt(
        FUN
        , bounds
        , initPoints = 3
        , iters.n = 8
        , errorHandling = 2
        , verbose = 1
      )
      , "Errors encountered in initialization are listed above."
    )

  }

)
