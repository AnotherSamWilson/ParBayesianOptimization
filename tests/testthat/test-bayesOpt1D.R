test_that(

  "1 Input, Different Specifications"

  , {

    skip_on_cran()

    set.seed(1991)
    sf <- function(x) 100 - x^2/5
    FUN <- function(x) {
      return(list(Score = sf(x)))
    }
    bounds = list(
      x = c(-2,2)
    )
    optObj <- bayesOpt(
      FUN
      , bounds
      , initPoints = 4
      , iters.n = 2
      , verbose = 0
    )
    expect_equal(optObj$stopStatus , "OK")
    expect_equal(nrow(optObj$scoreSummary) , 6)

    # Test adding Iterations
    optObj <- addIterations(
        optObj
      , iters.n = 2
      , verbose = 0
      , gsPoints = 10
    )

    # Test adding iterations with higher iters.k and different bounds
    newBounds <- list(x=c(-2,8))
    optObj <- addIterations(
        optObj
      , bounds = newBounds
      , iters.n = 6
      , iters.k = 2
      , verbose = 0
      , gsPoints = 10
    )

    print(optObj)

    expect_equal(nrow(optObj$scoreSummary) , 14)

  }

)
