context('1 Dimension')

set.seed(1991)

sf <- function(x) 100 - x^2

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

expect_true(optObj$stopStatus == "OK")
expect_true(nrow(optObj$scoreSummary) == 6)

optObj <- addIterations(
    optObj
  , iters.n = 2
  , verbose = 0
  , gsPoints = 10
)

optObj <- addIterations(
    optObj
  , iters.n = 2
  , iters.k = 2
  , verbose = 0
  , gsPoints = 10
)

expect_true(nrow(optObj$scoreSummary) == 10)
