context('Plotting')

set.seed(1991)

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
  , iters.n = 1
  , verbose = 0
)

optObj
plot(optObj)
