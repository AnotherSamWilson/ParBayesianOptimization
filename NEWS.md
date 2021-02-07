## ParBayesianOptimization 1.2.4
### Changes  
Fixed a small bug that allowed duplicates to make their way into the candidate table.

## ParBayesianOptimization 1.2.3
### Changes  
Some suggested packages are now used conditionally in vignettes, reade, tests and examples since they might not be available on all checking machines.


## ParBayesianOptimization 1.2.2
### Changes  
Removed Plotly from dependencies.


## ParBayesianOptimization 1.2.1  
### Changes  
Fixed a bug with initgrid on scoring functions with dimensionality over 4.

## ParBayesianOptimization 1.2.0

### Changes
Improved the way error handling works - any errors encountered in initialization will be returned.

## ParBayesianOptimization 1.1.0
### Changes
Changed Gaussian Process package to DiceKriging. predict method is much faster.
Added errorHandling parameter - bayesOpt() and addIterations() should now return results no matter what, unless errorHandling = 'stop'
Added otherHalting parameter.
