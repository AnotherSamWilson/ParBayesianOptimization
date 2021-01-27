## ParBayesianOptimization 1.2.2
### Changes  
Removed Plotly from dependencies - this package was causing a warning in check because it was not being used.


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
