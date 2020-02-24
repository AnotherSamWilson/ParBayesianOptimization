## ParBayesianOptimization 1.1.0


# Changes
Changed Gaussian Process package to DiceKriging. predict method is much faster.
Added errorHandling parameter - bayesOpt() and addIterations() should now return results no matter what, unless errorHandling = 'stop'
Added otherHalting parameter.
