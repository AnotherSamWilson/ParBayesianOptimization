
<!-- README.md is generated from README.Rmd. Please edit that file -->
Linux/Mac: \[![Build Status](https://api.travis-ci.org/AnotherSamWilson/ParBayesianOptimization.svg)\] (<https://travis-ci.org/AnotherSamWilson/ParBayesianOptimization/>)

ParBayesianOptimization
=======================

Installation
------------

You can install ParBayesianOptimization from github with:

``` r
# install.packages("devtools")
devtools::install_github("AnotherSamWilson/ParBayesianOptimization")
```

Package Process
---------------

Machine learning projects will commonly require a user to "tune" a model's hyperparameters to find a good balance between bias and variance. Several tools are available in a data scientist's toolbox to handle this task, the most blunt of which is a grid search. A grid search gauges the model performance over a pre-defined set of hyperparameters without regard for past performance. As models increase in complexity and training time, grid searches become unwieldly.

Idealy, we would use the information from prior model evaluations to guide us in our future parameter searches. This is precisely the idea behind Bayesian Optimization, in which our prior response distribution is iteratively updated based on our best guess of where the best parameters are. The `ParBayesianOptimization` package does exactly this in the following process:

1.  Initial parameter-score pairs are found
2.  Gaussian Process is fit/updated
3.  Numerical methods are used to estimate the best parameter set
4.  New parameter-score pairs are found
5.  Repeat steps 2-4 until some stopping criteria is met

Practical Example
-----------------

In this example, we will be using the agaricus.train dataset provided in the XGBoost package. Here, we load the packages, data, and create a folds object to be used in the scoring function.

``` r
library("xgboost")
library("ParBayesianOptimization")

data(agaricus.train, package = "xgboost")

Folds <- list(Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
            , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
            , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3)))
```

Now we need to define the scoring function. This function should, at a minimum, return a list with a `Score` element, which is the model evaluation metric we want to maximize. We can also retain other pieces of information created by the scoring function by including them as named elements of the returned list. In this case, we want to retain the optimal number of rounds determined by the `xgb.cv`:

``` r
scoringFunction <- function(max_depth, min_child_weight, subsample) {

  dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
  
  Pars <- list( booster = "gbtree"
              , eta = 0.01
              , max_depth = max_depth
              , min_child_weight = min_child_weight
              , subsample = subsample
              , objective = "binary:logistic"
              , eval_metric = "auc")

  xgbcv <- xgb.cv(params = Pars
                , data = dtrain
                , nround = 100
                , folds = Folds
                , prediction = TRUE
                , showsd = TRUE
                , early_stopping_rounds = 5
                , maximize = TRUE
                , verbose = 0)

  return(list(Score = max(xgbcv$evaluation_log$test_auc_mean)
             , nrounds = xgbcv$best_iteration
             )
         )
}
```

Some other objects we need to define are the bounds, GP kernel and acquisition function.

-   The `bounds` will tell our process its search space.
-   The kernel is passed to the `GauPro` function `GauPro_kernel_model` and defines the covariance function.
-   The acquisition function defines the utility we get from using a certain parameter set.

``` r
bounds <- list( max_depth = c(2L, 10L)
              , min_child_weight = c(1L, 100L)
              , subsample = c(0.25, 1))

kern <- "Matern52"

acq <- "ei"
```

We are now ready to put this all into the `BayesianOptimization` function.

``` r
ScoreResult <- BayesianOptimization(FUN = scoringFunction
                                  , bounds = bounds
                                  , initPoints = 10
                                  , bulkNew = 1
                                  , nIters = 12
                                  , kern = kern
                                  , acq = acq
                                  , kappa = 2.576
                                  , verbose = 1
                                  , parallel = FALSE)
#> 
#> Running initial scoring function 10 times in 1 thread(s).
#> 
#> Starting round number 1
#>   1) Fitting Gaussian process...
#>   2) Running global optimum search...
#>   3) Running scoring function 1 times in 1 thread(s)...
#> 
#> Starting round number 2
#>   1) Fitting Gaussian process...
#>   2) Running global optimum search...
#>   3) Running scoring function 1 times in 1 thread(s)...
```

The console informs us that the process initialized by running `scoringFunction` 10 times. It then fit a Gaussian process to the parameter-score pairs, found the global optimum of the acquisition function, and ran `scoringFunction` again. This process continued until we had 12 parameter-score pairs. You can interrogate the `ScoreResult` object to see the results:

``` r
ScoreResult$ScoreDT
#>     Iteration max_depth min_child_weight subsample Elapsed     Score nrounds
#>  1:         0         8               96 0.5935750    0.70 0.9797757      32
#>  2:         0         4               87 0.7504342    0.16 0.9779723       1
#>  3:         0         2               76 0.9639782    0.12 0.9779723       1
#>  4:         0         8               48 0.8497707    0.22 0.9889367       3
#>  5:         0         6               17 0.3089911    0.58 0.9951817      19
#>  6:         0         9               24 0.7529264    0.67 0.9974167      23
#>  7:         0         7                1 0.3099921    0.31 0.9986540       5
#>  8:         0         4               49 0.8077531    0.33 0.9901100       8
#>  9:         0         3               35 0.9579958    0.16 0.9900173       2
#> 10:         0         3               22 0.8018554    0.36 0.9953687      15
#> 11:         1        10                1 0.8609687    0.24 0.9984757       1
#> 12:         2         8                1 0.6257481    0.42 0.9986603       8
```

``` r
ScoreResult$BestPars
#>    Iteration max_depth min_child_weight subsample     Score nrounds elapsedSecs
#> 1:         0         7                1 0.3099921 0.9986540       5      5 secs
#> 2:         1         7                1 0.3099921 0.9986540       5     13 secs
#> 3:         2         8                1 0.6257481 0.9986603       8     20 secs
```
