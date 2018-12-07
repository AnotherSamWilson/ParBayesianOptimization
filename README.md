
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://api.travis-ci.org/AnotherSamWilson/ParBayesianOptimization.svg)](https://travis-ci.org/AnotherSamWilson/ParBayesianOptimization)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/ParBayesianOptimization)](http://cran.r-project.org/web/packages/ParBayesianOptimization)

# ParBayesianOptimization

## Installation

You can install the most recent stable version of
ParBayesianOptimization from CRAN with:

``` r
install.packages("ParBayesianOptimization")
```

You can also install the most recent development version from github
using devtools:

``` r
# install.packages("devtools")
devtools::install_github("AnotherSamWilson/ParBayesianOptimization")
```

## Package Process

Machine learning projects will commonly require a user to “tune” a
model’s hyperparameters to find a good balance between bias and
variance. Several tools are available in a data scientist’s toolbox to
handle this task, the most blunt of which is a grid search. A grid
search gauges the model performance over a pre-defined set of
hyperparameters without regard for past performance. As models increase
in complexity and training time, grid searches become unwieldly.

Idealy, we would use the information from prior model evaluations to
guide us in our future parameter searches. This is precisely the idea
behind Bayesian Optimization, in which our prior response distribution
is iteratively updated based on our best guess of where the best
parameters are. The `ParBayesianOptimization` package does exactly this
in the following process:

1.  Initial parameter-score pairs are found  
2.  Gaussian Process is fit/updated
3.  Numerical methods are used to estimate the best parameter set  
4.  New parameter-score pairs are found  
5.  Repeat steps 2-4 until some stopping criteria is met

## Bayesian Optimization Intuition

As an example, let’s say we are only tuning 1 hyperparameter in an
xgboost model, min\_child weight within the bounds \[0,1\]. We have
initialized the process by randomly sampling the scoring function 6
times, and get the following results:

| min\_child\_weight | Score |
| -----------------: | ----: |
|              0.628 | 0.713 |
|              0.327 | 0.865 |
|              0.748 | 0.681 |
|              0.242 | 1.000 |
|              0.072 | 0.130 |
|              0.157 | 0.573 |

In this example, Score can be generalized to any error metric that we
want to *maximize* (negative RMSE, AUC, etc.). Given these scores, how
do we go about determining the best min\_child\_weight to try next? As
it turns out, Gaussian processes can give us a very good definition for
our prior distribution. Fitting a Gaussian process to the data above
(indexed by min\_child\_weight), we can see the expected value of Score
accross our parameter bounds, as well as the uncertainty at different
points:

![](vignettes/GPpredictions.png)<!-- -->

Before we can select our next candidate parameter to run the scoring
function on, we need to determine how we define a “good” parameter
inside this prior distribution. This is done by maximizing different
functions within the Gaussian process. There are several functions to
choose from:

  - Upper Confidence Bound (ucb)
  - Probability Of Improvement (poi)
  - Expected Improvement (ei)
  - Expected Improvement Per Second (eips)

Continuing the example, we select to find the min\_child\_weight which
maximizes the expected improvement according to the Gaussian process. As
you can see, there are several good candidates:

![](vignettes/expectedImprovement.png)<!-- -->

An advanced feature of ParBayesianOptimization, which you can read about
in the vignette advancedFeatures, describes how to use the
`minClusterUtility` parameter to search over the different local
maximums shown above. If not specified, only the global maximum would be
sampled.

## Practical Example

In this example, we will be using the agaricus.train dataset provided in
the XGBoost package. Here, we load the packages, data, and create a
folds object to be used in the scoring function.

``` r
library("xgboost")
library("ParBayesianOptimization")

data(agaricus.train, package = "xgboost")

Folds <- list(Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
            , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
            , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3)))
```

Now we need to define the scoring function. This function should, at a
minimum, return a list with a `Score` element, which is the model
evaluation metric we want to maximize. We can also retain other pieces
of information created by the scoring function by including them as
named elements of the returned list. In this case, we want to retain the
optimal number of rounds determined by the `xgb.cv`:

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

Some other objects we need to define are the bounds, GP kernel and
acquisition function.

  - The `bounds` will tell our process its search space.
  - The kernel is passed to the `GauPro` function `GauPro_kernel_model`
    and defines the covariance function.
  - The acquisition function defines the utility we get from using a
    certain parameter set.

<!-- end list -->

``` r
bounds <- list( max_depth = c(2L, 10L)
              , min_child_weight = c(1L, 100L)
              , subsample = c(0.25, 1))

kern <- "Matern52"

acq <- "ei"
```

We are now ready to put this all into the `BayesianOptimization`
function.

``` r
tNoPar <- system.time(
  ScoreResult <- BayesianOptimization(
      FUN = scoringFunction
    , bounds = bounds
    , initPoints = 8
    , bulkNew = 1
    , nIters = 10
    , kern = kern
    , acq = acq
    , kappa = 2.576
    , verbose = 1
    , parallel = FALSE)
)
#> 
#> Running initial scoring function 8 times in 1 thread(s).
#> 
#> Starting round number 1
#>   1) Fitting Gaussian process...
#>   2) Running local optimum search...
#>   3) Running scoring function 1 times in 1 thread(s)...
#> 
#> Starting round number 2
#>   1) Fitting Gaussian process...
#>   2) Running local optimum search...
#>   3) Running scoring function 1 times in 1 thread(s)...
```

The console informs us that the process initialized by running
`scoringFunction` 8 times. It then fit a Gaussian process to the
parameter-score pairs, found the global optimum of the acquisition
function, and ran `scoringFunction` again. This process continued until
we had 10 parameter-score pairs. You can interrogate the `ScoreResult`
object to see the results:

``` r
ScoreResult$ScoreDT
#>     Iteration max_depth min_child_weight subsample Elapsed     Score nrounds
#>  1:         0         2               96 0.4588379    0.25 0.9751867      12
#>  2:         0         6               86 0.4818961    0.42 0.9772520      20
#>  3:         0         4               90 0.7149172    0.14 0.9779723       1
#>  4:         0         3               29 0.9639938    0.74 0.9968463      40
#>  5:         0         8               25 0.7375800    0.24 0.9954127       2
#>  6:         0         9               73 0.9711172    0.20 0.9871477       4
#>  7:         0         5               43 0.7303876    0.34 0.9901100       8
#>  8:         0         7               57 0.8553421    0.28 0.9881150       6
#>  9:         1        10                1 1.0000000    0.20 0.9984757       1
#> 10:         2         2                1 1.0000000    0.18 0.9871587       8
```

``` r
ScoreResult$BestPars
#>    Iteration max_depth min_child_weight subsample     Score nrounds elapsedSecs
#> 1:         0         3               29 0.9639938 0.9968463      40      3 secs
#> 2:         1        10                1 1.0000000 0.9984757       1      9 secs
#> 3:         2        10                1 1.0000000 0.9984757       1     13 secs
```

## Running In Parallel

The process that the package uses to run in parallel is explained above.
Actually setting the process up to run in parallel is relatively simple,
we only need to define two additional parameters in the
`BayesianOptimization` function, `export` and `packages`:

``` r
exp <- c('agaricus.train','Folds')
pac <- c('xgboost')
```

We also must register a parallel backend, which we do using the
`doParallel` package. The `bulkNew` parameter is set to 2 to make full
use of the registered cores:

``` r
library(doParallel)
#> Loading required package: iterators
#> Loading required package: parallel
cl <- makeCluster(2)
registerDoParallel(cl)

tWithPar <- system.time(
  ScoreResult <- BayesianOptimization(
      FUN = scoringFunction
    , bounds = bounds
    , initPoints = 8
    , bulkNew = 2
    , nIters = 10
    , kern = kern
    , acq = acq
    , kappa = 2.576
    , parallel = TRUE
    , export = exp
    , packages = pac
    , verbose = 0)
)
stopCluster(cl)
registerDoSEQ()
```

We managed to cut the process time in half by running the process on 2
cores in parallel:

``` r
tWithPar
#>    user  system elapsed 
#>    1.03    0.07    6.96
tNoPar
#>    user  system elapsed 
#>   14.53    2.63   13.29
```
