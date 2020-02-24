context('Hyperparameter Tuning')

testthat::test_that(

  "xgboost"

  , {

    skip_on_cran()
    library("xgboost")
    set.seed(0)

    data(agaricus.train, package = "xgboost")

    Folds <- list(
      Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
      , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
      , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3))
    )

    scoringFunction <- function(
        max_depth
      , max_leaves
      , min_child_weight
      , subsample
      , colsample_bytree
      , gamma
      , lambda
      , alpha
      ) {

      dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)

      Pars <- list(
        booster = "gbtree"
        , eta = 0.001
        , max_depth = max_depth
        , max_leaves = max_leaves
        , min_child_weight = min_child_weight
        , subsample = subsample
        , colsample_bytree = colsample_bytree
        , gamma = gamma
        , lambda = lambda
        , alpha = alpha
        , objective = "binary:logistic"
        , eval_metric = "auc"
      )

      xgbcv <- xgb.cv(
        params = Pars
        , data = dtrain
        , nround = 100
        , folds = Folds
        , early_stopping_rounds = 5
        , maximize = TRUE
        , verbose = 0
      )

      return(
        list(
            Score = max(xgbcv$evaluation_log$test_auc_mean)
          , nrounds = xgbcv$best_iteration
        )
      )
    }

    bounds <- list(
      max_depth = c(1L, 5L)
      , max_leaves = c(2L,25L)
      , min_child_weight = c(0, 25)
      , subsample = c(0.25, 1)
      , colsample_bytree = c(0.1,1)
      , gamma = c(0,1)
      , lambda = c(0,1)
      , alpha = c(0,1)
    )

    optObj <- bayesOpt(
      FUN = scoringFunction
      , bounds = bounds
      , initPoints = 9
      , iters.n = 4
      , iters.k = 1
      , gsPoints = 10
    )

    expect_equal(nrow(optObj$scoreSummary),13)

  }

)
