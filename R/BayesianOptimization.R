#' @title Bayesian Optimization
#'
#' @description
#' Flexible Bayesian optimization of model hyperparameters.
#'
#' @param FUN the function to be maximized. This function should return a named list with at least 1 component.
#'   The first component must be named \code{Score} and should contain the metric to be maximized.
#'   You may return other named scalar elements that you wish to include in the final summary table.
#' @param bounds named list of lower and upper bounds for each hyperparameter.
#'   The names of the list should be arguments passed to \code{FUN}.
#'   Use "L" suffix to indicate integer hyperparameters.
#' @param saveIntermediate character filepath (including file name) that specifies the location to save intermediary results. This will
#'   save a data.table as an RDS that can be specified as the \code{leftOff} parameter.
#' @param leftOff data.table containing parameter-Score pairs. If supplied, the process will rbind this table
#'   to the parameter-Score pairs obtained through initialization.
#'   This table should be obtained from the file saved by \code{saveIntermediate}.
#' @param parallel should the process run in parallel? If TRUE, several criteria must be met:
#' \itemize{
#'   \item A parallel backend must be registered
#'   \item \code{FUN} must be executable using only packages specified in \code{packages} (and base packages)
#'   \item \code{FUN} must be executable using only the the objects specified in \code{export}
#'   \item The function must be thread safe.
#' }
#' @param export character vector of object names needed to evaluate \code{FUN}.
#' @param packages character vector of the packages needed to run \code{FUN}.
#' @param initialize should the process initialize a parameter-Score pair set? If \code{FALSE}, \code{leftOff} must be provided.
#' @param initGrid user specified points to sample the target function, should
#'   be a \code{data.frame} or \code{data.table} with identical column names as bounds.
#' @param initPoints number of randomly chosen points to sample the
#'   scoring function before Bayesian Optimization fitting the Gaussian Process.
#' @param bulkNew integer that specifies the number of parameter combinations to try between each Gaussian process fit.
#' @param nIters total number of parameter sets to be sampled, including initial set.
#' @param kern a character that gets mapped to one of GauPro's \code{GauPro_kernel_beta} S6 classes.
#'   Determines the covariance function used in the gaussian process. Can be one of:
#' \itemize{
#'   \item \code{"Gaussian"}
#'   \item \code{"Exponential"}
#'   \item \code{"Matern52"}
#'   \item \code{"Matern32"}
#' }
#' @param beta the kernel lengthscale parameter log10(theta). Passed to \code{GauPro_kernel_beta} specified in kern.
#' @param acq acquisition function type to be used. Can be "ucb", "ei", "eips" or "poi".
#' \itemize{
#'   \item \code{ucb}   Upper Confidence Bound
#'   \item \code{ei}    Expected Improvement
#'   \item \code{eips}  Expected Improvement Per Second
#'   \item \code{poi}   Probability of Improvement
#' }
#' @param stopImpatient a list containing \code{rounds} and \code{newAcq}, if \code{acq = "eips"} you
#'   can switch the acquisition function to \code{newAcq} after \code{rounds} parameter-score pairs are found.
#' @param kappa tunable parameter kappa of GP Upper Confidence Bound, to balance exploitation against exploration,
#'   increasing kappa will incentivise exploration.
#' @param eps tunable parameter epsilon of ei, eips and poi. Balances exploitation against exploration.
#'   Increasing eps will make the "improvement" threshold higher.
#' @param gsPoints integer that specifies how many initial points to try when searching for the optimal parameter set.
#'   Increase this for a higher chance to find global optimum, at the expense of more time.
#' @param convThresh convergence threshold passed to \code{factr} when the \code{optim} function (L-BFGS-B) is called.
#'   Lower values will take longer to converge, but may be more accurate.
#' @param minClusterUtility number 0-1. Represents the minimum percentage of the optimal utility
#'   required for a less optimal local maximum to be included as a candidate parameter
#'   set in the next scoring function. If \code{NULL}, only the global optimum will be
#'   used as a candidate parameter set.
#' @param noiseAdd if bulkNew > 1, specifies how much noise to add to the optimal candidate parameter set
#'   to obtain the other \code{bulkNew-1} candidate parameter sets. New random draws are pulled from
#'   a shape(4,4) beta distribution centered at the optimal candidate parameter set
#'   with a range equal to \code{noiseAdd*(Upper Bound - Lower Bound)}
#' @param verbose Whether or not to print progress. If 0, nothing will be printed.
#'   If 1, progress will be printed. If 2, progress and information about new parameter-score pairs will be printed.
#' @return A list containing details about the process:
#' \item{GPlist}{The list of the gaussian process objects that were fit.}
#' \item{acqMaximums}{The optimal parameters according to each gaussian process}
#' \item{ScoreDT}{A list of all parameter-score pairs, as well as extra columns from FUN}
#' \item{BestPars}{The best parameter set at each iteration}
#' @references Jasper Snoek, Hugo Larochelle, Ryan P. Adams (2012) \emph{Practical Bayesian Optimization of Machine Learning Algorithms}
#' @examples
#' \dontrun{
#' library("xgboost")
#' library("ParBayesianOptimization")
#'
#' data(agaricus.train, package = "xgboost")
#'
#' Folds <- list(  Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
#'                 , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
#'                 , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3)))
#'
#' scoringFunction <- function(max_depth, min_child_weight, subsample) {
#'
#'   dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
#'
#'   Pars <- list( booster = "gbtree"
#'                 , eta = 0.01
#'                 , max_depth = max_depth
#'                 , min_child_weight = min_child_weight
#'                 , subsample = subsample
#'                 , objective = "binary:logistic"
#'                 , eval_metric = "auc")
#'
#'   xgbcv <- xgb.cv(params = Pars,
#'                   data = dtrain
#'                   , nround = 100
#'                   , folds = Folds
#'                   , prediction = TRUE
#'                   , showsd = TRUE
#'                   , early_stopping_rounds = 5
#'                   , maximize = TRUE
#'                   , verbose = 0)
#'
#'   return(list(Score = max(xgbcv$evaluation_log$test_auc_mean)
#'               , nrounds = xgbcv$best_iteration
#'   )
#'   )
#' }
#'
#'
#' bounds <- list( max_depth = c(2L, 10L)
#'                 , min_child_weight = c(1, 100)
#'                 , subsample = c(0.25, 1))
#'
#' kern <- "Matern52"
#'
#' acq <- "ei"
#'
#' ScoreResult <- BayesianOptimization(FUN = scoringFunction
#'                                     , bounds = bounds
#'                                     , initPoints = 10
#'                                     , bulkNew = 1
#'                                     , nIters = 12
#'                                     , kern = kern
#'                                     , acq = acq
#'                                     , kappa = 2.576
#'                                     , verbose = 1
#'                                     , parallel = FALSE)
#' }
#' @importFrom data.table data.table setDT setcolorder := as.data.table copy .I
#' @importFrom utils head
#' @importFrom GauPro GauPro_kernel_model Matern52 Matern32 Exponential Gaussian
#' @export

BayesianOptimization <- function(
    FUN
  , bounds
  , saveIntermediate = NULL
  , leftOff = NULL
  , parallel = FALSE
  , packages = NULL
  , export = NULL
  , initialize = TRUE
  , initGrid = NULL
  , initPoints = 0
  , bulkNew = 1
  , nIters = 0
  , kern = "Matern52"
  , beta = 0
  , acq = "ucb"
  , stopImpatient = list(newAcq = "ucb", rounds = Inf)
  , kappa = 2.576
  , eps = 0.0
  , gsPoints = 100
  , convThresh = 1e7
  , minClusterUtility = NULL
  , noiseAdd = 0.25
  , verbose = 1
) {

  StartT <- Sys.time()

  # Set counters and other helper objects
  ParamNames <- names(bounds)
  packages <- unique(c('data.table',packages))
  setDT(initGrid)
  setDT(leftOff)
  Overwrites <- 0
  Iter <- 0
  kern <- assignKern(kern,beta)
  boundsDT <- data.table( N = ParamNames
                        , L = sapply(bounds, function(x) x[1])
                        , U = sapply(bounds, function(x) x[2])
                        , C = sapply(bounds, function(x) class(x))
  )



  # Define processing function
  ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}
  `%op%` <- ParMethod(parallel)
  Workers <- getDoParWorkers()

  # Ensure environment fidelity
  if (sum(acq == c("ucb","ei","eips","poi")) == 0) stop("Acquisition function not recognized")
  if (sum(stopImpatient$newAcq == c("ucb","ei","eips","poi")) == 0) stop("New acquisition function not recognized")
  if (!initialize & nrow(leftOff) == 0) stop("initialize cannot be FALSE if leftOff is not provided. Set initialize to TRUE and provide either initGrid or initPoints. You can provide leftOff AND initialize if you want.\n")
  if (initialize & nrow(initGrid) == 0 & initPoints <= 0) stop("initialize is TRUE but neither initGrid or initPoints were provided")
  if (initPoints > 0 & nrow(initGrid)>0) stop("initGrid and initPoints are specified, choose one.")
  if (initPoints <= 0 & nrow(initGrid)==0) stop("neither initGrid or initPoints are specified, choose one or provide leftOff")
  if (parallel & (Workers == 1)) stop("parallel is set to TRUE but no back end is registered.\n")
  if (!parallel & Workers > 1 & verbose > 0) cat("parallel back end is registered, but parallel is set to false. Process will not be run in parallel.\n")
  if (nrow(initGrid)>0) {
    if (sum(sapply(ParamNames, CheckBounds,initGrid, bounds))>0) stop("initGrid not within bounds.")
  }
  if (nrow(leftOff) > 0){
    if (sum(sapply(ParamNames, CheckBounds,leftOff, bounds))>0) stop("leftOff not within bounds.")
  }
  if (nrow(leftOff)+initialize*(initPoints+nrow(initGrid)) >= nIters) stop("Rows in initial set will be larger than nIters")
  if (verbose > 0 & bulkNew < Workers & parallel) cat("bulkNew is less than the threads registered on the parallel back end - process may not utilize all workers.\n")


  # If an initGrid was specified, make that the initial process fit. If not, create one with initPoints combinations.
  # If leftOff was specified, append to the initialized table (if applicable)
  if (initialize){

      if (nrow(initGrid)>0){
        InitFeedParams <- initGrid
      } else{
        InitFeedParams <- data.table(sapply(ParamNames,RandParams,initPoints, boundsDT))
      }

      if (verbose > 0) cat("\nRunning initial scoring function",nrow(InitFeedParams),"times in",Workers,"thread(s).\n")
      sink("NUL")
      ScoreDT <- foreach( iter = 1:nrow(InitFeedParams)
                        , .combine = rbind
                        , .multicombine = TRUE
                        , .inorder = FALSE
                        , .errorhandling = 'pass'
                        , .packages = unique(c('data.table',packages))
                        , .verbose = FALSE
                        , .export = export
                        ) %op% {

          Params <- InitFeedParams[get("iter"),]
          Elapsed <- system.time(Result <- do.call(what = FUN, args = as.list(Params)))
          data.table(Params,Elapsed = Elapsed[[3]],as.data.table(Result))

      }
      sink()

      # Append leftOff if its names match ScoreDT
      if (nrow(leftOff) > 0) {
        if (!identical(sort(c("Iteration",names(ScoreDT))),sort(names(leftOff)))) {
          if (verbose > 0) cat("\nNames from scoring function do not match leftOff table. Continuing without using leftOff table.\n")
        } else{
        ScoreDT <- rbind(ScoreDT,leftOff, fill = TRUE)
        }
      }

  } else{ScoreDT <- leftOff}


  # Format ScoreDT table. Initial iteration is set to 0, even if leftOff was provided
  ScoreDT[,("Iteration") := rep(0,nrow(ScoreDT))]
  extraRet <- setdiff(names(ScoreDT),c("Iteration",ParamNames,"Elapsed","Score"))
  setcolorder(ScoreDT,c("Iteration",ParamNames,"Elapsed","Score",extraRet))

  Time <- Sys.time()

  # Save Intermediary Output
  if (!is.null(saveIntermediate)) {
    tryCatch({suppressWarnings(saveRDS(ScoreDT, file = saveIntermediate))
      if (verbose > 0) cat("\n   Saving Intermediary Results with ",nrow(ScoreDT)," rows to \n   ",saveIntermediate,"\n   This is the first Save/Overwrite.\n")
      Overwrites <- Overwrites + 1}
      , error = function(e) {
        if (verbose > 0) {cat("\n === Failed to save intermediary results. Please check file path.\n     This message will repeat. === \n")}
      }
    )
  }

  # Setup for iterations
  GPlist <- list()
  acqMaximums <- data.table()
  scaleList <- list(score = max(abs(ScoreDT$Score)), elapsed = max(ScoreDT$Elapsed))
  GP <- NULL
  BestPars <- data.table( "Iteration" = Iter
                       , ScoreDT[which.max(get("Score")),c(ParamNames,"Score",extraRet),with = F]
                       , elapsedSecs = round(difftime(Time,StartT,units = "secs"),0)
                       )

  # Start the iterative GP udpates
  while(nrow(ScoreDT) < nIters){

    Iter <- Iter + 1

    if (verbose > 0) cat("\nStarting round number",Iter)

    # How many runs to make this session
      runNew <- pmin(nIters-nrow(ScoreDT), bulkNew)

    # Should we switch to another Acq function:
      if (acq == "eips" & stopImpatient$newAcq != "eips" & nrow(ScoreDT) >= stopImpatient$rounds) {
        if (verbose > 0) cat("\n  0) Changing acquisition function from",acq,"to",stopImpatient$newAcq)
        acq <- stopImpatient$newAcq
      }

    # Fit GP
    newD <- ScoreDT[get("Iteration") == Iter-1,]
    if (verbose > 0) cat("\n  1) Fitting Gaussian process...")
    GP <- updateGP( GP = GP
                  , kern = kern
                  , X = matrix(sapply(ParamNames, MinMaxScale, newD, boundsDT), nrow = nrow(newD))
                  , Z = matrix(as.matrix(newD[,.(get("Score")/scaleList$score,Elapsed/scaleList$elapsed)]), nrow = nrow(newD))
                  , acq = acq
                  , scaleList = scaleList
                  , parallel = parallel)

    # Store GP in list
    GPlist[[Iter]] <- GP

    # Create random points to initialize local maximum search.
    LocalTries <- data.table(sapply(ParamNames,RandParams,gsPoints,boundsDT))
    LocalTryMM <- data.table(sapply(ParamNames,MinMaxScale,LocalTries,boundsDT))

    # Try gsPoints starting points to find parameter set that optimizes Acq
    if (verbose > 0) cat("\n  2) Running local optimum search...")
    LocalOptims <- maxAcq( GP = GP
                         , TryOver = LocalTryMM
                         , acq = acq
                         , y_max = max(ScoreDT$Score/scaleList$score)
                         , kappa = kappa
                         , eps = eps
                         , ParMethod = ParMethod
                         , parallel = parallel
                         , convThresh = convThresh
                         )

    if (sum(LocalOptims$gradCount > 2) == 0) stop("\n  2a) WARNING - No initial points converged.\n      Process may just be sampling random points.\n      Try decreasing convThresh.")

    fromCluster <- applyCluster()
    acqMaximums <- rbind(acqMaximums, data.table("Iteration" = Iter, fromCluster$clusterPoints))
    newScorePars <- sapply(ParamNames, UnMMScale, fromCluster$newSet, boundsDT)

    if (runNew < 2) {
      newScorePars <- as.data.table(as.list(newScorePars))
    } else{
      newScorePars <- data.table(newScorePars)
    }

    if (verbose > 0) cat("\n  3) Running scoring function",nrow(newScorePars),"times in",Workers,"thread(s)...\n")
    sink("NUL")
    NewResults <- foreach( iter = 1:nrow(newScorePars)
                         , .combine = rbind
                         , .multicombine = TRUE
                         , .inorder = FALSE
                         , .errorhandling = 'pass'
                         , .packages = packages
                         , .verbose = FALSE
                         , .export = export
                         ) %op% {

            Params <- newScorePars[get("iter"),]
            Elapsed <- system.time(Result <- do.call(what = FUN, args = as.list(Params)))
            data.table(newScorePars[get("iter"),], Elapsed = Elapsed[[3]], as.data.table(Result))

            }
    sink()

    Time <- Sys.time()

    # Print updates on parameter-score search
    if (verbose > 1) {

      cat("\nResults from most recent parameter scoring:\n")
      print(NewResults, row.names = FALSE)

      if (max(NewResults$Score) > max(ScoreDT$Score)) {
        cat("\nNew best parameter set found:\n")
        print(NewResults[which.max(get("Score")),], row.names = FALSE)
      } else {
        cat("\nMaximum score was not raised this round. Best score is still:\n")
        print(ScoreDT[which.max(get("Score")),], row.names = FALSE)
      }
    }

    ScoreDT <- rbind(ScoreDT
                    ,data.table("Iteration" = rep(Iter,nrow(NewResults))
                               ,NewResults
                               )
                    )
    BestPars <- rbind(BestPars
                     ,data.table("Iteration" = Iter
                               , ScoreDT[which.max(get("Score")),c(ParamNames,"Score",extraRet),with = F]
                               , "elapsedSecs" = round(difftime(Time,StartT,units = "secs"),0)
                               )
                    )

    # Save Intermediary Results
    if (!is.null(saveIntermediate)) {
      tryCatch({
        suppressWarnings(saveRDS(ScoreDT, file = saveIntermediate))
        if (verbose > 0) cat("\n   Saving Intermediary Results with ",nrow(ScoreDT)," rows to:  \n   ",saveIntermediate,"\n   This Save/Overwrite number",Overwrites+1,"\n")
        Overwrites <- Overwrites + 1
      }
      , error = function(e) {
        if (verbose > 0) cat("\n === Failed to save intermediary results. Please check file path. === \n")
      }
      )
    }
  }

  RetList <- list()
  names(GPlist) <- paste0("GP_", 1:Iter)
  RetList$GPlist <- GPlist
  RetList$acqMaximums <- acqMaximums
  RetList$ScoreDT <- ScoreDT
  RetList$BestPars <- BestPars

  return(RetList)


}
utils::globalVariables(c("."))
