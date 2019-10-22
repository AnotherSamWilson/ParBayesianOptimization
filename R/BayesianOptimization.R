#' @title Bayesian Optimization
#'
#' @description
#' Flexible Bayesian optimization of model hyperparameters.
#'
#' @param FUN the function to be maximized. This function should return a
#'   named list with at least 1 component. The first component must be named
#'   \code{Score} and should contain the metric to be maximized. You may
#'   return other named scalar elements that you wish to include in the final
#'   summary table.
#' @param bounds named list of lower and upper bounds for each hyperparameter.
#'   The names of the list should be arguments passed to \code{FUN}.
#'   Use "L" suffix to indicate integer hyperparameters.
#' @param saveIntermediate character filepath (including file name) that
#'   specifies the location to save intermediary results. This will save
#'   the ScoreDT data.table as an RDS. This RDS is saved after every
#'   iteration, and can be specified as the \code{leftOff} parameter
#'   so that you can continue a process where you left off.
#' @param leftOff data.table containing parameter-Score pairs. If supplied,
#'   the process will \code{rbind} this table to the parameter-Score pairs
#'   obtained through initialization. This table should be obtained
#'   from either the file saved by \code{saveIntermediate}, or from the ScoreDT
#'   \code{data.table} returned by this function. WARNING: any parameters
#'   not within \code{bounds} will be removed before optimization takes place.
#' @param parallel should the process run in parallel? If TRUE, several criteria must be met:
#' \itemize{
#'   \item A parallel backend must be registered
#'   \item \code{FUN} must be executable using only packages specified in \code{packages} (and base packages)
#'   \item \code{FUN} must be executable using only the the objects specified in \code{export}
#'   \item The function must be thread safe.
#' }
#' @param export character vector of object names needed to evaluate \code{FUN}.
#' @param packages character vector of the packages needed to run \code{FUN}.
#' @param initialize should the process initialize a parameter-Score pair set?
#'   If \code{FALSE}, \code{leftOff} must be provided.
#' @param initGrid user specified points to sample the scoring function, should
#'   be a \code{data.frame} or \code{data.table} with identical column names as bounds.
#' @param initPoints Number of points to initialize the process with. Points are
#'   chosen with latin hypercube sampling within the bounds supplied.
#' @param bulkNew integer that specifies the number of parameter combinations
#'   to sample at each optimization step. If \code{minClusterUtility} is \code{NULL}
#'   then noise is added to the acquisition optimum to obtain other sampling points.
#'   If running in parallel, good practice is to set \code{bulkNew} to some multiple
#'   of the number of cores you have designated for this process.
#' @param nIters total number of parameter sets to be sampled, including initial set.
#' @param kern a character that gets mapped to one of GauPro's \code{GauPro_kernel_beta}
#'   S6 classes. Determines the covariance function used in the gaussian process. Can be one of:
#' \itemize{
#'   \item \code{"Gaussian"}
#'   \item \code{"Exponential"}
#'   \item \code{"Matern52"}
#'   \item \code{"Matern32"}
#' }
#' @param beta Depreciated. The kernel lengthscale parameter log10(theta). Passed to \code{GauPro_kernel_beta} specified in kern.
#' @param acq acquisition function type to be used. Can be "ucb", "ei", "eips" or "poi".
#' \itemize{
#'   \item \code{ucb}   Upper Confidence Bound
#'   \item \code{ei}    Expected Improvement
#'   \item \code{eips}  Expected Improvement Per Second
#'   \item \code{poi}   Probability of Improvement
#' }
#' @param stopImpatient a list containing \code{rounds} and \code{newAcq},
#'   if \code{acq = "eips"} you can switch the acquisition function to \code{newAcq}
#'   after \code{rounds} parameter-score pairs are found.
#' @param kappa tunable parameter kappa of the upper confidence bound.
#'   Adjusts exploitation/exploration. Increasing kappa will increase the
#'   importance that uncertainty (unexplored space) has, therefore incentivising
#'   exploration. This number represents the standard deviations above 0 of your upper
#'   confidence bound. Default is 2.56, which corresponds to the ~99th percentile.
#' @param eps tunable parameter epsilon of ei, eips and poi. Adjusts exploitation/exploration.
#'   This value is added to y_max after the scaling, so should between -0.1 and 0.1.
#'   Increasing eps will make the "improvement" threshold for new points higher, therefore
#'   incentivising exploitation.
#' @param gsPoints integer that specifies how many initial points to try when
#'   searching for the optimum of the acquisition function. Increase this for a higher
#'   chance to find global optimum, at the expense of more time.
#' @param convThresh convergence threshold passed to \code{factr} when the
#'   \code{optim} function (L-BFGS-B) is called. Lower values will take longer
#'   to converge, but may be more accurate.
#' @param minClusterUtility number 0-1. Represents the minimum percentage
#'   of the optimal utility required for a less optimal local maximum to
#'   be included as a candidate parameter set in the next scoring function.
#'   If \code{NULL}, only the global optimum will be used as a candidate
#'   parameter set. If 0.5, only local optimums with 50 percent of the global
#'   optimum will be used.
#' @param noiseAdd specifies how much noise to add to acquisition optimums
#'   to obtain new parameter sets, if needed. New random draws are pulled
#'   from a shape(4,4) beta distribution centered at the optimal candidate
#'   parameter set with a range equal to \code{noiseAdd*(Upper Bound - Lower Bound)}
#' @param plotProgress Should the progress of the Bayesian optimization be
#'   printed? Top graph shows the score(s) obtained at each iteration.
#'   The bottom graph shows the optimal value of the acquisition function
#'   at each iteration. This is useful to display how much utility the
#'   Gaussian Process is actually assuming still exists. If your utility
#'   is approaching 0, then you can be confident you are close to an optimal
#'   parameter set.
#' @param verbose Whether or not to print progress to the console.
#'   If 0, nothing will be printed. If 1, progress will be printed.
#'   If 2, progress and information about new parameter-score pairs will be printed.
#' @return A list containing details about the process:
#' \item{GPs}{The last Gaussian process run on the parameter-score pairs}
#' \item{GPe}{If \code{acq = "eips"}, this contains the last Gaussian Process run on the parameter-elapsed time pairs}
#' \item{progressPlot}{a Plotly chart showing the evolution of the scores
#' and utility discovered during the Bayesian optimization}
#' \item{ScoreDT}{A list of all parameter-score pairs, as well as extra columns
#' from FUN. gpUtility is the acquisition function value at the time that parameter
#' set was tested. acqOptimum is a boolean column that specifies whether the parameter
#' set was an acquisition function optimum, or if it was obtained by applying noise
#' to another optimum. Elapsed is the amount of time in seconds it took FUN to
#' evaluate that parameter set.}
#' \item{BestPars}{The best parameter set at each iteration}
#' @references Jasper Snoek, Hugo Larochelle, Ryan P. Adams (2012) \emph{Practical Bayesian Optimization of Machine Learning Algorithms}
#' @examples
#' # Example 1 - Optimization of a continuous single parameter function
#' scoringFunction <- function(x) {
#'   a <- exp(-(2-x)^2)*1.5
#'   b <- exp(-(4-x)^2)*2
#'   c <- exp(-(6-x)^2)*1
#'   return(list(Score = a+b+c))
#' }
#'
#' bounds <- list(x = c(0,8))
#'
#' Results <- BayesianOptimization(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 5
#'   , nIters = 8
#'   , gsPoints = 10
#' )
#'
#' \dontrun{
#' # Example 2 - Hyperparameter Tuning in xgboost
#' library("xgboost")
#'
#' data(agaricus.train, package = "xgboost")
#'
#' Folds <- list( Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
#'              , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
#'              , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3)))
#'
#' scoringFunction <- function(max_depth, min_child_weight, subsample) {
#'
#'   dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
#'
#'   Pars <- list(
#'       booster = "gbtree"
#'     , eta = 0.01
#'     , max_depth = max_depth
#'     , min_child_weight = min_child_weight
#'     , subsample = subsample
#'     , objective = "binary:logistic"
#'     , eval_metric = "auc"
#'   )
#'
#'   xgbcv <- xgb.cv(
#'        params = Pars
#'      , data = dtrain
#'      , nround = 100
#'      , folds = Folds
#'      , prediction = TRUE
#'      , showsd = TRUE
#'      , early_stopping_rounds = 5
#'      , maximize = TRUE
#'      , verbose = 0
#'   )
#'
#'   return(list( Score = max(xgbcv$evaluation_log$test_auc_mean)
#'              , nrounds = xgbcv$best_iteration
#'   )
#'   )
#' }
#'
#' bounds <- list(
#'     max_depth = c(2L, 10L)
#'   , min_child_weight = c(1, 100)
#'   , subsample = c(0.25, 1)
#' )
#'
#' ScoreResult <- BayesianOptimization(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 5
#'   , bulkNew = 1
#'   , nIters = 7
#'   , kern = "Matern52"
#'   , acq = "ei"
#'   , verbose = 1
#'   , parallel = FALSE
#'   , gsPoints = 50
#' )
#' }
#' @importFrom data.table data.table setDT setcolorder := as.data.table copy .I setnames is.data.table
#' @importFrom utils head
#' @importFrom GauPro GauPro_kernel_model Matern52 Matern32 Exponential Gaussian
#' @importFrom crayon make_style red
#' @import plotly
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
  , beta
  , acq = "ucb"
  , stopImpatient = list(newAcq = "ucb", rounds = Inf)
  , kappa = 2.576
  , eps = 0.0
  , gsPoints = 100
  , convThresh = 1e7
  , minClusterUtility = NULL
  , noiseAdd = 0.25
  , plotProgress = TRUE
  , verbose = 1
) {

  StartT <- Sys.time()

  # Set counters and other helper objects
  mco <- list(preschedule=FALSE)
  packages <- unique(c('data.table',packages))
  if(acq %in% c("ei","poi","eips")) acqBase <- 0 else acqBase <- 1
  returnEarly <- crayon::make_style("#FF6200")
  setDT(initGrid)
  setDT(leftOff)
  Iter <- 0

  # This is the final list returned. It is updated whenever possible.
  # If an error occurs, it is returned in its latest configuration.
  RetList <- list()

  # Output from FUN is sunk into a temporary file.
  sinkFile <- file()
  on.exit(
    {
    while (sink.number() > 0) sink()
    close(sinkFile)
    }
  )

  # Make bounds data easily accessible
  boundsDT <- data.table(
    N = names(bounds)
    , L = sapply(bounds, function(x) x[1])
    , U = sapply(bounds, function(x) x[2])
    , R = sapply(bounds, function(x) x[2]) - sapply(bounds, function(x) x[1])
    , C = sapply(bounds, function(x) class(x))
  )

  # Depreciating beta:
  if(!missing(beta)) warning("beta parameter is being depreciated. This will be removed in future versions of the package.")

  # The same kernel is used throughout the entire process.
  # The kernel is updated at every iteration when the GP is recreated
  # with new 0-1 scaled Scores. This method allows for cleaner code
  # and much, MUCH faster update of the GP.
  kerns <- assignKern(kern,nrow(boundsDT))
  kerne <- assignKern(kern,nrow(boundsDT))


  # Define processing function
  ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}
  `%op%` <- ParMethod(parallel)
  if(parallel) Workers <- getDoParWorkers() else Workers <- 1


  # Better to quit gracefully than not.
  # Try to halt as early as possible, since this function can be very time intensive.
  while (sink.number() > 0) sink()
  if (sum(acq == c("ucb","ei","eips","poi")) == 0) stop("Acquisition function not recognized")
  if (sum(stopImpatient$newAcq == c("ucb","ei","eips","poi")) == 0) stop("New acquisition function not recognized")
  if (!initialize & nrow(leftOff) == 0) stop("initialize cannot be FALSE if leftOff is not provided. Set initialize to TRUE and provide either initGrid or initPoints. You can provide leftOff AND initialize if you want.\n")
  if (initialize & nrow(initGrid) == 0 & initPoints <= 0) stop("initialize is TRUE but neither initGrid or initPoints were provided")
  if (initPoints > 0 & nrow(initGrid)>0) stop("initGrid and initPoints are specified, choose one.")
  if (initPoints <= 0 & nrow(initGrid)==0 & nrow(leftOff) == 0) stop("neither initGrid or initPoints are specified, choose one or provide leftOff")
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.\n")
  if (!parallel & getDoParWorkers() > 1 & verbose > 0) cat("parallel back end is registered, but parallel is set to false. Process will not be run in parallel.\n")
  if (nrow(initGrid)>0) {
    if (sum(sapply(boundsDT$N, checkBounds,initGrid, bounds))>0) stop("initGrid not within bounds.")
  }
  if (nrow(leftOff) > 0){
    if (sum(sapply(boundsDT$N, checkBounds,leftOff, bounds))>0) stop("leftOff not within bounds.")
  }
  if (nrow(leftOff)+initialize*(initPoints+nrow(initGrid)) >= nIters) stop("Rows in initial set will be larger than nIters")
  if (verbose > 0 & bulkNew < getDoParWorkers() & parallel) cat("bulkNew is less than the threads registered on the parallel back end - process may not utilize all workers.\n")


  # If an initGrid was specified, make that the initial process fit.
  # If not, create one with initPoints combinations.
  # If leftOff was specified, append to the initialized table (if applicable)
  if (initialize){

    if (nrow(initGrid)>0){
      InitFeedParams <- initGrid
    } else{
      InitFeedParams <- randParams(boundsDT, initPoints)
    }

    if (verbose > 0) cat("\nRunning initial scoring function",nrow(InitFeedParams),"times in",Workers,"thread(s).\n")
    sink(file = sinkFile)
    ScoreDT <- foreach( iter = 1:nrow(InitFeedParams)
                        , .options.multicore = mco
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
      if(sum(names(Result) == "Score") == 0) stop("FUN must return list with element 'Score' at a minimum.")
      data.table(Params,Elapsed = Elapsed[[3]],as.data.table(Result))

    }
    while (sink.number() > 0) sink()


    if (!is.data.table(ScoreDT)) {
      cat("\nFUN failed to run with error list:\n"); print(ScoreDT)
      stop("Stopping process.")
    }

    ScoreDT[,("gpUtility") := rep(acqBase,nrow(ScoreDT))]
    ScoreDT[,("acqOptimum") := rep(FALSE,nrow(ScoreDT))]

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
  extraRet <- setdiff(names(ScoreDT),c("Iteration",boundsDT$N,"Elapsed","Score","gpUtility","acqOptimum"))
  setcolorder(ScoreDT,c("Iteration",boundsDT$N,"gpUtility","acqOptimum","Elapsed","Score",extraRet))

  Time <- Sys.time()

  # Save Intermediary Output
  if (!is.null(saveIntermediate)) {
    tryCatch(
      {suppressWarnings(saveRDS(ScoreDT, file = saveIntermediate))
      if (verbose > 0) cat("\n   Saving Intermediary Results with ",nrow(ScoreDT)," rows to \n   ",saveIntermediate,"\n")}
      , error = function(e) {
        if (verbose > 0) {cat(red("\n === Failed to save intermediary results. Please check file path.\n     This message will repeat. === \n"))}
      }
    )
  }

  # System.time function is not terribly precise for very small elapsed times. If
  # FUN takes < 1 second, ei should work fine anyway.
  if(any(ScoreDT$Elapsed < 1) & acq == "eips") {
    cat("\n   FUN elapsed time is too low to be precise. Switching acq to 'ei'.\n")
    acq <- 'ei'
  }

  # Keep track of our best parameters at each iteration.
  BestPars <- data.table( "Iteration" = Iter
                          , ScoreDT[which.max(get("Score")),c(boundsDT$N,"Score",extraRet),with = F]
                          , elapsedSecs = round(difftime(Time,StartT,units = "secs"),0)
  )

  RetList$ScoreDT <- ScoreDT
  RetList$BestPars <- BestPars

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
    if (verbose > 0) cat("\n  1) Fitting Gaussian process...")

    # Parameters are 0-1 scaled, as are the scores.
    X = matrix(as.matrix(minMaxScale(ScoreDT, boundsDT)), nrow = nrow(ScoreDT))
    Z = matrix(as.matrix(ScoreDT[,.(zeroOneScale(ScoreDT$Score),ScoreDT$Elapsed/max(ScoreDT$Elapsed))]),nrow=nrow(ScoreDT))

    GPs <- GauPro_kernel_model$new(
        X
      , matrix(Z[,1])
      , kernel = kerns
      , parallel = FALSE
      , useC = FALSE
    )
    RetList$GPs <- GPs

    if (acq == "eips") {

      GPe <- GauPro_kernel_model$new(
          X
        , matrix(Z[,2])
        , kernel = kerne
        , parallel = FALSE
        , useC = FALSE
      )

      RetList$GPe <- GPe

    } else GPe <- NULL

    # Create random points to initialize local maximum search.
    localTries <- randParams(boundsDT, gsPoints, FAIL = FALSE)
    localTryMM <- minMaxScale(localTries, boundsDT)

    # Try gsPoints starting points to find parameter set that optimizes Acq
    if (verbose > 0) cat("\n  2) Running local optimum search...")
    LocalOptims <- maxAcq(
      GPs = GPs
      , GPe = GPe
      , TryOver = localTryMM
      , acq = acq
      , y_max = 1
      , kappa = kappa
      , eps = eps
      , ParMethod = ParMethod
      , parallel = parallel
      , convThresh = convThresh
    )

    if (sum(LocalOptims$gradCount > 2) == 0) cat("\n  2a) optim function only took <3 steps.\n      Process may be sampling random points.\n      Try decreasing convThresh.")

    # Obtain parameter sets to run at next iteration.
    fromCluster <- applyCluster()
    if(any(class(fromCluster) == "character")) {
      cat(returnEarly(fromCluster))
      return(RetList)
    }

    if (verbose > 0) cat("\n  3) Running scoring function",nrow(fromCluster),"times in",Workers,"thread(s)...\n")
    sink(file = sinkFile)
    NewResults <- foreach(
      iter = 1:nrow(fromCluster)
      , .options.multicore = mco
      , .combine = rbind
      , .multicombine = TRUE
      , .inorder = FALSE
      , .errorhandling = 'pass'
      , .packages = packages
      , .verbose = FALSE
      , .export = export
    ) %op% {

      Params <- fromCluster[get("iter"),boundsDT$N,with=FALSE]
      Elapsed <- system.time(Result <- do.call(what = FUN, args = as.list(Params)))
      data.table(fromCluster[get("iter"),], Elapsed = Elapsed[[3]], as.data.table(Result))

    }
    while (sink.number() > 0) sink()


    if (!is.data.table(NewResults)) {
      cat("\nFUN failed to run with error list:\n"); print(NewResults)
      stop("Stopping process.")
    }

    Time <- Sys.time()

    # Print updates on parameter-score search
    if (verbose > 1) {

      cat("\nResults from most recent parameter scoring:\n")
      print(NewResults, row.names = FALSE)

      if (max(NewResults$Score) > max(ScoreDT$Score)) {
        cat("\nNew best parameter set found:\n")
        print(NewResults[which.max(get("Score")),c(boundsDT$N,"Score"),with=FALSE], row.names = FALSE)
      } else {
        cat("\nMaximum score was not raised this round. Best score is still:\n")
        print(ScoreDT[which.max(get("Score")),c(boundsDT$N,"Score"),with=FALSE], row.names = FALSE)
      }
    }

    # Keep track of performance.
    ScoreDT <- rbind(
      ScoreDT
      , data.table("Iteration" = rep(Iter,nrow(NewResults)),NewResults)
    )
    RetList$ScoreDT <- ScoreDT

    BestPars <- rbind(
      BestPars
      , data.table("Iteration" = Iter
                   , ScoreDT[which.max(get("Score")),c(boundsDT$N,"Score",extraRet),with = F]
                   , "elapsedSecs" = round(difftime(Time,StartT,units = "secs"),0)
      )
    )
    RetList$BestPars <- BestPars


    # Plotting
    progressPlot <- plotProg(ScoreDT,acq)
    RetList$progressPlot <- progressPlot
    if(plotProgress) print(progressPlot)


    # Save Intermediary Results
    if (!is.null(saveIntermediate)) {
      tryCatch({
        suppressWarnings(saveRDS(ScoreDT, file = saveIntermediate))
        if (verbose > 0) cat("\n   Saving Intermediary Results with ",nrow(ScoreDT)," rows to:  \n   ",saveIntermediate,"\n")
      }
      , error = function(e) {
        if (verbose > 0) cat(red("\n === Failed to save intermediary results. Please check file path. === \n"))
      }
      )
    }
  }

  return(RetList)

}
utils::globalVariables(c("."))
