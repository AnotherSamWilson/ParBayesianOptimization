#' Bayesian Optimization with Gaussian Processes
#'
#' Maximizes a user defined function within a set of bounds. After the function
#' is sampled a pre-determined number of times, a Gaussian process is fit to
#' the results. An acquisition function is then maximized to determine the most
#' likely location of the global maximum of the user defined function.  This
#' process is repeated for a set number of iterations.
#'
#' @param FUN the function to be maximized. This function should return a
#'   named list with at least 1 component. The first component must be named
#'   \code{Score} and should contain the metric to be maximized. You may
#'   return other named scalar elements that you wish to include in the final
#'   summary table.
#' @param bounds named list of lower and upper bounds for each \code{FUN} input.
#'   The names of the list should be arguments passed to \code{FUN}.
#'   Use "L" suffix to indicate integers.
#' @param saveFile character filepath (including file name and extension, .RDS) that
#'   specifies the location to save results as they are obtained. A \code{bayesOpt}
#'   object is saved to the file after each epoch.
#' @param initGrid user specified points to sample the scoring function, should
#'   be a \code{data.frame} or \code{data.table} with identical column names as bounds.
#' @param initPoints Number of points to initialize the process with. Points are
#'   chosen with latin hypercube sampling within the bounds supplied.
#' @param iters.n The total number of times FUN will be run after initialization.
#' @param iters.k integer that specifies the number of times to sample FUN
#'   at each Epoch (optimization step). If running in parallel, good practice
#'   is to set \code{iters.k} to some multiple of the number of cores you have designated
#'   for this process. Must be lower than, and preferrably some multiple of \code{iters.n}.
#' @param otherHalting A list of other halting specifications. The process will stop if any of
#'   the following is true. These checks are only performed in between optimization steps:
#' \itemize{
#'   \item The elapsed seconds is greater than the list element \code{timeLimit}.
#'   \item The utility expected from the Gaussian process is less than the list element
#'   \code{minUtility}.
#' }
#' @param acq acquisition function type to be used. Can be "ucb", "ei", "eips" or "poi".
#' \itemize{
#'   \item \code{ucb}   Upper Confidence Bound
#'   \item \code{ei}    Expected Improvement
#'   \item \code{eips}  Expected Improvement Per Second
#'   \item \code{poi}   Probability of Improvement
#' }
#' @param kappa tunable parameter kappa of the upper confidence bound.
#'   Adjusts exploitation/exploration. Increasing kappa will increase the
#'   importance that uncertainty (unexplored space) has, therefore incentivising
#'   exploration. This number represents the standard deviations above 0 of your upper
#'   confidence bound. Default is 2.56, which corresponds to the ~99th percentile.
#' @param eps tunable parameter epsilon of ei, eips and poi. Adjusts exploitation/exploration.
#'   This value is added to y_max after the scaling, so should between -0.1 and 0.1.
#'   Increasing eps will make the "improvement" threshold for new points higher, therefore
#'   incentivising exploitation.
#' @param parallel should the process run in parallel? If TRUE, several criteria must be met:
#' \itemize{
#'   \item A parallel backend must be registered
#'   \item Objects required by \code{FUN} must be loaded into each cluster.
#'   \item Packages required by \code{FUN} must be loaded into each cluster. See vignettes.
#'   \item \code{FUN} must be thread safe.
#' }
#' @param gsPoints integer that specifies how many initial points to try when
#'   searching for the optimum of the acquisition function. Increase this for a higher
#'   chance to find global optimum, at the expense of more time.
#' @param convThresh convergence threshold passed to \code{factr} when the
#'   \code{optim} function (L-BFGS-B) is called. Lower values will take longer
#'   to converge, but may be more accurate.
#' @param acqThresh number 0-1. Represents the minimum percentage
#'   of the global optimal utility required for a local optimum to
#'   be included as a candidate parameter set in the next scoring function.
#'   If 1.0, only the global optimum will be used as a candidate
#'   parameter set. If 0.5, only local optimums with 50 percent of the utility
#'   of the global optimum will be used.
#' @param errorHandling If FUN returns an error, how to proceed. All errors are
#'   stored in \code{scoreSummary}. Can be one of 3 options: "stop" stops the
#'   function running and returns results. "continue" keeps the process running.
#'   Passing an integer will allow the process to continue until that many errors
#'   have occured, after which the results will be returned.
#' @param plotProgress Should the progress of the Bayesian optimization be
#'   printed? Top graph shows the score(s) obtained at each iteration.
#'   The bottom graph shows the estimated utility of each point.
#'   This is useful to display how much utility the Gaussian Process is
#'   assuming still exists. If your utility is approaching 0, then you
#'   can be confident you are close to an optimal parameter set.
#' @param verbose Whether or not to print progress to the console.
#'   If 0, nothing will be printed. If 1, progress will be printed.
#'   If 2, progress and information about new parameter-score pairs will be printed.
#' @param ... Other parameters passed to \code{DiceKriging::km()}. All FUN inputs and scores
#' are scaled from 0-1 before being passed to km. FUN inputs are scaled within \code{bounds},
#' and scores are scaled by 0 = min(scores), 1 = max(scores).
#' @return An object of class \code{bayesOpt} containing information about the process.
#' \itemize{
#'   \item \code{FUN}          The scoring function.
#'   \item \code{bounds}       The bounds originally supplied.
#'   \item \code{iters}        The total iterations that have been run.
#'   \item \code{initPars}     The initialization parameters.
#'   \item \code{optPars}      The optimization parameters.
#'   \item \code{GauProList}   A list containing information on the Gaussian Processes used in optimization.
#'   \item \code{scoreSummary} A \code{data.table} with results from the execution of \code{FUN}
#'   at different inputs. Includes information on the epoch, iteration, function inputs, score, and any other
#'   information returned by \code{FUN}.
#'   \item \code{stopStatus}   Information on what caused the function to stop running. Possible explenations are
#'   time limit, minimum utility not met, errors in \code{FUN}, iters.n was reached, or the Gaussian Process encountered
#'   an error.
#'   \item \code{elapsedTime} The total time in seconds the function was executing.
#' }
#' @references Jasper Snoek, Hugo Larochelle, Ryan P. Adams (2012) \emph{Practical Bayesian Optimization of Machine Learning Algorithms}
#'
#' @section Vignettes:
#'
#' It is highly recommended to read the \href{https://github.com/AnotherSamWilson/ParBayesianOptimization}{GitHub} for examples.
#' There are also several vignettes available from the official \href{https://CRAN.R-project.org/package=ParBayesianOptimization}{CRAN Listing}.
#'
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
#' Results <- bayesOpt(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 3
#'   , iters.n = 2
#'   , gsPoints = 10
#' )
#'
#' \dontrun{
#' # Example 2 - Hyperparameter Tuning in xgboost
#' if (requireNamespace('xgboost', quietly = TRUE)) {
#'   library("xgboost")
#'
#'   data(agaricus.train, package = "xgboost")
#'
#'   Folds <- list(
#'       Fold1 = as.integer(seq(1,nrow(agaricus.train$data),by = 3))
#'     , Fold2 = as.integer(seq(2,nrow(agaricus.train$data),by = 3))
#'     , Fold3 = as.integer(seq(3,nrow(agaricus.train$data),by = 3))
#'   )
#'
#'   scoringFunction <- function(max_depth, min_child_weight, subsample) {
#'
#'     dtrain <- xgb.DMatrix(agaricus.train$data,label = agaricus.train$label)
#'
#'     Pars <- list(
#'         booster = "gbtree"
#'       , eta = 0.01
#'       , max_depth = max_depth
#'       , min_child_weight = min_child_weight
#'       , subsample = subsample
#'       , objective = "binary:logistic"
#'       , eval_metric = "auc"
#'     )
#'
#'     xgbcv <- xgb.cv(
#'          params = Pars
#'        , data = dtrain
#'        , nround = 100
#'        , folds = Folds
#'        , prediction = TRUE
#'        , showsd = TRUE
#'        , early_stopping_rounds = 5
#'        , maximize = TRUE
#'        , verbose = 0
#'     )
#'
#'     return(
#'       list(
#'           Score = max(xgbcv$evaluation_log$test_auc_mean)
#'         , nrounds = xgbcv$best_iteration
#'       )
#'     )
#'   }
#'
#'   bounds <- list(
#'       max_depth = c(2L, 10L)
#'     , min_child_weight = c(1, 100)
#'     , subsample = c(0.25, 1)
#'   )
#'
#'   ScoreResult <- bayesOpt(
#'       FUN = scoringFunction
#'     , bounds = bounds
#'     , initPoints = 3
#'     , iters.n = 2
#'     , iters.k = 1
#'     , acq = "ei"
#'     , gsPoints = 10
#'     , parallel = FALSE
#'     , verbose = 1
#'   )
#' }
#' }
#' @importFrom data.table data.table setDT setcolorder := as.data.table copy .I setnames is.data.table rbindlist
#' @importFrom utils head tail
#' @export
bayesOpt <- function(
    FUN
  , bounds
  , saveFile = NULL
  , initGrid
  , initPoints = 4
  , iters.n = 3
  , iters.k = 1
  , otherHalting = list(timeLimit = Inf,minUtility = 0)
  , acq = "ucb"
  , kappa = 2.576
  , eps = 0.0
  , parallel = FALSE
  , gsPoints = pmax(100,length(bounds)^3)
  , convThresh = 1e8
  , acqThresh = 1.000
  , errorHandling = "stop"
  , plotProgress = FALSE
  , verbose = 1
  , ...
) {

  startT <- Sys.time()

  # Construct bayesOpt list
  optObj <- list()
  class(optObj) <- "bayesOpt"
  optObj$FUN <- FUN
  optObj$bounds <- bounds
  optObj$iters <- 0
  optObj$initPars <- list()
  optObj$optPars <- list()
  optObj$GauProList <- list()

  # See if saveFile can be written to, and store saveFile if necessary.
  optObj <- changeSaveFile(optObj,saveFile)

  # Check the parameters
  checkParameters(
      bounds
    , iters.n
    , iters.k
    , otherHalting
    , acq
    , acqThresh
    , errorHandling
    , plotProgress
    , parallel
    , verbose
  )

  # Formatting
  boundsDT <- boundsToDT(bounds)
  otherHalting <- formatOtherHalting(otherHalting)

  # Initialization Setup
  if (missing(initGrid) + missing(initPoints) != 1) stop("Please provide 1 of initGrid or initPoints, but not both.")
  if (!missing(initGrid)) {
    setDT(initGrid)
    inBounds <- checkBounds(initGrid,bounds)
    inBounds <- as.logical(apply(inBounds,1,prod))
    if (any(!inBounds)) stop("initGrid not within bounds.")
    optObj$initPars$initialSample <- "User Provided Grid"
    initPoints <- nrow(initGrid)
  } else {
    initGrid <- randParams(boundsDT, initPoints)
    optObj$initPars$initialSample <- "Latin Hypercube Sampling"
  }
  optObj$initPars$initGrid <- initGrid
  if (nrow(initGrid) <= 2) stop("Cannot initialize with less than 3 samples.")
  optObj$initPars$initPoints <- nrow(initGrid)
  if (initPoints <= length(bounds)) stop("initPoints must be greater than the number of FUN inputs.")

  # Output from FUN is sunk into a temporary file.
  sinkFile <- file()
  on.exit(
    {
      while (sink.number() > 0) sink()
      close(sinkFile)
    }
  )

  # Define processing function
  `%op%` <- ParMethod(parallel)
  if(parallel) Workers <- getDoParWorkers() else Workers <- 1

  # Run initialization
  if (verbose > 0) cat("\nRunning initial scoring function",nrow(initGrid),"times in",Workers,"thread(s)...")
  sink(file = sinkFile)
  tm <- system.time(
    scoreSummary <- foreach(
        iter = 1:nrow(initGrid)
      , .options.multicore = list(preschedule=FALSE)
      , .combine = list
      , .multicombine = TRUE
      , .inorder = FALSE
      , .errorhandling = 'pass'
      #, .packages ='data.table'
      , .verbose = FALSE
    ) %op% {

      Params <- initGrid[get("iter"),]
      Elapsed <- system.time(
        Result <- tryCatch(
          {
            do.call(what = FUN, args = as.list(Params))
          }
          , error = function(e) e
        )
      )

      # Make sure everything was returned in the correct format. Any errors here will be passed.
      if (any(class(Result) %in% c("simpleError","error","condition"))) return(Result)
      if (class(Result) != "list") stop("Object returned from FUN was not a list.")
      resLengths <- lengths(Result)
      if (!any(names(Result) == "Score")) stop("FUN must return list with element 'Score' at a minimum.")
      if (!is.numeric(Result$Score)) stop("Score returned from FUN was not numeric.")
      if(any(resLengths != 1)) {
        badReturns <- names(Result)[which(resLengths != 1)]
        stop("FUN returned these elements with length > 1: ",paste(badReturns,collapse = ","))
      }

      data.table(Params,Elapsed = Elapsed[[3]],as.data.table(Result))

    }
  )[[3]]
  while (sink.number() > 0) sink()
  if (verbose > 0) cat(" ",tm,"seconds\n")

  # Scan our list for any simpleErrors. If any exist, stop the process and return the errors.
  se <- which(sapply(scoreSummary,function(cl) any(class(cl) %in% c("simpleError","error","condition"))))
  if(length(se) > 0) {
    print(
      data.table(
          initGrid[se,]
        , errorMessage = sapply(scoreSummary[se],function(x) x$message)
      )
    )
    stop("Errors encountered in initialization are listed above.")
  } else {
    scoreSummary <- rbindlist(scoreSummary)
  }

  # Format scoreSummary table. Initial iteration is set to 0
  scoreSummary[,("gpUtility") := rep(as.numeric(NA),nrow(scoreSummary))]
  scoreSummary[,("acqOptimum") := rep(FALSE,nrow(scoreSummary))]
  scoreSummary[,("Epoch") := rep(0,nrow(scoreSummary))]
  scoreSummary[,("Iteration") := 1:nrow(scoreSummary)]
  scoreSummary[,("inBounds") := rep(TRUE,nrow(scoreSummary))]
  scoreSummary[,("errorMessage") := rep(NA,nrow(scoreSummary))]
  extraRet <- setdiff(names(scoreSummary),c("Epoch","Iteration",boundsDT$N,"inBounds","Elapsed","Score","gpUtility","acqOptimum"))
  setcolorder(scoreSummary,c("Epoch","Iteration",boundsDT$N,"gpUtility","acqOptimum","inBounds","Elapsed","Score",extraRet))

  # System.time function is not terribly precise for very small elapsed times.
  if(any(scoreSummary$Elapsed < 1) & acq == "eips") {
    cat("\n   FUN elapsed time is too low to be precise. Switching acq to 'ei'.\n")
    acq <- 'ei'
  }

  # This is the final list returned. It is updated whenever possible.
  # If an error occurs, it is returned in its latest configuration.
  optObj$optPars$acq <- acq
  optObj$optPars$kappa <- kappa
  optObj$optPars$eps <- eps
  optObj$optPars$parallel <- parallel
  optObj$optPars$gsPoints <- gsPoints
  optObj$optPars$convThresh <- convThresh
  optObj$optPars$acqThresh <- acqThresh
  optObj$scoreSummary <- scoreSummary
  optObj$GauProList$gpUpToDate <- FALSE
  optObj$iters <- nrow(scoreSummary)
  optObj$stopStatus <- "OK"
  optObj$elapsedTime <- as.numeric(difftime(Sys.time(),startT,units = "secs"))

  # Save Intermediary Output
  saveSoFar(optObj,0)

  optObj <- addIterations(
      optObj
    , otherHalting = otherHalting
    , iters.n = iters.n
    , iters.k = iters.k
    , parallel = parallel
    , plotProgress = plotProgress
    , errorHandling = errorHandling
    , saveFile = saveFile
    , verbose = verbose
    , ...
  )

  return(optObj)

}
utils::globalVariables(c("."))
