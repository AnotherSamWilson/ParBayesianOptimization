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
#'   for this process. Must belower than, and preferrably some multiple of \code{iters.n}.
#' @param kern A character string representing one of \code{GauPro_kernel_beta}
#'   S6 classes. Determines the covariance function used in the gaussian process. Can be one of:
#' \itemize{
#'   \item \code{"Gaussian"}
#'   \item \code{"Exponential"}
#'   \item \code{"Matern52"}
#'   \item \code{"Matern32"}
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
#' @param plotProgress Should the progress of the Bayesian optimization be
#'   printed? Top graph shows the score(s) obtained at each iteration.
#'   The bottom graph shows the estimated utility of each point.
#'   This is useful to display how much utility the Gaussian Process is
#'   assuming still exists. If your utility is approaching 0, then you
#'   can be confident you are close to an optimal parameter set.
#' @param verbose Whether or not to print progress to the console.
#'   If 0, nothing will be printed. If 1, progress will be printed.
#'   If 2, progress and information about new parameter-score pairs will be printed.
#' @return A \code{bayesOpt} object, containing information about the process.
#' @references Jasper Snoek, Hugo Larochelle, Ryan P. Adams (2012) \emph{Practical Bayesian Optimization of Machine Learning Algorithms}
#' @section Vignettes
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
#' ScoreResult <- bayesOpt(
#'     FUN = scoringFunction
#'   , bounds = bounds
#'   , initPoints = 3
#'   , iters.n = 2
#'   , iters.k = 1
#'   , kern = "Matern52"
#'   , acq = "ei"
#'   , gsPoints = 10
#'   , parallel = FALSE
#'   , verbose = 1
#' )
#' }
#' @importFrom data.table data.table setDT setcolorder := as.data.table copy .I setnames is.data.table
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
  , kern = "Matern52"
  , acq = "ucb"
  , kappa = 2.576
  , eps = 0.0
  , parallel = FALSE
  , gsPoints = pmax(100,length(bounds)^3)
  , convThresh = 1e8
  , acqThresh = 1.000
  , plotProgress = TRUE
  , verbose = 1
) {

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
    , kern
    , acq
    , acqThresh
    , plotProgress
  )

  # Make bounds data easily accessible
  boundsDT <- boundsToDT(bounds)

  # Initialization Setup
  if (missing(initGrid) + missing(initPoints) != 1) stop("Please provide 1 of initGrid or initPoints, but not both.")
  if (!missing(initGrid)) {
    setDT(initGrid)
    inBounds <- checkBounds(initGrid,bounds)
    inBounds <- as.logical(apply(inBounds,1,prod))
    if (any(!inBounds)) stop("initGrid not within bounds.")
    optObj$initPars$initialSample <- "User Provided Grid"
  } else {
    initGrid <- randParams(boundsDT, initPoints)
    optObj$initPars$initialSample <- "Latin Hypercube Sampling"
  }
  optObj$initPars$initGrid <- initGrid
  if (nrow(initGrid) <= 2) stop("Cannot initialize with less than 3 samples.")
  optObj$initPars$initPoints <- nrow(initGrid)

  # Output from FUN is sunk into a temporary file.
  sinkFile <- file()
  on.exit(
    {
      while (sink.number() > 0) sink()
      close(sinkFile)
    }
  )

  # The same kernel is used throughout the entire process.
  # The kernel is updated at every iteration when the GP is recreated
  # with new 0-1 scaled Scores. This method allows for cleaner code
  # and much, MUCH faster update of the GP.
  optObj$GauProList$scoreKernel <- assignKern(kern,nrow(boundsDT))
  if (acq == "eips") optObj$GauProList$timeKernel <- assignKern(kern,nrow(boundsDT))


  # Define processing function
  `%op%` <- ParMethod(parallel)
  if(parallel) Workers <- getDoParWorkers() else Workers <- 1


  # Better to quit gracefully than not.
  # Try to halt as early as possible, since this function can be very time intensive.
  while (sink.number() > 0) sink()
  if (!any(acq == c("ucb","ei","eips","poi"))) stop("Acquisition function not recognized")
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.\n")
  if (!parallel & getDoParWorkers() > 1 & verbose > 0) cat("parallel back end is registered, but parallel is set to false. Process will not be run in parallel.\n")
  if (verbose > 0 & iters.k < getDoParWorkers() & parallel) cat("iters.k is less than the threads registered on the parallel back end - process may not utilize all workers.\n")

  # Run initialization
  if (verbose > 0) cat("\nRunning initial scoring function",nrow(initGrid),"times in",Workers,"thread(s)...")
  sink(file = sinkFile)
  tm <- system.time(
    scoreSummary <- foreach(
        iter = 1:nrow(initGrid)
      , .options.multicore = list(preschedule=FALSE)
      , .combine = rbind
      , .multicombine = TRUE
      , .inorder = FALSE
      , .errorhandling = 'pass'
      #, .packages ='data.table'
      , .verbose = FALSE
    ) %op% {

      Params <- initGrid[get("iter"),]
      Elapsed <- system.time(Result <- do.call(what = FUN, args = as.list(Params)))
      if(!any(names(Result) == "Score")) stop("FUN must return list with element 'Score' at a minimum.")
      data.table(Params,Elapsed = Elapsed[[3]],as.data.table(Result))

    }
  )[[3]]
  while (sink.number() > 0) sink()
  if (verbose > 0) cat(" ",tm,"seconds\n")

  # foreach passes errors as a vector.
  if (!is.data.table(scoreSummary)) {
    stop(paste0("FUN failed to run on initial try. First error returned was <<",scoreSummary[[1]],">>"))
  }

  # Format scoreSummary table. Initial iteration is set to 0
  scoreSummary[,("gpUtility") := rep(as.numeric(NA),nrow(scoreSummary))]
  scoreSummary[,("acqOptimum") := rep(FALSE,nrow(scoreSummary))]
  scoreSummary[,("Epoch") := rep(0,nrow(scoreSummary))]
  scoreSummary[,("Iteration") := 1:nrow(scoreSummary)]
  scoreSummary[,("inBounds") := rep(TRUE,nrow(scoreSummary))]
  extraRet <- setdiff(names(scoreSummary),c("Epoch","Iteration",boundsDT$N,"inBounds","Elapsed","Score","gpUtility","acqOptimum"))
  setcolorder(scoreSummary,c("Epoch","Iteration",boundsDT$N,"gpUtility","acqOptimum","inBounds","Elapsed","Score",extraRet))

  # System.time function is not terribly precise for very small elapsed times.
  if(any(scoreSummary$Elapsed < 1) & acq == "eips") {
    cat("\n   FUN elapsed time is too low to be precise. Switching acq to 'ei'.\n")
    acq <- 'ei'
  }

  # This is the final list returned. It is updated whenever possible.
  # If an error occurs, it is returned in its latest configuration.
  optObj$optPars$kern <- kern
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

  # Save Intermediary Output
  saveSoFar(optObj,verbose)

  optObj <- addIterations(
      optObj
    , iters.n = iters.n
    , iters.k = iters.k
    , parallel = parallel
    , plotProgress = plotProgress
    , saveFile = saveFile
    , verbose = verbose
  )

  return(optObj)

}
utils::globalVariables(c("."))
