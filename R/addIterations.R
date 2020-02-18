#' Run Additional Optimization Iterations
#'
#' Use this function to continue optimization of a bayesOpt object.
#'
#' By default, this function uses the original parameters used to create
#' \code{optObj}, however the parameters (including the bounds) can be customized.
#' If new bounds are used which cause some of the prior runs to fall outside of
#' the bounds, these samples are removed from the optimization procedure, but
#' will remain in \code{scoreSummary}. \code{FUN} should return the same elements
#' and accept the same inputs as the original, or this function may fail.
#'
#' @param optObj an object of class \code{bayesOpt}.
#' @param iters.n The total number of additional times to sample the scoring function.
#' @param iters.k integer that specifies the number of times to sample FUN
#'   at each Epoch (optimization step). If running in parallel, good practice
#'   is to set \code{iters.k} to some multiple of the number of cores you have designated
#'   for this process. Must belower than, and preferrably some multiple of \code{iters.n}.
#' @param otherHalting Same as bayesOpt()
#' @param bounds Same as bayesOpt()
#' @param acq Same as bayesOpt()
#' @param kappa Same as bayesOpt()
#' @param eps Same as bayesOpt()
#' @param gsPoints Same as bayesOpt()
#' @param convThresh Same as bayesOpt()
#' @param acqThresh Same as bayesOpt()
#' @param saveFile Same as bayesOpt()
#' @param parallel Same as bayesOpt()
#' @param plotProgress Same as bayesOpt()
#' @param verbose Same as bayesOpt()
#' @param ... Same as bayesOpt()
#' @importFrom crayon make_style red
#' @return A \code{bayesOpt} object.
#' @export
addIterations <- function(
      optObj
    , iters.n = 1
    , iters.k = 1
    , otherHalting = list(timeLimit = Inf,minUtility = 0)
    , bounds = optObj$bounds
    , acq = optObj$optPars$acq
    , kappa = optObj$optPars$kappa
    , eps = optObj$optPars$eps
    , gsPoints = optObj$optPars$gsPoints
    , convThresh = optObj$optPars$convThresh
    , acqThresh = optObj$optPars$acqThresh
    , saveFile = optObj$saveFile
    , parallel = FALSE
    , plotProgress = TRUE
    , verbose = 1
    , ...
) {

  startT <- Sys.time()
  if(class(optObj) != "bayesOpt") stop("optObj must be of class bayesOpt")

  # Check the parameters
  checkParameters(
      bounds
    , iters.n
    , iters.k
    , otherHalting
    , acq
    , acqThresh
    , plotProgress
    , parallel
    , verbose
  )

  optObj <- changeSaveFile(optObj,saveFile)
  otherHalting <- formatOtherHalting(otherHalting)

  # Set up for iterations
  FUN <- optObj$FUN
  boundsDT <- boundsToDT(optObj$bounds)
  scoreSummary <- optObj$scoreSummary
  Epoch <- max(scoreSummary$Epoch)
  `%op%` <- ParMethod(parallel)
  if(parallel) Workers <- getDoParWorkers() else Workers <- 1
  iters.s <- nrow(scoreSummary)
  iters.t <- iters.n + iters.s
  returnEarly <- crayon::make_style("#FF6200")

  # Store information we know about the different acquisition functions:
  # Display name
  # Base - upper conf bound will always be over 1, unless there was convergence issue.
  # For the sake of simplicity, ucb is subtracted by 1 to keep the utility on the same scale
  # It is more easily described as the 'potential' left in the search this way.
  acqN <- getAcqInfo(acq)

  # Check if bounds supplied can be used with prior parameter-score pairs.
    inBounds <- checkBounds(optObj$scoreSummary,bounds)
    scoreSummary$inBounds <- as.logical(apply(inBounds,1,prod))
    if (any(!scoreSummary$inBounds)) {
      message(
          "Bounds have been tightened. There are "
        , sum(!scoreSummary$inBounds)
        , " parameter pairs in scoreSummary which cannot"
        , " be used with the defined bounds. These will be"
        , " ignored this round. Continue? [y/n]"
      )
      line <- readline()
      if (tolower(line) == "y") invisible() else stop("Process Stopped by User.")
    }
    if (nrow(scoreSummary) <= 2) stop("Not enough samples in scoreSummary to perform optimizations.")

  # Output from FUN is sunk into a temporary file.
  sinkFile <- file()
  on.exit(
    {
      while (sink.number() > 0) sink()
      close(sinkFile)
    }
  )

  # Start the iterative GP udpates.
  while(nrow(scoreSummary) < iters.t){

    Epoch <- Epoch + 1

    if (verbose > 0) cat("\nStarting Epoch",Epoch)

    # How many runs to make this session
    runNew <- pmin(iters.t-nrow(scoreSummary), iters.k)

    # Fit GP
    if (verbose > 0) cat("\n  1) Fitting Gaussian Process...")
    optObj <- updateGP(optObj,bounds = bounds, verbose = 0, ...)

    # Find local optimums of the acquisition function
    if (verbose > 0) cat("\n  2) Running local optimum search...")
    tm <- system.time(
      LocalOptims <- getLocalOptimums(
          optObj
        , parallel=parallel
        , verbose=verbose
      )
    )[[3]]
    if (verbose > 0) cat("       ",tm,"seconds")

    # Should we continue?
    if (otherHalting$minUtility > max(LocalOptims$gpUtility)) {
      optObj$stopStatus <- paste0("Could not meet minimum required (",otherHalting$minUtility,") utility.")
      optObj$elapsedTime <- totalTime(optObj,startT)
      return(optObj)
    } else if (otherHalting$timeLimit < totalTime(optObj,startT)) {
      optObj$stopStatus <- paste0("Time Limit - ",otherHalting$timeLimit," seconds.")
      optObj$elapsedTime <- totalTime(optObj,startT)
      return(optObj)
    }

    # Filter out local optimums to our specifications
    # Obtain new candidates if we don't have enough
    nextPars <- getNextParameters(
        LocalOptims
      , boundsDT
      , scoreSummary
      , runNew
      , acq
      , kappa
      , eps
      , acqThresh
      , acqN
      , scoreGP = optObj$GauProList$scoreGP
      , timeGP = optObj$GauProList$timeGP
    )
    if(any(class(nextPars) == "stopEarlyMsg")) {
      cat(returnEarly(nextPars))
      optObj$stopStatus <- paste0("Error in getNextParameters: ",nextPars)
      optObj$elapsedTime <- totalTime(optObj,startT)
      return(optObj)
    }

    # Try to run the scoring function. If not all (but at least 1) new runs fail,
    # then foreach cannot call rbind correctly, and an error is thrown.
    if (verbose > 0) cat("\n  3) Running FUN",nrow(nextPars),"times in",Workers,"thread(s)...")
    sink(file = sinkFile)
    tm <- system.time(
      NewResults <- tryCatch(
        {
          foreach(
            iter = 1:nrow(nextPars)
            , .options.multicore = list(preschedule=FALSE)
            , .combine = rbind
            , .multicombine = TRUE
            , .inorder = FALSE
            , .errorhandling = 'pass'
            #, .packages = packages
            , .verbose = FALSE
            #, .export = export
          ) %op% {

            Params <- nextPars[get("iter"),boundsDT$N,with=FALSE]
            Elapsed <- system.time(Result <- do.call(what = FUN, args = as.list(Params)))
            return(data.table(nextPars[get("iter"),], Elapsed = Elapsed[[3]], as.data.table(Result)))

          }
        }
        , error = function(e) e
      )
    )[[3]]
    while (sink.number() > 0) sink()

    if (verbose > 0) cat(" ",tm,"seconds\n")

    # Check for errors.
    if (!is.data.table(NewResults)) {

      if (class(NewResults) %in% c("simpleError","Error","condition")) {
        er <- conditionMessage(NewResults)
        if (verbose > 0) {
          cat(returnEarly("\nAn error occured in FUN:",er))
          cat(returnEarly(" If this error is about inconsist column counts, it may mean that FUN failed in at least 1, but not all runs."))
          cat(returnEarly(" If you can verify that this is not an error in FUN, please submit an issue to: https://github.com/AnotherSamWilson/ParBayesianOptimization/issues."))
          cat(returnEarly(" Returning results so far."))
        }
        optObj$stopStatus <- paste0("Error in FUN: ",er)
        optObj$elapsedTime <- totalTime(optObj,startT)
        return(optObj)
      } else if(class(NewResults) == "matrix") {
        # foreach returns a matrix of errors if running FUN >1 times.
        cat(returnEarly("\nFUN returned all errors: ",NewResults[1,]$message,"."),sep = "")
        optObj$stopStatus <- paste0("Error in FUN: ",NewResults[1,]$message)
        optObj$elapsedTime <- totalTime(optObj,startT)
        return(optObj)
      }

    }

    # Print updates on parameter-score search
    if (verbose > 1) {

      cat("\nResults from most recent parameter scoring:\n")
      print(NewResults, row.names = FALSE)

      if (max(NewResults$Score) > max(scoreSummary$Score)) {
        cat("\nNew best parameter set found:\n")
        print(NewResults[which.max(get("Score")),c(boundsDT$N,"Score"),with=FALSE], row.names = FALSE)
      } else {
        cat("\nMaximum score was not raised this round. Best score is still:\n")
        print(scoreSummary[which.max(get("Score")),c(boundsDT$N,"Score"),with=FALSE], row.names = FALSE)
      }
    }

    # Keep track of performance.
    scoreSummary <- rbind(
      scoreSummary
      , data.table(
        "Epoch" = rep(Epoch,nrow(NewResults))
        , "Iteration" = 1:nrow(NewResults) + nrow(scoreSummary)
        , "inBounds" = rep(TRUE,nrow(NewResults))
        , NewResults
      )
    )
    optObj$scoreSummary <- scoreSummary
    optObj$GauProList$gpUpToDate <- FALSE

    # Save Intermediary Results
    saveSoFar(optObj,verbose)

    # Plotting
    if(plotProgress) plot(optObj)

  }

  optObj$stopStatus <- "OK"

  return(optObj)

}
