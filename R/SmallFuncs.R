# The functions in this file are all internal.

# Determine if a set of parameters is within the bounds.
checkBounds <- function(tab, bounds) {
  return(
      sapply(
        names(bounds)
      , function(paramName) {
        tab[[paramName]] >= bounds[[paramName]][[1]] & tab[[paramName]] <= bounds[[paramName]][[2]]
      }
    )
  )
}

# Draw random parameters with LHS
randParams <- function(boundsDT, rPoints, FAIL = TRUE) {

  # Attempt to procure rPoints unique parameter sets by lhs.
  attempt <- 1
  newPars <- data.table()
  poi <- rPoints

  while(attempt <= 100) {

    latinCube <- data.table(lhs::improvedLHS(n = poi, k = nrow(boundsDT)))

    setnames(latinCube, boundsDT$N)

    newPars <- unique(rbind(unMMScale(latinCube, boundsDT),newPars))

    if (nrow(newPars) == rPoints) break else poi <- rPoints-nrow(newPars)

    if (attempt >= 100 & FAIL) stop("Latin Hypercube Sampling could not produce the required distinct parameter sets. \nTry decreasing gsPoints or initPoints.")

    attempt <- attempt + 1

  }

  setnames(newPars, boundsDT$N)
  return(newPars)

}

# Scale parameters to 0-1 between their bounds.
minMaxScale <- function(tabl, boundsDT) {

  # tabl <- newD

  mms <- lapply(boundsDT$N, function(x) (tabl[[x]]-boundsDT[get("N")==x,]$L)/boundsDT[get("N")==x,]$R)

  setDT(mms)
  setnames(mms, boundsDT$N)
  return(mms)

}

# Do the reverse of minMaxScale
unMMScale <- function(tabl, boundsDT) {

  umms <- lapply(boundsDT$N, function(x) {

    B <- boundsDT[get("N")==x,]

    n <- tabl[[x]]*B$R+B$L

    if (B$C == "integer") n <- round(n)

    return(n)

  })

  setDT(umms)
  if(!identical(names(tabl),boundsDT$N)) umms <- cbind(umms, tabl[,-boundsDT$N, with = F])
  setnames(umms, names(tabl))
  return(umms)

}

# Scale a vector between 0-1
zeroOneScale <- function(vec) {

  r <- max(vec) - min(vec)

  # If the scoring function returned the same results
  # this results in the function a vector of 1s.
  if(r==0) stop("Results from FUN have 0 variance, cannot build GP.")

  vec <- (vec - min(vec))/r

  return(vec)

}

# Check to see if any rows from tab1 are exact duplicates of rows in tab2.
checkDup <- function(tab1,tab2) {

  sapply(1:nrow(tab1), function(i) {
    tab2 <- rbind(tab2,tab1[0:(i-1),])
    nrow(fintersect(tab2,tab1[i,])) > 0
  })

}

# Return a data.table from a bounds list. Easier to work with.
boundsToDT <- function(bounds) {
  data.table(
    N = names(bounds)
    , L = sapply(bounds, function(x) x[1])
    , U = sapply(bounds, function(x) x[2])
    , R = sapply(bounds, function(x) x[2]) - sapply(bounds, function(x) x[1])
    , C = sapply(bounds, function(x) class(x))
  )
}

# Attempt to save bayesOpt object between optimization steps.
saveSoFar <- function(optObj,verbose) {
  if (!is.null(optObj$saveFile)) {
    tryCatch(
      {
        suppressWarnings(saveRDS(optObj, file = optObj$saveFile))
        if (verbose > 0) cat("  4) Saving Intermediary Results to:  \n    ",optObj$saveFile,"\n")
      }
      , error = function(e) {
        if (verbose > 0) cat(red("  4) Failed to save intermediary results. Please check file path.\n"))
      }
    )
  }
}

# Cannot pass `%dopar%` so we recreate it with this function.
ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}

# Get information about the acquisition functions.
getAcqInfo <- function(acq) {
  return(
    data.table(
      nam = c("ei","eips","poi","ucb")
      , disp = c("Expected Improvement","Expct. Imprvmt./Second", "Prob. of Improvement","Upper Conf. Bound")
      , base = c(0,0,0,1)
    )[get("nam")==acq,]
  )
}

# Early checks for parameters.
checkParameters <- function(
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
) {
  if (iters.n < iters.k) stop("iters.n cannot be less than iters.k. See ?bayesOpt for parameter definitions.")
  if (iters.n %% 1 != 0 | iters.k %% 1 != 0) stop("iters.n and iters.k must be integers.")
  if (!any(acq == c("ucb","ei","eips","poi"))) stop("Acquisition function not recognized")
  if (parallel & (getDoParWorkers() == 1)) stop("parallel is set to TRUE but no back end is registered.\n")
  if (!parallel & getDoParWorkers() > 1 & verbose > 0) message("parallel back end is registered, but parallel is set to false. Process will not be run in parallel.")
  if (any(!names(otherHalting) %in% c("timeLimit","minUtility"))) stop("otherHalting element not recognized. Must be one of timeLimit and minUtility.")
  if (class(bounds) != "list") stop("bounds must be a list of parameter bounds with the same arguments as FUN.")
  if (any(lengths(bounds) != 2)) stop("Not all elements in bounds are length 2.")
  if (acqThresh > 1 | acqThresh < 0) stop("acqThresh must be in [0,1]")
  if (!is.logical(plotProgress)) stop("plotProgress must be logical")
  if (!errorHandling %in% c("stop","continue") & !is.numeric(errorHandling)) stop("errorHandling is malformed: Must be one of 'stop', 'continue', or an integer.")
}

# Get the total time run of an object given the time it was started.
totalTime <- function(optObj,startT) {
  optObj$elapsedTime + as.numeric(difftime(Sys.time(),startT,units = "secs"))
}

# Fill in any missing elements of otherHalting we need.
formatOtherHalting <- function(otherHalting) {
  if (is.null(otherHalting$timeLimit)) otherHalting$timeLimit <- Inf
  if (is.null(otherHalting$minUtility)) otherHalting$minUtility <- 0
  return(otherHalting)
}

# When the process stops early it will print this color.
#' @importFrom crayon make_style red
returnEarly <- crayon::make_style("#FF6200")

# Constructor for stopEarlyMsg class.
makeStopEarlyMessage <- function(msg) {
  class(msg) <- "stopEarlyMsg"
  return(msg)
}

# Multiple places the process can stop early. This just prints the message.
printStopStatus <- function(optObj,verbose) {
  if (verbose > 0) cat(returnEarly("\n",optObj$stopStatus,"\n"))
}

# Combining function for foreach. Allows the return of message without scores.
rbindFE <- function(...) rbind(...,fill=TRUE)

# What to do if FUN produced errors?
getEarlyStoppingErrorStatus <- function(NewResults,scoreSummary,errorHandling,verbose) {
  newErrors <- sum(!is.na(NewResults$errorMessage))
  allErrors <- newErrors + sum(!is.na(scoreSummary$errorMessage))
  if (errorHandling == "stop" & allErrors > 0) {
    return(makeStopEarlyMessage("Errors encountered in FUN"))
  } else if (errorHandling == "continue") {
    return("OK")
  } else if (errorHandling <= allErrors) {
    return(makeStopEarlyMessage("Errors from FUN exceeded errorHandling limit"))
  } else {
    return("OK")
  }
}













