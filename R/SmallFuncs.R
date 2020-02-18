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

minMaxScale <- function(tabl, boundsDT) {

  # tabl <- newD

  mms <- lapply(boundsDT$N, function(x) (tabl[[x]]-boundsDT[get("N")==x,]$L)/boundsDT[get("N")==x,]$R)

  setDT(mms)
  setnames(mms, boundsDT$N)
  return(mms)

}

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

zeroOneScale <- function(vec) {

  r <- max(vec) - min(vec)

  vec <- (vec - min(vec))/r

  return(vec)

}

checkDup <- function(tab1,tab2) {

  sapply(1:nrow(tab1), function(i) {
    tab2 <- rbind(tab2,tab1[0:(i-1),])
    nrow(fintersect(tab2,tab1[i,])) > 0
  })

}

boundsToDT <- function(bounds) {
  data.table(
    N = names(bounds)
    , L = sapply(bounds, function(x) x[1])
    , U = sapply(bounds, function(x) x[2])
    , R = sapply(bounds, function(x) x[2]) - sapply(bounds, function(x) x[1])
    , C = sapply(bounds, function(x) class(x))
  )
}

saveSoFar <- function(optObj,verbose) {
  if (!is.null(optObj$saveFile)) {
    tryCatch(
      {
        suppressWarnings(saveRDS(optObj, file = optObj$saveFile))
        if (verbose > 0) cat("\n   Saving Intermediary Results with ",nrow(optObj$scoreSummary)," rows to:  \n   ",optObj$saveFile,"\n")
      }
      , error = function(e) {
        if (verbose > 0) cat(red("\n === Failed to save intermediary results. Please check file path. === \n"))
      }
    )
  }
}

ParMethod <- function(x) if(x) {`%dopar%`} else {`%do%`}

getAcqInfo <- function(acq) {
  return(
    data.table(
      nam = c("ei","eips","poi","ucb")
      , disp = c("Expected Improvement","Expct. Imprvmt./Second", "Prob. of Improvement","Upper Conf. Bound")
      , base = c(0,0,0,1)
    )[get("nam")==acq,]
  )
}

checkParameters <- function(
    bounds
  , iters.n
  , iters.k
  , otherHalting
  , acq
  , acqThresh
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
}

totalTime <- function(optObj,startT) {
  optObj$elapsedTime + as.numeric(difftime(Sys.time(),startT,units = "secs"))
}

formatOtherHalting <- function(otherHalting) {
  if (is.null(otherHalting$timeLimit)) otherHalting$timeLimit <- Inf
  if (is.null(otherHalting$minUtility)) otherHalting$minUtility <- 0
  return(otherHalting)
}
