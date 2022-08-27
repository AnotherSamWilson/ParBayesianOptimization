#' Change Save File Location
#'
#' Use this to change the saveFile parameter in a pre-existing bayesOpt object.
#' @param optObj An object of class bayesOpt
#' @param saveFile A filepath stored as a character. Must include the
#' filename and extension as a .RDS.
#' @return The same \code{optObj} with the updated saveFile.
#' @examples
#' \dontrun{
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
#'   , saveFile = "filepath.RDS"
#' )
#' Results <- changeSaveFile(Results,saveFile = "DifferentFile.RDS")
#' }
#' @export
changeSaveFile <- function(optObj,saveFile = NULL) {

  if (!inherits(x = optObj, what = "bayesOpt")) stop("optObj should be of class bayesOpt.")

  # See if saveFile can be written to.
  if (!is.null(saveFile)) {
    if (toupper(substr(saveFile, nchar(saveFile)-4+1, nchar(saveFile))) != ".RDS") stop("saveFile is saved as an RDS using saveRDS() - please change file extension in saveFile parameter.")
  }
  optObj$saveFile <- saveFile
  return(optObj)
}
