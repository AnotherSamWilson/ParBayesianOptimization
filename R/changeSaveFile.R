#' Change Save File Location
#'
#' Use this to change the saveFile parameter in a pre-existing bayesOpt object.
#' @param optObj An object of class bayesOpt
#' @param saveFile A filepath stored as a character. Must include the
#' filename and extension as a .RDS.
#' @return The same optObj with the updated saveFile.
#' @export
changeSaveFile <- function(optObj,saveFile = NULL) {

  if (class(optObj) != "bayesOpt") stop("optObj should be of class bayesOpt.")

  # See if saveFile can be written to.
  if (!is.null(saveFile)) {
    if (toupper(substr(saveFile, nchar(saveFile)-4+1, nchar(saveFile))) != ".RDS") stop("saveFile is saved as an RDS using saveRDS() - please change file extension in saveFile parameter.")
    if (file.access(saveFile,mode=2) != 0) {
      message("saveFile is not writeable according to file.access(). Continue? [y/n]")
      line <- readline()
      if (tolower(line) == "y") invisible() else stop("Process Stopped by User.")
    }
  }
  optObj$saveFile <- saveFile
  return(optObj)
}
