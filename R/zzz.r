#' Setting up the package dartR.popgenomics
#'
#' Setting up dartR.popgenomics
#' @importFrom crayon blue
#' @importFrom utils packageVersion combn head tail
#' @importFrom methods getPackageName is
#' @import adegenet
#' @import dartR.base
#' @import dartR.data
#' @import data.table
#' @importFrom grDevices rainbow hcl 
#' @importFrom graphics lines legend par
#' @importFrom stats dist quantile
#' @import ggplot2
#' @importFrom MASS kde2d write.matrix
#' 
#' @keywords internal


#needed to avoid error
zzz<-NULL

error <- crayon::red
warn <- crayon::yellow
report <- crayon::green
important <- crayon::blue
code <- crayon::cyan


# WELCOME MESSAGE
.onAttach <- function(...) {
  pn <- getPackageName()
  packageStartupMessage(important(
    paste(
      "**** Welcome to",pn,"[Version",
      packageVersion(pn),
      "] ****\n"
    )
  ))
}

