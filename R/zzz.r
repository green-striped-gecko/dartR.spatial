#' Setting up the package dartR.spatial
#'
#' Setting up dartR.spatial
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

## returns NULL if the 'fbm' slot is missing OR is NULL
.fbm_or_null <- function(x) {
  if (methods::.hasSlot(x, "fbm")) {
    val <- methods::slot(x, "fbm")
    return(if (is.null(val)) NULL else val)
  }
  NULL
}

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

