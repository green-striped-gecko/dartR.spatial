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

# function to replicate defaults colors of ggplot
discrete_palette <- function(n) {
  hues <-seq(15, 375, length = n + 1)
  return(hcl(h = hues, l = 65, c = 100)[1:n])
}

# taken from wes_palette::Zissou1
diverging_palette <-
  colorRampPalette(c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"))
# creating convergent to 0 palette
cool <-
  rainbow(50, start = rgb2hsv(col2rgb("cyan"))[1], end = rgb2hsv(col2rgb("blue"))[1])
warm <-
  rainbow(50, start = rgb2hsv(col2rgb("red"))[1], end = rgb2hsv(col2rgb("yellow"))[1])
cols <- c(rev(cool), rev(warm))
convergent_palette <- colorRampPalette(cols)

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

