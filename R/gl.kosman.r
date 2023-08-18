#' Calculates a Kosman distance matrix for each pair of individuals
#'
#' This script calculates an individual based distance matrix.
#' @param  x genlight/dartR object with a unique ploidy
#' @param  verbose verbosity of the function.
#' @return returns a matrix of [dimensions nInd(x) x nInd(x)] of kosman distances between individuals, 
#' @export
#' @examples
#' #use only five individuals and seven loci
#' gg <- gl.kosman(possums.gl[1:5,14:21])
#' gg$kosman
#' gg$nloci



gl.kosman <- function(x, verbose=NULL){
  # SET VERBOSITY
  verbose <- gl.check.verbosity(verbose)
  
  # FLAG SCRIPT START
  funname <- match.call()[[1]]
  utils.flag.start(func = funname,
                   build = "Jody",
                   verbose = verbose)
  
  #runs with any ploidy, hence no check of datatype
  if (!is(x,"genlight")) stop("No valid genlight/dartR object provided!!")
  
  # getting the number of loci
  n <- nLoc(x)
  
  # check the ploidy to be unique across all individuals..
  uniqueploidy<-unique(x@ploidy)
  if(length(uniqueploidy)==1){
    ploidy<-uniqueploidy
  } else if(uniqueploidy>1){
    message("Your data set has multiple ploidies, please separate loci by ploidy")
    stop("Script stopped!")
    
  } else if(uniqueploidy<=0){
    stop("Your dataset has an invalid ploidy (ploidy<=0). Script stopped!")
    
  }
  
  # this calculates the manhattan distance between each individual and adjusts for ploidy..
  matrices <- apply(as.matrix(x),2, function(l) as.matrix(dist(cbind(l, 2-l), "manhattan")/(2*ploidy)), simplify = F)
  
  # if the value is missing, mark it with a 1, if it is real, mark it 0
  missing <- lapply(matrices, function(m) ifelse(is.na(m), 1, 0))
  
  # This is going to replace missing numbers with 0. 
  replaced <- lapply(matrices, function(m) ifelse(is.na(m),0,m))
  
  
  loci.used<-(n-Reduce("+", missing))
  colnames(loci.used) <- indNames(x)
  rownames(loci.used) <- indNames(x)
  
  # This sums the values across lists and then divides by the number of loci compared less loci with missing numbers  
  d.fast<-(Reduce("+", replaced)/loci.used)
  colnames(d.fast) <- indNames(x)
  rownames(d.fast) <- indNames(x)
  
  # clean up matrices for export
  d.fast[upper.tri(d.fast, diag = FALSE)] <- NA
  loci.used[upper.tri(loci.used, diag = FALSE)] <- NA
  kosman.out <- list(kosman = d.fast, nloci = loci.used)
  return(kosman.out)
}
