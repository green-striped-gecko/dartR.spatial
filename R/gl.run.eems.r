#' Runs the EEMS algorithm  (Estimating Effective Migration Surfaces) on a genlight object.
#'
#' @description
#' This function runs the EEMS algorithm on a genlight object. The EEMS algorithm
#' is a spatially explicit model that estimates effective migration surfaces
#' (EEMS) from genetic data. The EEMS algorithm is implemented in C++, hence it 
#' is necessary to have the binary downloaded and the function need to point to 
#' this file via the path specified in eems.path. The binary is call runeems_snps[.exe] 
#' and can be downloaded from the github site of dartR.
#' 
#' @param x Genlight object [required].
#' @param eems.path Path to the EEMS binary (runeems_snps[.exe]) [default = "./"].
#' @param buffer Buffer size in meters around the sampling locations [default = 10000].
#' @param nDemes Number of demes to use in the EEMS model [default = 500].
#' @param diploid Logical indicating whether the data is diploid [default = TRUE].
#' @param numMCMCIter Number of MCMC iterations to run [default = 10000].
#' @param numBurnIter Number of burn-in iterations to run [default = 2000].
#' @param numThinIter Number of thinning iterations to run [default = 9].
#' @param seed Random seed for reproducibility [default = NULL].
#' @param out.dir Directory to save the output files [default = NULL].
#' @param plot.dir Directory in which to save output and intermediate files [default = tempdir()]
#' @param plot.file Name for the RDS binary file to save the list of plots (base name only, exclude extension) [default eems]
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' progress log ; 3, progress and results summary; 5, full report
#' [default 2 or as specified using gl.set.verbosity].
#' @param cleanup Logical indicating whether to delete intermediate files after
#' @param ... Additional arguments to be passed to the EEMS algorithm.

#' @return A list of contour plots of migration and diversity rates as well as 
#' several diagnostic plots It is a good idea to examine all these figures, 
#' which is why they are generated by default. Please check the examples how to 
#' customize the figrues
#' \describe{
#'  \item{mrates01}{effective migration surface. This contour plot visualizes the estimated effective migration rates \code{m}, on the log10 scale after mean centering.}
#'  \item{mrates02}{posterior probability contours \code{P(log(m) > 0) = p} and \code{P(log(m) < 0) = p} for the given probability level \code{p}. Since migration rates are visualized on the log10 scale after mean centering, 0 corresponds to the overall mean migration rate. This contour plot emphasizes regions with effective migration that is significantly higher/lower than the overall average.}
#'  \item{qrates01}{effective diversity surface. This contour plot visualizes the estimated effective diversity rates \code{q}, on the log10 scale after mean centering.}
#'  \item{qrates02}{posterior probability contours \code{P(log(q) > 0) = p} and \code{P(log(q) < 0) = p}. Similar to \code{mrates02} but applied to the effective diversity rates.}
#'  \item{rdist01}{scatter plot of the observed vs the fitted between-deme component of genetic dissimilarity, where one point represents a pair of sampled demes.}
#'  \item{rdist01}{scatter plot of the observed vs the fitted within-deme component of genetic dissimilarity, where one point represents a sampled deme.}
#'  \item{rdist03}{scatter plot of observed genetic dissimilarities between demes vs observed geographic distances between demes.}
#'  \item{pilogl01}{posterior probability trace}
#' }
#' @export
#' @importFrom grDevices chull
#' @importFrom utils write.table
#' @importFrom dismo Mercator
#' @importFrom stats runif
#' @author Bernd Gruber  & Robyn (bugs? Post to 
#' \url{https://groups.google.com/d/forum/dartr})
#' @references
#' Petkova D (2024). _reemsplots2: Generate plots to inspect and visualize the results
#' of EEMS_. R package version 0.1.0, <https://github.com/dipetkov/eems>.
#' D Petkova, J Novembre, M Stephens. Visualizing spatial population structure
#' with estimated effective migration surfaces. Nature Genetics 48, 94 -- 100 (2016). 
#' \url{http://dx.doi.org/10.1038/ng.3464}.

#' @examples
#'  \dontrun{
#'  #this examples needs a binary (runeems_snps[.exe]) specific to your operating 
#'  #system  to run
#'  eems <- gl.run.eems(bandicoot.gl, eems.path = "d:/downloads/eems/")
#'  print(eems[[1]])
#'  }
#'

gl.run.eems <- function(x, eems.path= "./",buffer=10000, nDemes =500, diploid=TRUE,numMCMCIter=10000, numBurnIter=2000, numThinIter=9,seed=NULL, out.dir = NULL, plot.dir=NULL, plot.file=NULL, verbose=NULL, cleanup=TRUE, ...) {

#util function to calculate similarities
bed2diffs_v2 <- function(Geno) {
  nIndiv <- nrow(Geno)
  nSites <- ncol(Geno)
  Miss <- is.na(Geno)
  ## Impute NAs with the column means (= twice the allele frequencies)
  Mean <- matrix(colMeans(Geno, na.rm = TRUE), ## a row of means
                 nrow = nIndiv, ncol = nSites, byrow = TRUE) ## a matrix with nIndiv identical rows of means
  Mean[Miss == 0] <- 0 ## Set the means that correspond to observed genotypes to 0
  Geno[Miss == 1] <- 0 ## Set the missing genotypes to 0 (used to be NA) 
  Geno <- Geno + Mean
  ## Compute similarities
  Sim <- Geno %*% t(Geno) / nSites
  SelfSim <- diag(Sim) ## self-similarities
  vector1s <- rep(1, nIndiv) ## vector of 1s
  ## This chunk generates a `diffs` matrix
  Diffs <- SelfSim %*% t(vector1s) + vector1s %*% t(SelfSim) - 2 * Sim
  Diffs
}

# CHECK IF PACKAGES ARE INSTALLED
pkg <- "reemsplots2"
if (!(requireNamespace(pkg, quietly = TRUE))) {
  cat(error(
    "Package",
    pkg,
    " needed for this function to work. Please install it using: \n
    install_github('dipetkov/reemsplots2')" ))
  return(-1)
}
pkg <- "sf"
if (!(requireNamespace(pkg, quietly = TRUE))) {
  cat(error(
    "Package",
    pkg,
    " needed for this function to work. Please install it using: \n
    install_github('dipetkov/reemsplots2')" ))
  return(-1)
}


pkg <- "dismo"
if (!(requireNamespace(pkg, quietly = TRUE))) {
  cat(error(
    "Package",
    pkg,
    " needed for this function to work. Please install it.\n"
  ))
  return(-1) } else {
  
 
  # SET VERBOSITY
  verbose <- gl.check.verbosity(verbose)
  
  # SET WORKING DIRECTORY
  plot.dir <- gl.check.wd(plot.dir,verbose=0)
  
  #out.dir
  
  if (is.null(out.dir)) out.dir<- tempdir()
  
  # FLAG SCRIPT START
  funname <- match.call()[[1]]
  utils.flag.start(func = funname,
                   build = "v.2023.2",
                   verbose = verbose)
  
  # CHECK DATATYPE
  if (!is.null(x)){
    dt <- utils.check.datatype(x, verbose = 0)
  }
  
  # FUNCTION SPECIFIC ERROR CHECKING
  if (is.null(plot.file))     plot.file <- "eems"

  # DO THE JOB
  # create dissimilarity matrix
  D <- bed2diffs_v2(as.matrix(x))

  

  
  
  
  
# Write dissimilarity matrix
write.table(D, file.path(tempdir(), paste0(plot.file,".diffs")), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)


write.table(x = matrix(c(
  paste0("datapath = ",file.path(tempdir(), plot.file)),
  paste0("mcmcpath = ",file.path(tempdir(),paste0("data_", plot.file))),
  paste0("nIndiv = ", nInd(x)),
  paste0("nSites = ", nLoc(x)),
  paste0("nDemes = ", nDemes),
  paste0("diploid = ", diploid),
  paste0("numMCMCIter = ", format(numMCMCIter, scientific = FALSE)),
  paste0("numBurnIter = ", format(numBurnIter, scientific = FALSE)),
  paste0("numThinIter = ", format(numThinIter, scientific = FALSE))),
  nrow = 9, 
  ncol = 1), 
  file = file.path(tempdir(),paste0("param_", plot.file, ".ini")), 
  row.names = FALSE, 
  quote = FALSE, 
  col.names = FALSE)





#### create datapath file (outer polygon)

ll <- data.frame(x=x@other$latlon$lon, y=x@other$latlon$lat)
xy <- Mercator(ll)
plot(xy)
hpts <- chull(xy)
hpts <- c(hpts, hpts[1])
poly <- xy[hpts, ]


p = sf::st_polygon(list(as.matrix(poly)))
pbuf = sf::st_buffer(p, buffer)
plot(pbuf, axes=TRUE, border="green", lwd=2)
plot(p,add=TRUE,col="red")
points(xy, pch=20, col="blue") 

pxy <- sf::st_coordinates(pbuf)[,1:2]


# Write outer

  write.table(x = pxy, 
              quote = FALSE,
              file = file.path(tempdir(), paste0(plot.file,".outer")), 
              row.names = FALSE, 
              col.names = FALSE)
  
#write coordinates
  
  write.table(x = xy, 
              quote = FALSE,
              file =file.path(tempdir(), paste0(plot.file,".coord")), 
              row.names = FALSE, 
              col.names = FALSE)
  
  if (is.null(seed)) seed <-round(runif(1,1,1000000))
  if (Sys.info()["sysname"] == "Windows") {
    prog <- "runeems_snps.exe"
    cmd <- paste0("runeems_snps.exe --params ",paste0("param_", plot.file, ".ini"), paste0(" --seed ",seed ))
  }
  
  if (Sys.info()["sysname"] == "Linux")  {
    prog <- "runeems_snps"
    cmd <- paste0("./runeems_snps --params ",paste0("param_", plot.file, ".ini"), paste0(" --seed ",seed ))
  }
  
  if (Sys.info()["sysname"] == "Darwin") {
    prog <- "runeems_snps"
    cmd <- paste0("./runeems_snps --params ",paste0("param_", plot.file, ".ini"), paste0(" --seed ",seed ))
  }
  
  # check if file program can be found
  if (file.exists(file.path(eems.path, prog))) {
    ff<- file.copy(file.path(eems.path, prog),
              to = tempdir(),
              overwrite = TRUE)
  } else {
    cat(
      error(
        "  Cannot find",
        prog,
        "in the specified folder given by eems.path:",
        eems.path,
        "\n"
      )
    )
    stop()
  }
  

  
  # change into tempdir (run it there)
  old.path <- getwd()
  setwd(tempdir())
  on.exit(setwd(old.path))
### run eems
  if (Sys.info()["sysname"] == "Linux") system("chmod +x runeems_snps")
  #cmd <- paste0(paste0(prog,"  --params ",  paste0("param_", plot.file, ".ini "), paste0("--seed ",seed )))
  
  system(cmd)
  
  
  eems_results <- file.path(tempdir(), paste0("data_", plot.file))
  eems_files <- list.files(tempdir(), pattern=plot.file) 
  if (out.dir != tempdir()) {
    file.copy(eems_results, to = out.dir, overwrite = TRUE, recursive = TRUE)
    file.copy(eems_files, to = out.dir, overwrite = TRUE)
  }
 
  
  
  
  p8 <- reemsplots2::make_eems_plots(mcmcpath =  eems_results, longlat = TRUE,...)
  
  if (cleanup) {
    unlink(eems_results, recursive =TRUE)
    unlink(eems_files, recursive =TRUE )
  }
  
  # Optionally save the plot ---------------------
  
  if (!is.null(plot.file)) {
    tmp <- utils.plot.save(p8,
                           dir = plot.dir,
                           file = plot.file,
                           verbose = verbose
    )
  }
  return(p8) 
  
  } 
}
