#' @name gl.kosman
#' @title Calculates a Kosman distance matrix for each pair of individuals
#' @family distance
#'
#' @description
#' This function calculates the Kosman and Leonard (2005) distance between
#' each pair of individuals.
#'
#' @details
#' For each locus, the distance between two individuals is the absolute
#' difference in allele dosage divided by the ploidy (0, 0.5 or 1 for diploid
#' SNPs). The Kosman distance is the mean of these values over the loci called
#' in both individuals, so missing data are handled pair by pair.
#'
#' All individuals must have the same ploidy. SilicoDArT data (ploidy 1) are
#' treated as haploid, so the distance is the proportion of shared called
#' loci at which presence/absence differs.
#'
#' Pairs of individuals with no called loci in common get a distance of NaN.
#'
#' @param x Name of the genlight object containing the SNP or SilicoDArT data,
#' with a single ploidy across individuals [required].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#'
#' @return A list with two nInd(x) x nInd(x) matrices, with individual names
#' as row and column names. Only the lower triangle and the diagonal are
#' filled; the upper triangle is NA.
#' \itemize{
#' \item kosman -- Kosman distances between individuals.
#' \item nloci -- number of loci called in both individuals, used for each
#' distance; the diagonal holds the number of loci called in that individual.
#' }
#' Use \code{as.dist(result$kosman)} to obtain a dist object.
#'
#' @author Author(s): Bernd Gruber. Custodian: Bernd Gruber -- Post to
#' \url{https://groups.google.com/d/forum/dartr}
#'
#' @references
#' \itemize{
#' \item Kosman, E., & Leonard, K. J. (2005). Similarity coefficients for
#' molecular markers in studies of genetic relationships between individuals
#' for haploid, diploid, and polyploid species. Molecular Ecology, 14(2),
#' 415-424.
#' }
#'
#' @examples
#' #use only five individuals and seven loci
#' gg <- gl.kosman(possums.gl[1:5,14:21])
#' gg$kosman
#' gg$nloci
#'
#' @export

gl.kosman <- function(x, verbose = NULL) {
  # SET VERBOSITY
  verbose <- gl.check.verbosity(verbose)

  # FLAG SCRIPT START
  funname <- match.call()[[1]]
  utils.flag.start(func = funname,
                   verbose = verbose)

  # runs with any ploidy, hence no check of datatype
  if (!inherits(x, "genlight")) {
    stop(error("Fatal Error: x must be a genlight or dartR object\n"))
  }

  # FUNCTION SPECIFIC ERROR CHECKING

  # check the ploidy to be unique across all individuals
  uniqueploidy <- unique(ploidy(x))
  if (length(uniqueploidy) != 1) {
    stop(error(
      "Fatal Error: individuals have different ploidies. Separate them by",
      "ploidy before running gl.kosman\n"
    ))
  }
  if (is.na(uniqueploidy) || uniqueploidy <= 0) {
    stop(error("Fatal Error: invalid ploidy (must be greater than 0)\n"))
  }
  ploidy <- uniqueploidy

  # DO THE JOB

  # For biallelic dosages, |a - b| is the sum over k = 1..ploidy of
  # |[a >= k] - [b >= k]|. Each term is a cross-product of indicator
  # matrices, so no nInd x nInd matrix per locus is needed.
  mat <- as.matrix(x)
  called <- !is.na(mat)
  mat[!called] <- 0
  called <- called * 1

  diffsum <- matrix(0, nInd(x), nInd(x))
  for (k in seq_len(ploidy)) {
    above <- (mat >= k) * called
    s <- tcrossprod(above, called - above)
    diffsum <- diffsum + s + t(s)
  }

  # number of loci called in both individuals
  loci.used <- tcrossprod(called)
  colnames(loci.used) <- indNames(x)
  rownames(loci.used) <- indNames(x)

  # mean distance over the loci called in both individuals
  d.fast <- diffsum / ploidy / loci.used
  diag(d.fast) <- 0
  colnames(d.fast) <- indNames(x)
  rownames(d.fast) <- indNames(x)

  noshared <- sum(loci.used[lower.tri(loci.used)] == 0)
  if (verbose >= 1 && noshared > 0) {
    cat(warn(
      "  Warning:",
      noshared,
      "pair(s) of individuals share no called loci; their distance is NaN\n"
    ))
  }

  # clean up matrices for export
  d.fast[upper.tri(d.fast, diag = FALSE)] <- NA
  loci.used[upper.tri(loci.used, diag = FALSE)] <- NA
  kosman.out <- list(kosman = d.fast, nloci = loci.used)

  # FLAG SCRIPT END

  if (verbose > 0) {
    cat(report("Completed:", funname, "\n"))
  }

  return(kosman.out)
}
