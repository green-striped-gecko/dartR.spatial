#' @name gl.spatial.autoCorr
#' @title Spatial autocorrelation following Smouse and Peakall 1999
#'
#' @description  Global spatial autocorrelation is a multivariate approach
#' combining all loci into a single analysis. The autocorrelation coefficient
#'  "r" is calculated for each pair of individuals in each specified distance
#'  class. For more information see Smouse and Peakall 1999, Peakall et al. 2003
#'   and Smouse et al. 2008.
#'
#' @details This function executes a modified version
#'  of \code{spautocorr} from the package \code{PopGenReport}. Differently 
#' from \code{PopGenReport}, this function also computes the 95\% confidence 
#' intervals around the r via bootstraps, the 95% confidence interval around the 
#' null hypothesis of no spatial structure and the one-tail test via permutation, 
#' and the correction factor described by Peakall et al 2003.
#' 
#' The input can be i) a genlight object (which has to have the latlon slot 
#' populated), ii) a pair of \code{Dgeo} and \code{Dgen}, which have to be
#'  either
#'  \code{matrix} or \code{dist} objects, or iii) a \code{list} of the 
#'  \code{matrix} or \code{dist} objects if the 
#'  analysis needs to be carried out for multiple populations (in this case, 
#'  all the elements of the \code{list} have to be of the same class (i.e. 
#'  \code{matrix} or \code{dist}) and the population order in the two lists has 
#'  to be the same. 
#' 
#' If the input is a genlight object, the function calculates the linear 
#' distance
#' for \code{Dgeo} and the relevant \code{Dgen} matrix (see \code{Dgen_method}) 
#' for each population. 
#' Genetic distances are used as distances, so that positive
#' autocorrelation coefficients indicate more related individuals, as in
#' GenAlEx. The Euclidean distance is squared, as in Smouse and Peakall (1999)
#' and GenAlEx. Similarity matrices are converted to distances: 'propShared'
#' as \code{1 - Dgen} and 'grm' (a relationship matrix G) as
#' \code{G[i,i] + G[j,j] - 2 * G[i,j]}. If the user provides the matrices,
#' \code{Dgen} must be a distance: a similarity matrix will generate negative
#' values for closely related individuals.
#'
#' With \code{coordinates = "latlon"} (or a lat/lon data.frame), geographic
#' distances are geodesic distances in metres (package terra). With 'xy' or an
#' x/y data.frame, they are Euclidean distances in the coordinate units.
#'
#' If \code{max(Dgeo)>1000} (e.g. the geographic distances are in thousands of
#' metres), the distance class labels in the plot are divided by 1000 (in the
#' example before these would then become km) to facilitate readability. The
#' returned table keeps the original units.
#'
#' If \code{bins} is of length = 1 it is interpreted as the number of (even)
#' bins to use. In this case the first class starts at the minimum value in
#' the distance matrix, the classes have equal width, and the last ends at the
#' maximum. If it is a numeric vector 
#' of length>1, it is interpreted as the breaking points. In this case, the 
#' first has to be the lowest value, and the last has to be the highest. There 
#' are no internal checks for this and it is user responsibility to ensure that
#' distance classes are properly set up. If that is not the case, data that fall
#' outside the range provided will be dropped. The number of bins will be 
#' \code{length(bins) - 1}.
#'
#' The permutation constructs the 95\% confidence intervals around the null
#' hypothesis of no spatial structure (this is a two-tail test). The same data
#' are also used to calculate the probability of the one-tail test as
#' (number of permutations at least as extreme + 1) / (reps + 1) (See
#' references below for details).
#'
#' Bootstrap calculations are skipped and \code{NA} is returned when the number 
#' of possible combinations given the sample size of any given distance class is
#' < \code{reps}.
#' 
#' Methods available to calculate genetic distances for SNP data:
#' \itemize{
#' \item "propShared" using the function \code{\link{gl.propShared}}.
#' \item "grm" using the function \code{gl.grm2}.
#' \item "Euclidean" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Simple" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Absolute" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Manhattan" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' }
#' 
#' Methods available to calculate genetic distances for SilicoDArT data:
#' \itemize{
#' \item "Euclidean" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Simple" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Jaccard" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' \item "Bray-Curtis" using the function \code{\link[dartR.base]{gl.dist.ind}}.
#' }
#'
#' Examples of other themes that can be used can be consulted in \itemize{
#'  \item \url{https://ggplot2.tidyverse.org/reference/ggtheme.html} and \item
#'  \url{https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/}
#'  }
#'  
#' @param x Name of the genlight object containing the SNP or SilicoDArT data.
#' If provided, Dgen and Dgeo are ignored [default NULL].
#' @param Dgen Genetic distance matrix, dist object or list of them (one per
#' population) if no genlight object is provided [default NULL].
#' @param Dgeo Geographic distance matrix, dist object or list of them (one per
#' population, in the same order as Dgen; list names are used as population
#' names) if no genlight object is provided.
#'  This is typically an Euclidean distance but it can be any meaningful 
#'  (geographical) distance metrics [default NULL].
#' @param coordinates Can be either 'latlon', 'xy' or a two column data.frame
#' with column names 'lat','lon', 'x', 'y')  Coordinates are provided via
#' \code{gl@other$latlon} ['latlon'] or via \code{gl@other$xy} ['xy']. For
#' latlon, geodesic distances in metres are calculated; for xy, Euclidean
#' distances are calculated on the coordinates [default "latlon"].
#' @param Dgen_method Method to calculate genetic distances. See details
#'  [default "Euclidean"].
#' @param Dgeo_trans Transformation to be used on the geographic distances. See
#' Dgen_trans [default "Dgeo"].
#' @param Dgen_trans You can provide a formula to transform the genetic
#' distance. The transformation can be applied as a formula using Dgen as the
#'  variable to be transformed. For example: \code{Dgen_trans = 'Dgen/(1-Dgen)'.
#'   Any valid R expression can be used here 
#'   [default 'Dgen', which is the identity function.]}
#' @param bins The number of bins for the distance classes
#' (i.e. \code{length(bins) == 1)} or a vectors with the break points. See 
#' details [default 5].
#' @param reps The number to be used for permutation and bootstrap analyses
#' [default 100].
#' @param plot.pops.together Plot all the populations in one plot. Confidence 
#' intervals from permutations are not shown [default FALSE].
#' @param permutation Whether permutation calculations for the null hypothesis 
#' of no spatial structure should be carried out [default TRUE].
#' @param bootstrap Whether bootstrap calculations to compute the 95\% 
#' confidence intervals around r should be carried out [default TRUE].
#' @param plot.theme Theme for the plot. See details [default theme_dartR()].
#' @param plot.colors.pop A color palette for populations or a list with
#' as many colors as there are populations in the dataset [default NULL].
#' @param CI.color Color for the shade of the 95\% confidence intervals around 
#' the r estimates [default "red"].
#' @param plot.out Specify if plot is to be produced [default TRUE].
#' @param plot.dir Directory in which to save files [default = working directory]
#' @param plot.file Name for the RDS binary file to save (base name only,
#' exclude extension) [default NULL].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#'
#' @return Returns a list with one data frame per population (named by
#' population), each with the following columns:
#' \enumerate{
#' \item Bin  The distance classes
#' \item N The number of pairwise comparisons within each distance class
#' \item r.uc The uncorrected autocorrelation coefficient
#' \item Correction the correction
#' \item r The corrected autocorrelation coefficient
#' \item L.r The corrected autocorrelation coefficient lower limit
#' (if \code{bootstap = TRUE})
#' \item U.r The corrected autocorrelation coefficient upper limit
#' (if \code{bootstap = TRUE})
#' \item L.r.null.uc The uncorrected lower limit for the null hypothesis of no 
#' spatial autocorrelation (if \code{permutation = TRUE})
#' \item U.r.null.uc  The uncorrected upper limit for the null hypothesis of no 
#' spatial autocorrelation (if \code{permutation = TRUE})
#' \item L.r.null The corrected lower limit for the null hypothesis of no 
#' spatial autocorrelation (if \code{permutation = TRUE})
#' \item U.r.null The corrected upper limit for the null hypothesis of no 
#' spatial autocorrelation (if \code{permutation = TRUE})
#' \item p.one.tail The p value of the one tail statistical test
#' }
#'  Plots and table are saved plot.file in plot.dir if specified.
#'  Bootstraps and permutations (if requested) are saved in a temporary directory 
#'
#' @author Author(s): Carlo Pacioni, Bernd Gruber & Luis Mijangos. Custodian:
#' Carlo Pacioni -- Post to \url{https://groups.google.com/d/forum/dartr}
#' @references
#' \itemize{
#' \item Smouse PE, Peakall R. 1999. Spatial autocorrelation analysis of
#' individual multiallele and multilocus genetic structure. Heredity 82:
#' 561-573.
#' \item Double, MC, et al. 2005. Dispersal, philopatry and infidelity: 
#' dissecting local genetic structure in superb fairy-wrens (Malurus cyaneus). 
#' Evolution 59, 625-635.
#' \item Peakall, R, et al. 2003. Spatial autocorrelation analysis offers new
#' insights into gene flow in the Australian bush rat, Rattus fuscipes.
#' Evolution 57, 1182-1195.
#' \item Smouse, PE, et al. 2008. A heterogeneity test for fine-scale genetic
#' structure. Molecular Ecology 17, 3389-3400.
#' \item Gonzales, E, et al. 2010. The impact of landscape disturbance on 
#' spatial genetic structure in the Guanacaste tree, Enterolobium
#' cyclocarpum (Fabaceae). Journal of Heredity 101, 133-143.
#' \item Beck, N, et al. 2008. Social constraint and an absence of sex-biased
#' dispersal drive fine-scale genetic structure in white-winged choughs.
#' Molecular Ecology 17, 4346-4358.
#' }
#' @examples
#' \donttest{
#' require("dartR.data")
#' res <- gl.spatial.autoCorr(platypus.gl, bins=seq(0,10000,2000))
#' # using one population, showing sample size
#' test <- gl.keep.pop(platypus.gl,pop.list = "TENTERFIELD")
#' res <- gl.spatial.autoCorr(test, bins=seq(0,10000,2000),CI.color = "green")
#' }
#' @family spatial analysis functions
#' @importFrom tidyr pivot_wider
#' @export

gl.spatial.autoCorr <- function(x = NULL,
                                Dgeo = NULL,
                                Dgen = NULL,
                                coordinates = "latlon", 
                                Dgen_method = "Euclidean",
                                Dgeo_trans = "Dgeo",
                                Dgen_trans = "Dgen",
                                bins = 5,
                                reps = 100,
                                plot.pops.together = FALSE,
                                permutation = TRUE,
                                bootstrap = TRUE,
                                plot.theme = theme_dartR(),
                                plot.colors.pop = NULL,
                                CI.color = "red",
                                plot.out = TRUE,
                                plot.file=NULL,
                                plot.dir=NULL,
                                verbose = NULL) {
  
  # SET VERBOSITY
  verbose <- gl.check.verbosity(verbose)
  
  # SET WORKING DIRECTORY
  plot.dir <- gl.check.wd(plot.dir,verbose=0)
  
  # FLAG SCRIPT START
  funname <- match.call()[[1]]
  utils.flag.start(func = funname,
                   verbose = verbose)
  
  # CHECK DATATYPE
  if (!is.null(x)) {
    dt <- utils.check.datatype(x, verbose = 0)
  }
  
  # specific error checks
  if (!is.numeric(bins)) {
    stop(error(" The argument 'bins' should be a numeric vector\n"))
  }
  
  if (is(x, "genlight")) {
    if ((!is.null(Dgen) | !is.null(Dgeo)) & verbose >= 1) {
      cat(warn(
        "  Warning: a genlight object was provided, so Dgeo and Dgen are ignored.\n"
      ))
    }
    if (verbose > 0)
      cat(report("  Analysis performed on the genlight object.\n"))
    ta <-"genlight"
  } else {
    if (is.null(Dgen) | is.null(Dgeo)) {
      stop(error(
        " Provide either a genlight object (x) or both Dgeo and Dgen.\n"
      ))
    }
    if (verbose > 0)
      cat(
        report(
          "  Analysis performed using provided genetic and Euclidean distance matrices.\n"
        )
      )
    ta <-"dgendgeo"
  }
  
  # geodesic distances need terra
  lonlat.input <- ta == "genlight" &&
    ((is.character(coordinates) && coordinates == "latlon") ||
       (is.data.frame(coordinates) &&
          all(c("lat", "lon") %in% colnames(coordinates))))
  if (lonlat.input) {
    pkg <- "terra"
    if (!(requireNamespace(pkg, quietly = TRUE))) {
      stop(error(
        "Package",
        pkg,
        " needed for this function to work. Please install it.\n"
      ))
    }
  }
  
  # avoid global binding error
  Bin <-
    r <-
    L.r <-
    U.r <- L.r.null <- U.r.null <- Freq <- Var1 <- NULL
  
  # DO THE JOB #
  
  #### if a genlight object is provided ####
  if (ta == "genlight") {
    
    pop_list <- seppop(x)
    
    # rows of a coordinates data.frame follow the individuals of x
    if (is(coordinates, "data.frame")) {
      if (nrow(coordinates) != nInd(x)) {
        stop(error(
          "The coordinates data.frame must have one row per individual.\n"
        ))
      }
      coord_rows <- split(seq_len(nInd(x)), pop(x))
    }
    
    Dgen_list <- list()
    Dgeo_list <- list()
    
    for (i in seq_along(pop_list)) {
      
      if (verbose > 2) {
        cat(
          report(paste("  Analysing population",names(pop_list[i]),"\n")
          )
        )
      }
      
      x_temp <- pop_list[[i]]
      
      # check coordinates (if no Dgen and Dgeo is provided)
      coords <- NULL
      lonlat <- FALSE
      if (is(coordinates, "character")) {
        if (coordinates == "latlon") {
          if (is.null(x_temp@other$latlon))
            stop(error(
              "Cannot find coordinates in x@other$latlon"
            ))
          coords <- x_temp@other$latlon[, c("lon", "lat")]
          lonlat <- TRUE
          coordstring <-"x@other$latlon (geodesic distances)"
        }
        
        if (coordinates == "xy") {
          if (is.null(x_temp@other$xy))
            stop(error("Cannot find coordinates in x@other$xy"))
          coords <- x_temp@other$xy
          coordstring <-"x@other$xy"
        }
      }
      
      if (is(coordinates, "data.frame")) {
        if (length(setdiff(colnames(coordinates), c("lat", "lon"))) == 0) {
          coords <- coordinates[coord_rows[[names(pop_list)[i]]], c("lon", "lat")]
          lonlat <- TRUE
          coordstring <-"data.frame lat/lon (geodesic distances)"
        }
        
        if (length(setdiff(colnames(coordinates), c("x", "y"))) == 0) {
          coords <- coordinates[coord_rows[[names(pop_list)[i]]], c("x", "y")]
          coordstring <-"data.frame x/y"
        }
        
        if (is.null(coords)) {
          stop(
            error(
              "No valid coordinates provided. Check the provided data.frame and its format.\n"
            )
          )
        }
      }
      
      if (is.null(coords)) {
        stop(error("No valid coordinates provided!\n"))
      }
      
      # make sure coordinates have the correct length
      if (nrow(coords) != nInd(x_temp) & ta == "genlight") {
        stop(error(
          "Cannot find coordinates for each individual in slot @other$latlon.\n"
        ))
      }
      
      if (nInd(x_temp) > 1) {
        if (lonlat) {
          Dgeo <- as.dist(as.matrix(terra::distance(as.matrix(coords),
                                                    lonlat = TRUE)))
        } else {
          Dgeo <- dist(coords)
        }
      } else {
        stop(
          error(
            "Less than 2 individuals provided, therefore no pairwise distances can be calculated.\n"
          )
        )
      }
      
      # calculate genetic distances; similarities are converted to distances
      # so that positive r indicates more related individuals, as in GenAlEx
      if (Dgen_method == "propShared") {
        Dgen <- 1 - gl.propShared(x_temp)
      } else {
        if (Dgen_method == "grm") {
          G <- as.matrix(gl.grm2(x_temp, plotheatmap=FALSE, verbose = 0))
          # squared distance implied by a relationship matrix
          Dgen <- outer(diag(G), diag(G), "+") - 2 * G
        } else {
          Dgen <- as.matrix(gl.dist.ind(x_temp, method = Dgen_method,
                                        verbose = 0))
          # Smouse and Peakall (1999) and GenAlEx use squared distances
          if (tolower(Dgen_method) == "euclidean") {
            Dgen <- Dgen ^ 2
          }
        }
      }
      
      distance <- Dgen_method
      
      # convert matrices to distance objects
      Dgen_list[[i]] <- as.dist(Dgen)
      Dgeo_list[[i]] <- as.dist(Dgeo)
    } # Close for(i in 1:length(pop_list))
    pop.names <- popNames(x)
  } # close if a genlight object is provided
  
  
  #### if distances are provided ####
  if (ta == "dgendgeo") {
    chk.D <- function(D, name.D) {
      if(!(is(D, "dist") | is.matrix(D)))
        stop(error(paste0(" ", name.D, 
                          " is neither a list, a matrix nor a distance\n")))
      if(is.matrix(D)) D <- as.dist(D)
      return(list(D))
    }
    
    if(!is(Dgeo, "list")) {
      Dgeo_list <- chk.D(D=Dgeo, name.D = "Dgeo")
    } else {
      Dgeo_list <- Dgeo
    }
    if(!is(Dgen, "list")) {
      Dgen_list <- chk.D(D=Dgen, name.D = "Dgen")
    } else {
      Dgen_list <- Dgen
    }
    
    # now distances are a list
    if(length(Dgeo_list) != length(Dgen_list)) 
      stop(error( " The arguments Dgen and Dgeo should be of same length\n"))
    
    if(is.null(names(Dgeo_list))) {
      pop.names <- paste0("Pop", seq_along(Dgen_list))
    } else {
      pop.names <- names(Dgeo_list)
    }
    
    chk.D.list <- function(D.list, name.D) {
      # first class only: a matrix has class c("matrix", "array")
      classes <- vapply(D.list, function(D) class(D)[1], character(1))
      if(length(unique(classes)) != 1) {
        stop(error(paste0(" ", name.D, 
                          " is a list, but its elements are of different classes. These should be either all matrices or distances\n")))
      }
      
      if(!(is(D.list[[1]], "dist") | is.matrix(D.list[[1]]))) {
        stop(error(paste0(" ", name.D, 
                          " is a list, but its element are neither all matrices nor distances\n")))
      }
      
      if(is.matrix(D.list[[1]])) D.list <- lapply(D.list, as.dist)
      return(D.list)
    }
    
    Dgeo_list <- chk.D.list(Dgeo_list, name.D = "Dgeo")
    Dgen_list <- chk.D.list(Dgen_list, name.D = "Dgen")
    
    len.elements.Dgeo <- sapply(Dgeo_list, length)
    len.elements.Dgen <- sapply(Dgen_list, length)
    
    if (is.character(all.equal(len.elements.Dgeo, len.elements.Dgen))) {
      stop(error(" The arguments Dgen and Dgeo should have identical dimensions\n"))
    }
    coordstring <- "Dgeo provided."
    distance <- "Dgen provided"
    typedis <- "ind"
  } # Close if matrices are provided
  #----------------------------------------------------------------------------#
  #### Apply transformations ####
  apply.transformation <- function(D, transFUN, name.D) {
    assign(name.D, value = D)
    new.obj <- eval(parse(text = transFUN))
    return(new.obj)
  }
  
  Dgen_list <- lapply(Dgen_list, apply.transformation, transFUN=Dgen_trans, name.D="Dgen")
  Dgeo_list <- lapply(Dgeo_list, apply.transformation, transFUN=Dgeo_trans, name.D="Dgeo")
  
  lapply(Dgeo_list, function(D) {
    if(sum(is.infinite(D)) > 0) {
      stop(
        error(
          "Most likely some pairwise individual distances were zero and the transformation created missing values [e.g. log(Dgeo)]. Consider adding a suitable tranformation e.g. an offset to your Dgeo transformation if using a log transformation [e.g. Dgeo_trans='log(Dgeo+1)'] or adding some 'noise' to the coordinates.\n"
        )
      )
    }
    
  } 
  )
  
  convert2matrix <- function(D) {
    D <- as.matrix(D)
    diag(D) <- 0
    return(D)
  }
  
  Dgen_list <- lapply(Dgen_list, convert2matrix) 
  Dgeo_list <- lapply(Dgeo_list, convert2matrix)
  
  #### Execute utils.spautocorr on a list ####
  res <- list()
  
  for(z in seq_along(Dgeo_list)) {
    
    Dgeo <- Dgeo_list[[z]]
    Dgen <- Dgen_list[[z]]
    
    sample.size <- nrow(Dgeo)
    crt <- 1 / (sample.size - 1) # correction
    nbins <- if (length(bins) == 1) {
      bins
    } else {
      length(bins) - 1
    }
    
    splist <-
      utils.spautocor(Dgen,
                      Dgeo,
                      permutation = FALSE,
                      bins = bins,
                      reps = reps)
    
    if (permutation) {
      bssplist <- replicate(reps,
                            utils.spautocor(
                              Dgen,
                              Dgeo,
                              permutation = TRUE,
                              bins = bins,
                              reps = reps
                            ))
      
      #convert the output into a matrix
      bs <- matrix(
        unlist(bssplist),
        nrow = reps,
        ncol = nbins,
        byrow = TRUE
      )
      
      # Save permutations
      temp_perm <- tempfile(pattern = paste0("Perm_Pop", z))
      saveRDS(bs, file = temp_perm)
      
      bs.l <- apply(bs, 2, quantile, probs = 0.025, na.rm = TRUE)
      bs.u <- apply(bs, 2, quantile, probs = 0.975, na.rm = TRUE)
      
      p.one.tail <-
        sapply(seq_along(splist$r.uc), function(i, r.rc, r) {
          if (is.na(r[i])) {
            NA
          } else{
            # the observed value counts as one of the permutations
            if (r[i] >= 0) {
              (sum(r.rc[, i] >= r[i]) + 1) / (length(r.rc[, i]) + 1)
            } else{
              (sum(r.rc[, i] <= r[i]) + 1) / (length(r.rc[, i]) + 1)
            }
          }
        }, r = splist$r.uc + crt,  r.rc = bs + crt)
      
    }
    
    if (bootstrap) {
      errors <-
        replicate(reps,
                  utils.spautocor(
                    Dgen,
                    Dgeo,
                    bootstrap = TRUE,
                    bins = bins,
                    reps = reps
                  ))
      errors <-
        matrix(unlist(errors),
               nrow = reps,
               ncol = nbins,
               byrow = TRUE)
      # save Boots
      temp_boots <- tempfile(pattern = paste0("Boots_Pop", z))
      saveRDS(errors, file = temp_boots)
      
      err.l <- apply(errors, 2, quantile, probs = 0.025, na.rm = TRUE)
      err.u <- apply(errors, 2, quantile, probs = 0.975, na.rm = TRUE)
    }
    
    res_temp <- cbind(splist, Correction = crt, r = splist$r.uc + crt)
    if (bootstrap) {
      res_temp <- cbind(res_temp, L.r = err.l + crt, U.r = err.u + crt)
    }
    
    if (permutation) {
      res_temp <- cbind(
        res_temp,
        L.r.null.uc = bs.l,
        U.r.null.uc = bs.u,
        L.r.null = bs.l + crt,
        U.r.null = bs.u + crt,
        p.one.tail = p.one.tail
      )
    }
    res[[z]] <- res_temp
  }
  
  names(res) <- pop.names
  #-------- Close Execute utils.spautoCorr ------------------------------------#
  
  #### PRINTING OUTPUTS ####
  
  if (plot.out) {
    
    if (is.null(plot.theme)) {
      plot.theme <- theme_dartR()
    }
    
    if (is.null(plot.colors.pop)) {
      plot.colors.pop <- dartR.base::gl.select.colors(x, verbose=0)
    }
    
    spa_multi <-data.table::rbindlist(res, use.names = TRUE, 
                                      fill = TRUE, idcol = "Population")
    if(spa_multi [, max(Bin)] > 1000) {
      lbls <- round(spa_multi$Bin/1000, 1) 
    } else {
      lbls <- spa_multi$Bin
    }
    
    if (length(Dgen_list) == 1) {
      x.scale <- scale_x_continuous(breaks = spa_multi$Bin,
                                    labels = lbls,
                                    sec.axis = sec_axis(
                                      ~ .,
                                      breaks = spa_multi$Bin,
                                      labels = spa_multi$N))
    } else {
      x.scale <- scale_x_continuous(breaks = spa_multi$Bin,
                                    labels = lbls)
    }
    
    p3 <- ggplot(spa_multi, aes(x = .data$Bin, y = .data$r,
                                col = .data$Population)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, col = "black", linewidth = 1) +
      scale_color_manual(values = plot.colors.pop) +
      x.scale +
      ylab("Autocorrelation (r)") + 
      xlab("Distance class") + 
      plot.theme
    
    if (bootstrap) {
      p3 <- p3 +   
        geom_errorbar(aes(ymin = .data$L.r, ymax = .data$U.r), 
                      width=spa_multi[, mean(tail(Bin, -1) - head(Bin, -1))]/10) 
    }
    
    if (permutation & plot.pops.together == FALSE) {
      p3 <- p3 +  
        geom_ribbon(aes(ymin = .data$L.r.null, ymax = .data$U.r.null),
                    fill = CI.color, alpha=0.25,show.legend = FALSE) + 
        geom_line(aes(y = .data$L.r.null), col = "black", linetype = "dashed") +
        geom_point(aes(y = .data$L.r.null), col = "black") +
        geom_line(aes(y = .data$U.r.null), col = "black", linetype = "dashed") +
        geom_point(aes(y = .data$U.r.null), col = "black") +
        facet_wrap(~Population, nrow = length(Dgen_list), scales = "free_y") +
        theme(legend.position = "none") 
    }
    
    if(length(Dgen_list) == 1) {
      p3 <- p3 + 
        theme(strip.text = element_blank(), legend.position = "none")
    }
    suppressWarnings(
      suppressMessages(print(p3))
    )
  }
  
  if (verbose > 0) {
    cat(report("  Coordinates used from:", coordstring, "\n"))
    cat(report("  Transformation of Dgeo:", Dgeo_trans, "\n"))
    cat(report("  Genetic distance:", distance, "\n"))
    cat(report("  Tranformation of Dgen: ", Dgen_trans, "\n"))
  }
  if (verbose >= 3) {
    print(res)
  }
  
  # Optionally save the plot ---------------------
  
  if(!is.null(plot.file)){
    tmp <- utils.plot.save(p3,
                           dir=plot.dir,
                           file=plot.file,
                           verbose=verbose)
  }
  
  # FLAG SCRIPT END
  
  if (verbose >= 1) {
    cat(report("Completed:", funname, "\n"))
  }
  
  # RETURN
  return(invisible(res))
}
