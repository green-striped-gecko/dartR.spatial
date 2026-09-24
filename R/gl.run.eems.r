#' Runs the EEMS algorithm  (Estimating Effective Migration Surfaces) on a 
#' genlight object.
#'
#' @description
#' This function runs the EEMS algorithm on a genlight object. The EEMS
#'  algorithm is a spatially explicit model that estimates effective migration 
#'  surfaces (EEMS) from genetic data. The EEMS algorithm is implemented in 
#'  C++, hence it is necessary to have the binary downloaded and the function 
#'  needs to point to this file via the path specified in eems.path. The binary
#'   is call runeems_snps[.exe] and can be downloaded from the github site of
#'    dartRverse (https://github.com/green-striped-gecko/dartRverse/tree/main/binaries).
#'
#' @param x Name of the genlight object containing the SNP data [required].
#' @param eems.path Path to the folder containing the eems executable
#'  [default working directory ("./")].
#' @param buffer Buffer distance for all the elements [default 10000].
#' @param nDemes The approximate number of demes in the population graph 
#' [default 500].
#' @param diploid Whether the organism is diploid. TRUE requires ploidy 2
#' for every individual; FALSE requires ploidy 1. Input must represent SNP
#' allele dosages, not dominant presence/absence markers [default TRUE].
#' @param numMCMCIter Number of MCMC iterations [default 10000].
#' @param numBurnIter Number of burn-in iterations to discard at the start 
#' [default 2000].
#' @param numThinIter Number of iterations to thin between two writing steps
#'  [default 9].
#' @param seed 	An integer used to seed the random number generator 
#' [default NULL].
#' @param dpi Resolution of the contour raster [default 250].
#' @param add_grid A logical value indicating whether to add the population 
#' grid or not [default FALSE].
#' @param col_grid The color of the population grid [default gray ("#BBBBBB")].
#' @param add_demes A logical value indicating whether to add the observed 
#' demes or not [default FALSE].
#' @param col_demes The color of the demes [default black ("#000000")].
#' @param add_outline A logical value indicating whether to add the habitat 
#' outline or not [default FALSE].
#' @param col_outline The color of the habitat outline 
#' [default white ("#FFFFFF")].
#' @param eems_colors The EEMS color scheme as a vector of colors, ordered 
#' from low to high [default NULL].
#' @param plot.colors.pop A color palette for population plots or a list with
#' as many colors as there are populations in the dataset
#' [default gl.colors("dis")].
#' @param out.dir Existing directory for output. Each call creates a unique
#' eems-run-* subdirectory containing data_eems/ and eems.log. Relative paths
#' are resolved from the calling directory. Results in tempdir() last only
#' for the R session [default tempdir()].
#' @param plot.dir Directory to save the plot RDS files 
#' [default as specified by the global working directory or tempdir()].
#' @param plot.file Name for the RDS binary file to save (base name only, 
#' exclude extension); NULL disables RDS saving [default NULL].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2, 
#' progress log; 3, progress and results summary; 5, full report 
#' [default NULL, unless specified using gl.set.verbosity].
#' @param cleanup Whether to delete this run's input and parameter files
#' after success. Raw EEMS results and logs are retained; failed runs retain
#' all files for diagnosis [default TRUE].
#' @param ... Extra parameters to add to function reemsplots2::make_eems_plots.
#' 
#' @details
#' Set the Number of MCMC iterations to 2 million for an initial run. If the 
#' posterior trace is still trending, lengthen the chain. Set the number of 
#' initial burnin iterations at 50% of total iterations. Set the iterations to 
#' thin between writes so that total/thin is around 100–200; a 10000‑step 
#' thinning interval is a practical default.
#' 
#' Choose the number of demes to match geographic scale: 100–250 for local or 
#' island studies and 300–500 for continental datasets. Run time grows 
#' cubically with number of demes, so anything above 1000 rarely pays off. 
#' 
#' Draw the habitat polygon with a small buffer (in meters), so every sample 
#' sits at least one grid spacing inside the edge. A 5–10 % expansion of the 
#' sample bounding box (or a few kilometers for fine‑scale work) is usually 
#' adequate.
#' 
#' The dpi argument controls sampling of the contour grid in reemsplots2,
#' not the dots per inch of an exported image. Higher values increase grid
#' resolution and plotting work without changing EEMS inference. Set image
#' dimensions and export DPI separately when saving a rendered plot.
#' Routine messages are suppressed at verbose = 0; diagnostic plots are
#' displayed at verbose >= 3. All eight plots are returned at every level.
#' 
#' @return A list of contour plots of migration and diversity rates as well as
#' several diagnostic plots. It is a good idea to examine all these figures,
#' which is why they are generated by default. Please check the examples how to
#' customise the figures.
#' \describe{
#'  \item{mrates01}{Effective migration surface. This contour plot visualises
#'   the estimated effective migration rates \code{m}, on the log10 scale after
#'    mean centering.}
#'  \item{mrates02}{Posterior probability contours \code{P(log(m) > 0) = p} 
#'  and \code{P(log(m) < 0) = p} for the given probability level \code{p}. 
#'  Since migration rates are visualised on the log10 scale after mean 
#'  centering, 0 corresponds to the overall mean migration rate. This contour 
#'  plot emphasizes regions with effective migration that is significantly 
#'  higher/lower than the overall average.}
#'  \item{qrates01}{Effective diversity surface. This contour plot visualises 
#'  the estimated effective diversity rates \code{q}, on the log10 scale after
#'   mean centering.}
#'  \item{qrates02}{Posterior probability contours \code{P(log(q) > 0) = p} 
#'  and \code{P(log(q) < 0) = p}. Similar to \code{mrates02} but applied to 
#'  the effective diversity rates.}
#'  \item{rdist01}{Scatter plot of the observed vs the fitted between-deme
#'   component of genetic dissimilarity, where one point represents a pair of 
#'   sampled demes.}
#'  \item{rdist02}{Scatter plot of the observed vs the fitted within-deme
#'   component of genetic dissimilarity, where one point represents a 
#'   sampled deme.}
#'  \item{rdist03}{Scatter plot of observed genetic dissimilarities between 
#'  demes vs observed geographic distances between demes.}
#'  \item{pilogl01}{Posterior probability trace}
#' }
#' @export
#' @importFrom grDevices chull
#' @importFrom utils write.table
#' @importFrom dismo Mercator
#' @importFrom stats runif
#' @author Bernd Gruber  & Robyn (bugs? Post to
#' \url{https://groups.google.com/d/forum/dartr})
#' @references
#' \itemize{
#' \item Petkova D (2024). _reemsplots2: Generate plots to inspect and 
#' visualize the results of EEMS_. R package version 0.1.0,
#' \url{https://github.com/dipetkov/eems}.
#' \item 
#' D Petkova, J Novembre, M Stephens. Visualizing spatial population structure
#' with estimated effective migration surfaces. Nature Genetics 48, 
#' 94 -- 100 (2016). \doi{10.1038/ng.3464}.
#' }
#' @examples
#'  \dontrun{
#'  # This example needs a binary (runeems_snps[.exe]) specific to your 
#'  # operating system  to run
#'  eems <- gl.run.eems(bandicoot.gl, eems.path = "d:/downloads/eems/")
#'  print(eems[[1]])
#'  }
#'

gl.run.eems <- function(x,
                        eems.path = "./",
                        buffer = 10000,
                        nDemes = 500,
                        diploid = TRUE,
                        numMCMCIter = 10000,
                        numBurnIter = 2000,
                        numThinIter = 9,
                        seed = NULL,
                        dpi = 250,
                        add_grid = FALSE,
                        col_grid = "#BBBBBB", 
                        add_demes = FALSE, 
                        col_demes = "#000000",
                        add_outline = FALSE,
                        col_outline = "#FFFFFF", 
                        eems_colors = NULL,
                        plot.colors.pop = gl.colors("dis"),
                        out.dir = NULL,
                        plot.dir = NULL,
                        plot.file = NULL,
                        verbose = NULL,
                        cleanup = TRUE,
                        ...) {
  # SET VERBOSITY AND FLAG SCRIPT START
  verbose <- gl.check.verbosity(verbose)
  funname <- match.call()[[1]]
  utils.flag.start(func = funname, verbose = verbose)

  # CHECK DEPENDENCIES AND INPUTS BEFORE CREATING FILES
  for (pkg in c("reemsplots2", "sf", "dismo")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      advice <- if (pkg == "reemsplots2") {
        "Install it with devtools::install_github('dipetkov/reemsplots2')."
      } else {
        paste0("Install it with install.packages('", pkg, "').")
      }
      stop(error(paste0("Package ", pkg, " is required. ", advice)),
           call. = FALSE)
    }
  }
  utils.check.datatype(x, verbose = 0)
  if (!is.logical(diploid) || length(diploid) != 1L || is.na(diploid)) {
    stop(error("diploid must be TRUE or FALSE."), call. = FALSE)
  }
  expected_ploidy <- if (diploid) 2L else 1L
  input_ploidy <- adegenet::ploidy(x)
  if (length(input_ploidy) == 0L || anyNA(input_ploidy) ||
      any(input_ploidy != expected_ploidy)) {
    stop(error(paste0("diploid = ", diploid, " requires input ploidy ",
                      expected_ploidy, " for every individual.")),
         call. = FALSE)
  }
  if (!is.null(plot.file) &&
      (!is.character(plot.file) || length(plot.file) != 1L ||
       is.na(plot.file) || !nzchar(plot.file) ||
       plot.file %in% c(".", "..") || grepl("[/\\\\]", plot.file))) {
    stop(error("plot.file must be NULL or a base name without path separators."),
         call. = FALSE)
  }
  extra <- list(...)
  fixed_plot_args <- c("mcmcpath", "longlat", "dpi", "add_grid", "col_grid",
                       "add_demes", "col_demes", "add_outline", "col_outline",
                       "eems_colors")
  if (length(extra) > 0L) {
    extra_names <- names(extra)
    allowed <- setdiff(names(formals(reemsplots2::make_eems_plots)),
                       c(fixed_plot_args, "..."))
    if (is.null(extra_names) || any(!nzchar(extra_names)) ||
        anyDuplicated(extra_names) || any(!extra_names %in% allowed)) {
      stop(error(paste0(
        "Extra plotting arguments must have unique supported names: ",
        paste(allowed, collapse = ", "), ".")), call. = FALSE)
    }
  }
  if (is.null(out.dir)) out.dir <- tempdir()
  if (!is.character(out.dir) || length(out.dir) != 1L || is.na(out.dir) ||
      !dir.exists(out.dir)) {
    stop(error("out.dir must name an existing directory."), call. = FALSE)
  }
  out.dir <- normalizePath(out.dir, winslash = "/", mustWork = TRUE)
  plot.dir <- normalizePath(gl.check.wd(plot.dir, verbose = 0),
                            winslash = "/", mustWork = TRUE)
  prog <- if (.Platform$OS.type == "windows") {
    "runeems_snps.exe"
  } else {
    "runeems_snps"
  }
  executable <- file.path(eems.path, prog)
  if (!file.exists(executable)) {
    stop(error(paste0("Cannot find ", prog, " in eems.path: ", eems.path)),
         call. = FALSE)
  }
  executable <- normalizePath(executable, winslash = "/", mustWork = TRUE)
  if (file.access(executable, mode = 1) != 0L) {
    stop(error(paste0("EEMS executable is not executable: ", executable)),
         call. = FALSE)
  }

  #util function to calculate similarities
  bed2diffs_v2 <- function(Geno) {
    nIndiv <- nrow(Geno)
    nSites <- ncol(Geno)
    Miss <- is.na(Geno)
    ## Impute NAs with the column means (= twice the allele frequencies)
    Mean <- matrix(
      colMeans(Geno, na.rm = TRUE),
      ## a row of means
      nrow = nIndiv,
      ncol = nSites,
      byrow = TRUE) 
    ## a matrix with nIndiv identical rows of means
    Mean[Miss == 0] <- 0 
    ## Set the means that correspond to observed genotypes to 0
    Geno[Miss == 1] <- 0 
    ## Set the missing genotypes to 0 (used to be NA)
    Geno <- Geno + Mean
    ## Compute similarities
    Sim <- Geno %*% t(Geno) / nSites
    SelfSim <- diag(Sim) ## self-similarities
    vector1s <- rep(1, nIndiv) ## vector of 1s
    ## This chunk generates a `diffs` matrix
    Diffs <-
      SelfSim %*% t(vector1s) + vector1s %*% t(SelfSim) - 2 * Sim
    Diffs
  }
  
  # DO THE JOB
  x <- gl.filter.allna(x, verbose = 0)
  D <- bed2diffs_v2(as.matrix(x))

  # Keep final results in a fresh directory under the caller's destination.
  # Running there directly avoids unchecked export copies and stale outputs.
  # tempfile(tmpdir = ) joins with "\\" on Windows; file.path keeps "/"
  run.dir <- file.path(out.dir, basename(tempfile("eems-run-")))
  if (!dir.create(run.dir)) {
    stop(error(paste0("Cannot create an EEMS run directory in ", out.dir)),
         call. = FALSE)
  }
  data_path <- file.path(run.dir, "eems")
  eems_results <- file.path(run.dir, "data_eems")
  param_file <- file.path(run.dir, "params.ini")
  log_file <- file.path(run.dir, "eems.log")
  intermediate_files <- c(paste0(data_path, c(".diffs", ".outer", ".coord")),
                          param_file)
  if (verbose >= 2) cat(report("  EEMS run directory: ", run.dir, "\n"))

  utils::write.table(D, paste0(data_path, ".diffs"), col.names = FALSE,
                     row.names = FALSE, quote = FALSE)
  writeLines(c(
    paste0("datapath = ", data_path),
    paste0("mcmcpath = ", eems_results),
    paste0("nIndiv = ", adegenet::nInd(x)),
    paste0("nSites = ", adegenet::nLoc(x)),
    paste0("nDemes = ", nDemes),
    paste0("diploid = ", diploid),
    paste0("numMCMCIter = ", format(numMCMCIter, scientific = FALSE)),
    paste0("numBurnIter = ", format(numBurnIter, scientific = FALSE)),
    paste0("numThinIter = ", format(numThinIter, scientific = FALSE))
  ), param_file)

  ll <- data.frame(x = x@other$latlon$lon, y = x@other$latlon$lat)
  xy <- dismo::Mercator(ll)
  hpts <- grDevices::chull(xy)
  hpts <- c(hpts, hpts[1])
  poly <- xy[hpts, ]
  p <- sf::st_polygon(list(as.matrix(poly)))
  pbuf <- sf::st_buffer(p, buffer)
  if (verbose >= 3) {
    plot(pbuf, axes = TRUE, border = "green", lwd = 2)
    plot(p, add = TRUE, col = "red")
    graphics::points(xy, pch = 20, col = "blue")
  }
  pxy <- sf::st_coordinates(pbuf)[, 1:2]
  utils::write.table(pxy, paste0(data_path, ".outer"), quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  utils::write.table(xy, paste0(data_path, ".coord"), quote = FALSE,
                     row.names = FALSE, col.names = FALSE)
  if (is.null(seed)) seed <- round(stats::runif(1, 1, 1000000))

  # system2 quotes the executable; arguments need their own quoting.
  status <- system2(executable,
                    args = c("--params", shQuote(param_file),
                             "--seed", shQuote(as.character(seed))),
                    stdout = log_file, stderr = log_file)
  if (verbose >= 3 && file.exists(log_file)) {
    cat(report(paste(readLines(log_file, warn = FALSE), collapse = "\n"), "\n"))
  }
  if (status != 0L) {
    stop(error(paste0("EEMS failed (exit status ", status,
                      "). See log: ", log_file)),
         call. = FALSE)
  }
  # These files are required by the eight plots returned by reemsplots2.
  required_files <- c("rdistJtDobsJ.txt", "rdistJtDhatJ.txt", "rdistoDemes.txt",
                      "mcmcmtiles.txt", "mcmcmrates.txt", "mcmcxcoord.txt",
                      "mcmcycoord.txt", "mcmcqtiles.txt", "mcmcqrates.txt",
                      "mcmcwcoord.txt", "mcmczcoord.txt", "mcmcpilogl.txt",
                      "outer.txt", "demes.txt", "edges.txt", "ipmap.txt",
                      "eemsrun.txt")
  missing_files <- required_files[
    !file.exists(file.path(eems_results, required_files))]
  if (length(missing_files) > 0L) {
    stop(error(paste0("EEMS output is incomplete: ",
                      paste(missing_files, collapse = ", "),
                      ". See log: ", log_file)),
         call. = FALSE)
  }

  # Keep third-party diagnostics in the log, including in quiet mode.
  plot_messages <- character()
  on.exit({
    if (length(plot_messages) > 0L) {
      cat(plot_messages, file = log_file, sep = "\n", append = TRUE)
    }
  }, add = TRUE)
  plot_output <- utils::capture.output(
    p8 <- withCallingHandlers({
      plots <- do.call(reemsplots2::make_eems_plots, c(list(
        mcmcpath = eems_results, longlat = TRUE, dpi = dpi,
        add_grid = add_grid, col_grid = col_grid,
        add_demes = add_demes, col_demes = col_demes,
        add_outline = add_outline, col_outline = col_outline,
        eems_colors = eems_colors
      ), extra))
      if (missing(plot.colors.pop)) {
        plot.colors.pop <- gl.colors("dis", verbose = 0)
      }
      colors_pops <- if (is.function(plot.colors.pop)) {
        plot.colors.pop(length(levels(adegenet::pop(x))))
      } else {
        plot.colors.pop
      }
      xy_plot <- data.frame(x = xy[, 1], y = xy[, 2],
                            pop = as.character(adegenet::pop(x)))
      for (i in seq_len(4)) {
        plots[[i]] <- plots[[i]] +
          ggplot2::geom_point(data = xy_plot,
                             ggplot2::aes(x = .data$x, y = .data$y,
                                          color = .data$pop)) +
          ggplot2::scale_color_manual(values = colors_pops) +
          ggplot2::coord_equal()
      }
      plots
    }, message = function(m) {
      plot_messages <<- c(plot_messages, conditionMessage(m))
      if (verbose < 2) invokeRestart("muffleMessage")
    }, warning = function(w) {
      plot_messages <<- c(plot_messages, conditionMessage(w))
      if (verbose < 2) invokeRestart("muffleWarning")
    })
  )
  cat(plot_output, file = log_file, sep = "\n", append = TRUE)
  if (verbose >= 2 && length(plot_output) > 0L) {
    cat(report(paste(plot_output, collapse = "\n"), "\n"))
  }
  if (verbose >= 3) print(p8)

  if (!is.null(plot.file)) {
    utils.plot.save(p8, dir = plot.dir, file = plot.file, verbose = verbose)
  }
  # Never select files by a pattern; final results and logs survive cleanup.
  if (cleanup) unlink(intermediate_files)
  if (verbose > 0) cat(report("Completed:", funname, "\n"))
  return(p8)
}
