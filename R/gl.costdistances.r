#' @name gl.costdistances
#' @title Calculates distances across a resistance landscape
#' @family spatial analysis functions
#' @description Calculates pairwise least-cost, randomised shortest-path or
#' random-walk commute distances between locations on a resistance raster.
#' @details
#' Adjacent cells have conductance equal to the reciprocal of their mean
#' resistance. Connections are symmetric. Geographic correction uses inter-cell
#' distance, with type 'c' for least-cost and randomised shortest paths (RSP), and
#' type 'r' for commute. For geographic rasters, distances use metres; projected
#' rasters use their coordinate units. Explicit x/y coordinates on rasters with
#' no CRS use local grid units; no projection or metre units are inferred.
#'
#' Least-cost and RSP distances combine resistance and distance units. Commute
#' is expected random-walk travel time (steps there and back), equal to effective
#' resistance multiplied by graph volume (the sum of conductance over both
#' directions). It is not the unscaled effective resistance used by Circuitscape.
#' RSP retains gdistance's default net-movement method. Theta controls the path
#' model and interacts with cost scale; decrease it explicitly if theta = 1
#' underflows on large costs. No automatic cost or theta scaling is performed.
#' Type 'c' is the RSP correction convention here; gdistance has no unique
#' geographic correction for intermediate RSP regimes.
#'
#' NA and positive Inf raster values are barriers. Traversable cells must have
#' strictly positive, finite resistance. Locations on barriers or outside the
#' raster are rejected. Disconnected least-cost pairs return Inf; commute and
#' RSP require a connected traversable graph. A single location, or locations
#' in the same cell, have zero pairwise distance.
#'
#' Genlight coordinates are interpreted as WGS84 longitude/latitude. Arithmetic
#' population centres are calculated in those coordinates, then transformed to
#' the raster CRS. Stored coordinate rows must follow individual order. With no
#' population assignments, individuals are used instead. Missing assignments
#' or coordinates are rejected; no observations are silently omitted.
#' @param landscape Single-layer RasterLayer of resistance values [required].
#' @param locs Genlight with named lon/lat columns in @other$latlon, or a numeric
#' two-column matrix/data.frame in x/y order in the raster CRS. Supplied row
#' names must be unique. For individual distances from a genlight with existing
#' populations, set pop(x) <- indNames(x) [required].
#' @param method One of 'leastcost', 'rSPDistance' or 'commute' [required].
#' @param NN Neighbourhood: 4, 8, 16 or 'bishop' (diagonals only). Eight neighbours
#' is a common choice [required].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#' @param theta RSP parameter, strictly between 0 and 20. Used only when method
#' is 'rSPDistance'; changing it changes the path model [default 1].
#' @return A numeric square matrix of pairwise distances, with location or
#' population names on both axes when supplied. Input objects are unchanged.
#' @seealso \link[gdistance]{costDistance}, \link[gdistance]{rSPDistance},
#' \link[gdistance]{commuteDistance}, \link[gdistance]{geoCorrection}
#' @examples
#' if (requireNamespace("gdistance", quietly = TRUE)) {
#'   landscape.sim <- readRDS(system.file("extdata", "landscape.sim.rdata",
#'                                        package = "dartR.data"))
#'   # This synthetic landscape uses local grid coordinates, not a named CRS.
#'   xy <- apply(possums.gl@other$xy, 2, function(a)
#'     tapply(a, pop(possums.gl), mean))
#'   cd <- gl.costdistances(landscape.sim, xy, method = "leastcost", NN = 8)
#'   round(cd, 3)
#' }
#' @export

gl.costdistances <- function(landscape,
                             locs,
                             method,
                             NN,
                             verbose = NULL,
                             theta = 1) {
    verbose <- gl.check.verbosity(verbose)
    funname <- match.call()[[1]]
    if (is.function(funname)) funname <- "gl.costdistances"
    utils.flag.start(func = funname, verbose = verbose)

    if (!requireNamespace("gdistance", quietly = TRUE)) {
        stop(error("Install package gdistance to calculate cost distances.\n"))
    }
    if (!inherits(landscape, "RasterLayer") || !raster::hasValues(landscape)) {
        stop(error("landscape must be a single RasterLayer with values.\n"))
    }
    if (!is.character(method) || length(method) != 1L || is.na(method) ||
        !method %in% c("leastcost", "rSPDistance", "commute")) {
        stop(error("method must be leastcost, rSPDistance or commute.\n"))
    }
    if (!(is.numeric(NN) && length(NN) == 1L && !is.na(NN) &&
          NN %in% c(4, 8, 16)) && !identical(NN, "bishop")) {
        stop(error("NN must be 4, 8, 16 or 'bishop'.\n"))
    }
    if (method == "rSPDistance" &&
        (!is.numeric(theta) || length(theta) != 1L || !is.finite(theta) ||
         theta <= 0 || theta >= 20)) {
        stop(error("theta must be a finite number strictly between 0 and 20.\n"))
    }
    resistance <- raster::getValues(landscape)
    if (any(resistance <= 0, na.rm = TRUE)) {
        stop(error("Traversable resistance must be strictly positive.\n"))
    }
    traversable <- is.finite(resistance)
    if (!any(traversable)) {
        stop(error("landscape has no finite, positive traversable cells.\n"))
    }
    # Work on a local raster: either NA or positive Inf denotes a barrier.
    resistance[!traversable] <- NA_real_
    landscape <- raster::setValues(landscape, resistance)

    check.names <- function(ids) {
        if (!is.null(ids) &&
            (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))) {
            stop(error("Location names must be unique and non-missing.\n"))
        }
    }
    check.coords <- function(coords, label) {
        if (!is.matrix(coords) || !is.numeric(coords) ||
            ncol(coords) != 2L || nrow(coords) < 1L) {
            stop(error(label, "must contain at least one numeric x/y pair.\n"))
        }
        bad <- which(rowSums(!is.finite(coords)) > 0L)
        if (length(bad)) {
            ids <- rownames(coords)
            if (is.null(ids)) ids <- seq_len(nrow(coords))
            stop(error(label, "has non-finite coordinates for:",
                       paste(ids[bad], collapse = ", ")))
        }
    }
    if (is(locs, "genlight")) {
        coords <- locs@other$latlon
        if (is.null(coords) || length(dim(coords)) != 2L ||
            !all(c("lon", "lat") %in% colnames(coords)) ||
            nrow(coords) != nInd(locs)) {
            stop(error("Provide lon/lat columns for every genlight individual.\n"))
        }
        coords <- as.matrix(coords[, c("lon", "lat"), drop = FALSE])
        rownames(coords) <- indNames(locs)
        check.coords(coords, "Genlight coordinates")
        if (any(abs(coords[, 1]) > 180 | abs(coords[, 2]) > 90)) {
            stop(error("Genlight coordinates must be WGS84 longitude/latitude.\n"))
        }
        groups <- pop(locs)
        if (is.null(groups)) {
            if (verbose >= 2) {
                cat(report("No populations supplied; using individuals.\n"))
            }
            groups <- indNames(locs)
            if (is.null(groups)) groups <- as.character(seq_len(nInd(locs)))
            check.names(groups)
        } else if (anyNA(groups) || any(!nzchar(as.character(groups)))) {
            stop(error("Every individual must have a population assignment.\n"))
        }
        centres <- apply(coords, 2, function(a) tapply(a, groups, mean))
        check.coords(centres, "Population centres")
        target.crs <- raster::projection(landscape)
        if (is.na(target.crs) || !nzchar(target.crs)) {
            stop(error("Genlight coordinates require a known landscape CRS.\n"))
        }
        if (!requireNamespace("terra", quietly = TRUE)) {
            stop(error("Install package terra to transform genlight coordinates.\n"))
        }
        locs <- terra::project(centres, from = "EPSG:4326", to = target.crs)
        rownames(locs) <- rownames(centres)
    } else {
        locs <- as.matrix(locs)
    }
    check.coords(locs, "Locations")
    check.names(rownames(locs))
    cells <- raster::cellFromXY(landscape, locs)
    bad <- which(is.na(cells) | !traversable[cells])
    if (length(bad)) {
        ids <- rownames(locs)
        if (is.null(ids)) ids <- seq_len(nrow(locs))
        stop(error("Locations outside the raster or on barriers:",
                   paste(ids[bad], collapse = ", ")))
    }
    if (verbose >= 2) {
        cat(report("Calculating", method, "for", nrow(locs), "locations.\n"))
    }
    if (length(unique(cells)) == 1L) {
        cd.mat <- matrix(0, nrow(locs), nrow(locs))
    } else {
        transition <- suppressMessages(gdistance::transition(
            landscape, function(a) 1 / mean(a), NN, symm = TRUE))
        correction <- if (method == "commute") "r" else "c"
        transition <- gdistance::geoCorrection(transition, type = correction)
        if (method == "leastcost") {
            cd.mat <- gdistance::costDistance(transition, locs, locs)
        } else {
            # Check all traversable cells: a disconnected graph has no global
            # commute/RSP solution under this function's connected-graph model.
            weights <- gdistance::transitionMatrix(transition)
            active <- which(traversable)
            weights <- weights[active, active, drop = FALSE]
            if (any(!is.finite(weights@x)) || any(weights@x < 0)) {
                stop(error("Non-finite graph weights; check resistance scale.\n"))
            }
            graph <- igraph::graph_from_adjacency_matrix(
                weights, mode = "undirected", weighted = TRUE, diag = FALSE)
            if (igraph::components(graph)$no != 1L) {
                stop(error(paste0("Commute and RSP require a connected ",
                                  "traversable landscape; check barriers and NN.\n")))
            }
            if (method == "rSPDistance") {
                cd.mat <- gdistance::rSPDistance(transition, locs, locs,
                                                theta = theta)
                if (any(!is.finite(cd.mat))) {
                    stop(error(paste0("RSP produced non-finite distances. ",
                        "Check cost scale and choose theta explicitly ",
                        "(a smaller theta may avoid underflow).\n")))
                }
            } else {
                # Normalising conductance leaves volume * resistance invariant.
                weights <- weights / max(weights)
                degree <- Matrix::rowSums(weights)
                laplacian <- Matrix::Diagonal(x = degree) - weights
                n <- nrow(weights)
                selected <- match(unique(cells), active)
                k <- length(selected)
                inverse.selected <- matrix(0, k, k)
                grounded <- laplacian[-n, -n, drop = FALSE]
                factor <- Matrix::Cholesky(grounded, LDL = FALSE)
                for (j in which(selected != n)) {
                    rhs <- numeric(n - 1L)
                    rhs[selected[j]] <- 1
                    potential <- c(as.numeric(Matrix::solve(
                        factor, rhs, system = "A")), 0)
                    inverse.selected[, j] <- potential[selected]
                }
                # Only selected entries of the inverse are stored, including
                # the grounded node's zero row/column, never a dense n-by-n inverse.
                effective <- outer(diag(inverse.selected),
                                   diag(inverse.selected), "+") -
                    inverse.selected - t(inverse.selected)
                commute <- pmax(effective, 0) * sum(degree)
                index <- match(cells, unique(cells))
                cd.mat <- commute[index, index, drop = FALSE]
            }
        }
    }
    dimnames(cd.mat) <- if (is.null(rownames(locs))) NULL else
        list(rownames(locs), rownames(locs))
    if (verbose >= 3) {
        cat(report("Returned", nrow(cd.mat), "by", ncol(cd.mat),
                   "distance matrix;", sum(!is.finite(cd.mat)),
                   "unreachable entries.\n"))
    }
    if (verbose > 0) cat(report("Completed:", funname, "\n"))
    return(cd.mat)
}
