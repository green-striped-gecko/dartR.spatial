#' @name gl.genleastcost
#' @title Performs least-cost path analysis based on a friction matrix
#' @family spatial analysis functions
#'
#' @description
#' This function calculates pairwise distances (Euclidean, cost path
#' distances and genetic distances) between populations or between individuals
#' using a friction matrix and a spatial genlight object. The output of this
#' function can be used with the functions wassermann and lgrMMRR from package
#' PopGenReport to test for the significance of a layer on the genetic
#' structure.
#'
#' @details
#' The friction matrix can be a single raster or a stack of several layers,
#' supplied as a file path, a RasterLayer, RasterStack or RasterBrick (package
#' raster) or a SpatRaster (package terra). The specified cost distance is
#' calculated for each layer.
#'
#' Cost distances are calculated with \code{\link{gl.costdistances}}, so both
#' functions return the same values for the same landscape and locations. See
#' that function for how resistance, barriers and geographic correction are
#' handled.
#'
#' Coordinates are taken from x@other$xy, which must be in the same coordinate
#' system as the friction matrix. If x@other$xy is missing, the lon and lat
#' columns of x@other$latlon are used as they are, without projection, so they
#' must also match the coordinate system of the friction matrix. For population
#' distances, the arithmetic mean of the coordinates of each population is
#' used.
#'
#' Genetic distances between populations are 'D', 'Gst.Nei' and 'Gst.Hedrick'
#' (package mmod). Genetic distances between individuals are 'kosman'
#' (\code{\link{gl.kosman}}), 'propShared' (one minus the proportion of shared
#' alleles, \code{\link{gl.propShared}}) and 'dist' (Euclidean distance between
#' allele counts).
#'
#' @param x Name of the genlight object containing SNP data and coordinates
#' [required].
#' @param fric.raster A friction matrix: file path, RasterLayer, RasterStack,
#' RasterBrick or SpatRaster [required].
#' @param gen.distance Genetic distance between populations ('D', 'Gst.Nei',
#' 'Gst.Hedrick') or individuals ('kosman', 'propShared', 'dist')
#' [default "Gst.Nei"].
#' @param NN Number of neighbours used when calculating the cost distance
#' (possible values 4, 8, 16 or 'bishop'). NN=8 is most commonly used. Be
#' aware that linear structures may cause artefacts in the least-cost paths,
#' therefore inspect the actual least-cost paths in the provided output
#' [default 8].
#' @param pathtype Type of cost distance to be calculated: 'leastcost',
#' 'commute' or 'rSPDistance'. If the path type is set to 'leastcost' and
#' plotpath is TRUE, paths and path lengths are also returned
#' [default 'leastcost'].
#' @param plotpath If TRUE, plots each friction layer with the individuals,
#' population centres and (for 'leastcost') the least-cost paths, and returns
#' the paths and their lengths. Calculating paths slows down the computation,
#' but checking them visually is recommended [default TRUE].
#' @param theta Value needed for pathtype 'rSPDistance', strictly between 0 and
#' 20. See \code{\link{gl.costdistances}} [default 1].
#' @param plot.colors.pop A color palette for population plots or a list with
#' as many colors as there are populations in the dataset
#' [default gl.colors("dis")].
#' @param raster.colors The color palette to use to color the raster values
#' [default rev(terrain.colors(255))].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#'
#' @return A list with six elements:
#' \itemize{
#' \item gen.mat -- matrix of pairwise genetic distances.
#' \item eucl.mat -- matrix of pairwise Euclidean distances.
#' \item cost.matnames -- names of the friction layers.
#' \item cost.mats -- list of cost distance matrices, one per layer.
#' \item pathlength.mats -- list of path length matrices, one per layer
#' (only for pathtype 'leastcost' with plotpath = TRUE).
#' \item paths -- list of least-cost paths as SpatialLines objects, one list
#' per layer (only for pathtype 'leastcost' with plotpath = TRUE).
#' }
#'
#' @author Author(s): Bernd Gruber. Custodian: Bernd Gruber -- Post to
#' \url{https://groups.google.com/d/forum/dartr}
#'
#' @references
#' \itemize{
#' \item Cushman, S., Wasserman, T., Landguth, E. and Shirk, A. (2013).
#' Re-Evaluating Causal Modeling with Mantel Tests in Landscape Genetics.
#' Diversity, 5(1), 51-72.
#' \item Landguth, E. L., Cushman, S. A., Schwartz, M. K., McKelvey, K. S.,
#' Murphy, M. and Luikart, G. (2010). Quantifying the lag time to detect
#' barriers in landscape genetics. Molecular ecology, 4179-4191.
#' \item Wasserman, T. N., Cushman, S. A., Schwartz, M. K. and Wallin, D. O.
#'  (2010). Spatial scaling and multi-model inference in landscape genetics:
#'  Martes americana in northern Idaho. Landscape Ecology, 25(10), 1601-1612.
#'  }
#'
#' @examples
#' \donttest{
#' if (requireNamespace("gdistance", quietly = TRUE) &&
#'     requireNamespace("mmod", quietly = TRUE)) {
#'   landscape.sim <- readRDS(system.file("extdata", "landscape.sim.rdata",
#'                                        package = "dartR.data"))
#'   # three populations and a coarser landscape to keep the example fast
#'   x <- possums.gl[pop(possums.gl) %in% c("A", "B", "C"), ]
#'   landscape <- raster::aggregate(landscape.sim, 5)
#'   glc <- gl.genleastcost(x, fric.raster = landscape, gen.distance = "D",
#'                          NN = 8, pathtype = "leastcost", plotpath = TRUE)
#'   glc$cost.mats
#'   # test the significance of the layer (package PopGenReport)
#'   if (requireNamespace("PopGenReport", quietly = TRUE)) {
#'     PopGenReport::wassermann(eucl.mat = glc$eucl.mat,
#'                              cost.mat = glc$cost.mats,
#'                              gen.mat = glc$gen.mat)
#'   }
#' }
#' }
#'
#' @seealso \code{\link{gl.costdistances}}
#' @importFrom sp Line Lines SpatialLines SpatialLinesLengths
#' @importFrom raster plot
#' @importFrom grDevices terrain.colors
#' @export

gl.genleastcost <- function(x,
                            fric.raster,
                            gen.distance = "Gst.Nei",
                            NN = 8,
                            pathtype = "leastcost",
                            plotpath = TRUE,
                            theta = 1,
                            plot.colors.pop = gl.colors("dis", verbose = 0),
                            raster.colors = rev(terrain.colors(255)),
                            verbose = NULL) {
    # SET VERBOSITY
    verbose <- gl.check.verbosity(verbose)
    
    # FLAG SCRIPT START
    funname <- match.call()[[1]]
    utils.flag.start(func = funname,
                     verbose = verbose)
    
    # CHECK DATATYPE
    datatype <- utils.check.datatype(x, accept = "SNP", verbose = verbose)
    
    # FUNCTION SPECIFIC ERROR CHECKING
    
    pop.distances <- c("D", "Gst.Hedrick", "Gst.Nei")
    ind.distances <- c("kosman", "propShared", "dist")
    if (!is.character(gen.distance) || length(gen.distance) != 1 ||
        !(gen.distance %in% c(pop.distances, ind.distances))) {
        stop(error(
            "Fatal Error: gen.distance must be one of",
            paste(c(pop.distances, ind.distances), collapse = ", "),
            "\n"
        ))
    }
    dist.type <- if (gen.distance %in% pop.distances) "pop" else "ind"
    
    if (!is.character(pathtype) || length(pathtype) != 1 ||
        !(pathtype %in% c("leastcost", "commute", "rSPDistance"))) {
        stop(error(
            "Fatal Error: pathtype must be leastcost, commute or rSPDistance\n"
        ))
    }
    
    if (!(is.numeric(NN) && length(NN) == 1 && !is.na(NN) &&
          NN %in% c(4, 8, 16)) && !identical(NN, "bishop")) {
        stop(error(
            "Fatal Error: NN must be 4, 8, 16 or 'bishop'. NN=8 is the most",
            "commonly used option; if linear features are tested you may want",
            "to consider NN=4.\n"
        ))
    }
    
    # CHECK IF PACKAGES ARE INSTALLED
    pkg <- "gdistance"
    if (!(requireNamespace(pkg, quietly = TRUE))) {
        stop(error(
            "Package",
            pkg,
            " needed for this function to work. Please install it.\n"
        ))
    }
    
    if (dist.type == "pop") {
        pkg <- "mmod"
        if (!(requireNamespace(pkg, quietly = TRUE))) {
            stop(error(
                "Package",
                pkg,
                " needed for this function to work. Please install it.\n"
            ))
        }
    }
    
    # coordinates: projected x/y, otherwise lon/lat used as they are
    xy <- x@other$xy
    if (is.null(xy)) {
        latlon <- x@other$latlon
        if (is.null(latlon) || length(dim(latlon)) != 2 ||
            !all(c("lon", "lat") %in% colnames(latlon))) {
            stop(error(
                "Fatal Error: No coordinates found. Provide projected",
                "coordinates in x@other$xy or lon/lat columns in",
                "x@other$latlon\n"
            ))
        }
        cat(
            warn(
                "No projected coordinates in @other$xy found. Hence will use latlons (if provided), which are not projected, hence there might be distortions if the area covered is large or close to the poles. Be aware your resistance layer and coordinates in the genlight object need to have the same coordinate system.\n"
            )
        )
        xy <- latlon[, c("lon", "lat")]
    }
    xy <- as.matrix(xy)[, 1:2, drop = FALSE]
    if (nrow(xy) != nInd(x)) {
        stop(error(
            "Fatal Error: Number of coordinates is different from the number",
            "of individuals\n"
        ))
    }
    if (anyNA(xy)) {
        stop(error(
            "Fatal Error: Missing coordinates for:",
            paste(indNames(x)[rowSums(is.na(xy)) > 0], collapse = ", "),
            "\n"
        ))
    }
    
    # read the friction matrix as a stack so that every layer is used
    if (is.character(fric.raster) ||
        inherits(fric.raster, c("RasterLayer", "RasterStack", "RasterBrick",
                                "SpatRaster"))) {
        fric.raster <- raster::stack(fric.raster)
    } else {
        stop(error(
            "Fatal Error: fric.raster must be a file path, RasterLayer,",
            "RasterStack, RasterBrick or SpatRaster\n"
        ))
    }
    
    # DO THE JOB
    
    if (dist.type == "pop") {
        # calculate the centers if population measurement is wanted
        c.x <- tapply(xy[, 1], x@pop, mean)
        c.y <- tapply(xy[, 2], x@pop, mean)
        cp <- cbind(c.x, c.y)
        eucl.mat <- as.matrix(dist(cp))
        dimnames(eucl.mat) <- list(popNames(x), popNames(x))
        npop <- length(levels(x@pop))
    } else {
        cp <- cbind(xy[, 1], xy[, 2])
        eucl.mat <- as.matrix(dist(cp))
        dimnames(eucl.mat) <- list(indNames(x), indNames(x))
        npop <- length(indNames(x))
    }
    rownames(cp) <- NULL
    
    # population colors 
    # if pop colors is a palette
    if (is(plot.colors.pop, "function")) {
      cols <- plot.colors.pop(length(levels(pop(x))))
    }
    # if pop colors is a vector
    if (!is(plot.colors.pop, "function")) {
      cols <- plot.colors.pop
    }
    
    colors_pops <- cols[as.numeric(pop(x))]

    mats <- list()
    mats.names <- NA
    mats.pathlength <- list()
    mats.paths <- list()
    
    pathlength.mat <- NULL
    paths <- NULL
    
    n.mats <-
        dim(fric.raster)[3]  #number of rasters in the stack
    
    for (ci in 1:n.mats) {
        layer <- fric.raster[[ci]]
        
        if (verbose >= 2) {
            cat(report(
                "  Calculating", pathtype, "distances for layer",
                names(fric.raster)[ci], "\n"
            ))
        }
        
        # same cost distances as gl.costdistances
        cd.mat <- gl.costdistances(
            layer,
            cp,
            method = pathtype,
            NN = NN,
            verbose = 0,
            theta = theta
        )
        dimnames(cd.mat) <- dimnames(eucl.mat)
        
        pathlength.mat <- NULL
        paths <- NULL
        
        if (plotpath) {
            raster::plot(layer,
                         col = raster.colors,
                         main = paste(names(fric.raster)[ci],
                                      ":", 
                                      pathtype, 
                                      ", NN=", 
                                      NN, 
                                      sep = ""))
            
            points(
                xy,
                cex = 1,
                pch = 16,
                col = colors_pops
            )
            if (dist.type == "pop")
                points(cp,
                       cex = 1.5,
                       pch = 15,
                       col = "black")
        }
        
        # only show paths if leastcost otherwise not possible
        if (pathtype == "leastcost" & plotpath == TRUE) {
            # transition built as in gl.costdistances: NA and Inf cells are
            # barriers, conductance is the reciprocal of the mean resistance
            resistance <- raster::getValues(layer)
            resistance[!is.finite(resistance)] <- NA_real_
            layer.local <- raster::setValues(raster::raster(layer), resistance)
            fric.mat <- suppressMessages(gdistance::transition(
                layer.local, function(a) 1 / mean(a), NN, symm = TRUE))
            fric.mat.cor <- gdistance::geoCorrection(fric.mat, type = "c")
            
            comb <- t(combn(1:npop, 2))
            
            # pathlength matrix
            pathlength.mat <- cd.mat
            pathlength.mat[,] <- 0
            paths <- list()
            
            cols <- rainbow(dim(comb)[1], alpha = 0.5)
            for (i in 1:dim(comb)[1]) {
                if (dist(rbind(cp[comb[i, 1],], cp[comb[i, 2],])) == 0) {
                    ll <- Line(rbind(cp[comb[i, 1],], cp[comb[i, 2],]))
                    S1 <- Lines(list(ll), ID = "Null")
                    sPath <- SpatialLines(list(S1))
                } else {
                    sPath <-
                        gdistance::shortestPath(fric.mat.cor, cp[comb[i, 1],], cp[comb[i, 2],], output = "SpatialLines")
                }
                
                lines(sPath, lwd = 1.5, col = cols[i])
                paths[[i]] <- sPath
                ll <- round(SpatialLinesLengths(sPath), 3)
                pathlength.mat[comb[i, 1], comb[i, 2]] <- ll
                pathlength.mat[comb[i, 2], comb[i, 1]] <- ll
            }
            
        }
        
        mats[[ci]] <- cd.mat
        mats.names[[ci]] <- names(fric.raster)[ci]
        mats.pathlength[[ci]] <- pathlength.mat
        mats.paths[[ci]] <- paths
        
    }  #end of ci loop
    
    names(mats) <- names(fric.raster)
    
    # genetic distances across populations or individuals
    
    if (gen.distance == "Gst.Nei") {
        gendist.mat <- as.matrix(mmod::pairwise_Gst_Nei(gl2gi(x, verbose = 0)))
    }
    
    if (gen.distance == "Gst.Hedrick") {
        gendist.mat <- as.matrix(mmod::pairwise_Gst_Hedrick(gl2gi(x, verbose = 0)))
    }
    
    if (gen.distance == "D") {
        gendist.mat <- as.matrix(mmod::pairwise_D(gl2gi(x, verbose = 0)))
    }
    
    if (gen.distance == "kosman") {
        gendist.mat <- as.matrix(as.dist(gl.kosman(x, verbose = 0)$kosman))
    }
    
    if (gen.distance == "propShared") {
        # gl.propShared returns a similarity
        gendist.mat <- 1 - gl.propShared(x, verbose = 0)
    }
    
    if (gen.distance == "dist") {
      gendist.mat <- as.matrix(dist(as.matrix(gl2gi(x, verbose = 0))))
    }
    
    dimnames(gendist.mat) <- dimnames(eucl.mat)
    
    # FLAG SCRIPT END
    
    if (verbose >= 1) {
        cat(report("Completed:", funname, "\n"))
    }
    
    # RETURN
    
    return(
        list(
            gen.mat = gendist.mat,
            eucl.mat = eucl.mat,
            cost.matnames = mats.names,
            cost.mats = mats,
            pathlength.mats = mats.pathlength,
            paths = mats.paths
        )
    )
}
