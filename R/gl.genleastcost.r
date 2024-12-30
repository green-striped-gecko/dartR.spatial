#' Performs least-cost path analysis based on a friction matrix
#'
#' This function calculates the pairwise distances (Euclidean, cost path
#'  distances and genetic distances) of populations using a friction matrix and
#'   a spatial genind object. The genind object needs to have coordinates in the
#'   same projected coordinate system as the friction matrix. The friction
#'   matrix can be either a single raster of a stack of several layers. If a
#'   stack is provided the specified cost distance is calculated for each layer
#'    in the stack. The output of this function can be used with the functions
#'    wassermann from package PopGenReport and
#'    lgrMMRR from package PopGenReport to test for the significance of a
#'    layer on the genetic structure.
#' @param x A spatial gelight object. [required].
#' @param fric.raster A friction matrix [required].
#' @param gen.distance Specification which genetic distance method should be
#' used to calculate pairwise genetic distances between populations ( 'D',
#' 'Gst.Nei', 'Gst.Hedrick') or individuals ('kosman', 'propShared')
#'  [default "Gst.Nei"].
#' @param NN Number of neighbours used when calculating the cost distance
#' (possible values 4, 8 or 16). As the default is NULL a value has to be
#' provided if pathtype='leastcost'. NN=8 is most commonly used. Be aware that
#'  linear structures may cause artefacts in the least-cost paths, therefore
#'  inspect the actual least-cost paths in the provided output [default 8].
#' @param pathtype Type of cost distance to be calculated (based on function in
#'  the \code{gdistance} package. Available distances are 'leastcost', 'commute'
#'   or 'rSPDistance'. See functions in the gdistance package for futher
#'   explanations. If the path type is set to 'leastcost' then paths and also
#'   pathlength are returned [default 'leastcost'].
#' @param plotpath switch if least cost paths should be plotted (works only if
#' pathtype='leastcost'. Be aware this slows down the computation, but it is
#' recommended to do this to check least cost paths visually.
#' @param theta value needed for rSPDistance function. See
#' \code{\link[gdistance]{rSPDistance}} in package \code{gdistance} [default 1].
#' @param plot.colors.pop A color palette for population plots or a list with
#' as many colors as there are populations in the dataset
#' [default gl.colors("dis")].
#' @param raster.colors The color palette to use to color the raster values
#'  [default rev(terrain.colors(255))].
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' progress log; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#' @importFrom stats step
#' @importFrom sp Line Lines SpatialLines SpatialLinesLengths
#' @importFrom raster plot
#' @return Returns a list that consists of four pairwise distance matrices
#' (Euclidean, Cost, length of path and genetic) and the actual paths as spatial
#'  line objects.
#' @author Bernd Gruber (bugs? Post to
#' \url{https://groups.google.com/d/forum/dartr})
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
#' @examples
#' #this example takes about 20 seconds to run...
#' \donttest{
#' data(possums.gl)
#' library(raster)  #needed for that example
#' landscape.sim <- readRDS(system.file('extdata','landscape.sim.rdata', 
#' package='dartR.data'))
#' #use only 3 population (first 90 individuals) due to speed
#' glc <- gl.genleastcost(x=possums.gl,fric.raster=landscape.sim ,
#' gen.distance = 'D', NN=8, pathtype = 'leastcost',plotpath = TRUE)
#' #### run tests as implemented in PopGenreport (maybe need to install)
#' if (require("PopGenReport", quietly=TRUE)) {
#' PopGenReport::wassermann(eucl.mat = glc$eucl.mat, cost.mat = glc$cost.mats, 
#'  gen.mat = glc$gen.mat)
#' lgrMMRR(gen.mat = glc$gen.mat, cost.mats = glc$cost.mats,  
#' eucl.mat = glc$eucl.mat)
#' }
#' }
#' @export

gl.genleastcost <- function(x,
                            fric.raster,
                            gen.distance = "Gst.Nei",
                            NN = 8,
                            pathtype = "leastcost",
                            plotpath = TRUE,
                            theta = 1,
                            plot.colors.pop = gl.colors("dis"),
                            raster.colors = rev(terrain.colors(255)),
                            verbose = NULL) {
    
    # SET VERBOSITY
    verbose <- gl.check.verbosity(verbose)
    
    # FLAG SCRIPT START
    funname <- match.call()[[1]]
    utils.flag.start(func = funname,
                     build = "Jody",
                     verbose = verbose)
    
    # FUNCTION SPECIFIC ERROR CHECKING
    # CHECK IF PACKAGES ARE INSTALLED
    pkg <- "gdistance"
    if (!(requireNamespace(pkg, quietly = TRUE))) {
      cat(error(
        "Package",
        pkg,
        " needed for this function to work. Please install it.\n"
      ))
      return(-1)
    }
    
    pkg <- "mmod"
    if (!(requireNamespace(pkg, quietly = TRUE))) {
      cat(error(
        "Package",
        pkg,
        " needed for this function to work. Please install it.\n"
      ))
      return(-1)
    }
    
    if (is.null(NN) & pathtype == "leastcost") {
        stop(
            error(
                "NN is not specified!\nPlease specify the number of nearest neighbour to use for the least-cost path calculations (NN=4 or NN=8). If linear features are tested you may want to consider NN=4 otherwise NN=8 is the most commonly used and prefered option. In any case check the actual least-cost paths for artefacts by inspecting the plot on least-cost paths.\n"
            )
        )
    }
    
    dist.type <- NA
    if (gen.distance == "D" ||
        gen.distance == "Gst.Hedrick" ||
        gen.distance == "Gst.Nei")
        dist.type <- "pop"
    
    if (gen.distance == "kosman" ||
        gen.distance == "propShared" ||
        gen.distance == "dist" ) 
        dist.type <- "ind"
    
    if (is.na(dist.type)) {
        stop(
            error(
                "No valid genetic distance type was provided. Please check ?landgenreport for valid options\n"
            )
        )
    }
    
    if (is.null(x@other$xy)) {
        cat(
            warn(
                "No projected coordinates in @other$xy found. Hence will use latlons (if provided), which are not projected, hence there might be distortions if the area covered is large or close to the poles. Be aware your resistance layer and coordinates in the genlight object need to have the same coordinate system.\n"
            )
        )
        x@other$xy <- x@other$latlon[, c("lon", "lat")]
        if (is.null(x@other$xy)) {
            step(warn("No coordinates found in the genlight object!!\n"))
        }
    }
    
    if (dist.type == "pop") {
        # calculate the centers if population measurement is wanted
        c.x <- tapply(x@other$xy[, 1], x@pop, mean)
        c.y <- tapply(x@other$xy[, 2], x@pop, mean)
        cp <- cbind(c.x, c.y)
        eucl.mat <- as.matrix(dist(cp))
        dimnames(eucl.mat) <- list(popNames(x), popNames(x))
        npop <- length(levels(x@pop))
    } else {
        cp <- cbind(x@other$xy[, 1], x@other$xy[, 2])
        eucl.mat <- as.matrix(dist(cp))
        dimnames(eucl.mat) <- list(indNames(x), indNames(x))
        npop <- length(indNames(x))
    }
    
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

    # check if fric.raster is a stack or not...
    fric.raster <- raster::raster(fric.raster)
    mats <- list()
    mats.names <- NA
    mats.pathlength <- list()
    mats.paths <- list()
    
    pathlength.mat <- NULL
    paths <- NULL
    
    n.mats <-
        dim(fric.raster)[3]  #number of rasters in the stack
    
    for (ci in 1:n.mats) {
        raster::plot(fric.raster[[ci]],
                     col = raster.colors,
                     main = paste(names(fric.raster)[ci],
                                  ":", 
                                  pathtype, 
                                  ", NN=", 
                                  NN, 
                                  sep = ""))
        # image(fric.raster, col=fric.raster@legend@colortable, asp=1)
        
        points(
            x@other$xy,
            cex = 1,
            pch = 16,
            col = colors_pops
        )
        if (dist.type == "pop")
            points(cp,
                   cex = 1.5,
                   pch = 15,
                   col = "black")
        
        # create friction matrix
        fric.mat <-
            gdistance::transition(fric.raster[[ci]], function(x)
                1 / x[2], NN)
        
        # set distances to meters if not projected already
        fric.mat@crs@projargs <- "+proj=merc +units=m"
        fric.mat.cor <- gdistance::geoCorrection(fric.mat)
        
        if (pathtype == "leastcost") {
            cd.mat <- gdistance::costDistance(fric.mat.cor, cp, cp)
        }
        
        if (pathtype == "rSPDistance") {
            cd.mat <- gdistance::rSPDistance(fric.mat.cor, cp, cp, theta = 1)
        }
        
        if (pathtype == "commute") {
            cd.mat <- as.matrix(gdistance::commuteDistance(fric.mat.cor, cp))
        }
        
        dimnames(cd.mat) <- dimnames(eucl.mat)
        
        # only show paths if leastcost otherwise not possible
        if (pathtype == "leastcost" & plotpath == TRUE) {
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
    
    # mats[[n.mats+1]] <- eucl.mat mats.names[n.mats+1]<- 'Euclidean'
    names(mats) <- names(fric.raster)
    # put other calculations here....  Calculate genetic distances across 
    #subpopulations
    
    xx <- gl2gi(x, verbose = 0)
    
    if (gen.distance == "Gst.Nei") {
        gendist.mat <- as.matrix(mmod::pairwise_Gst_Nei(xx))
    }
    
    if (gen.distance == "Gst.Hedrick") {
        gendist.mat <- as.matrix(mmod::pairwise_Gst_Hedrick(xx))
    }
    
    if (gen.distance == "D") {
        gendist.mat <- as.matrix(mmod::pairwise_D(xx))
    }
    
    if (gen.distance == "kosman") {
        gendist.mat <- as.matrix(as.dist(gl.kosman(xx)$kosman))
    }
    
    if (gen.distance == "propShared") {
        gendist.mat <- as.matrix(as.dist(propShared(xx)))
    }
    
    if (gen.distance == "dist") {
      gendist.mat <- as.matrix(dist(as.matrix(xx)))
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
