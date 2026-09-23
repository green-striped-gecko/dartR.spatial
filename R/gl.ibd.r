#' Performs isolation by distance analysis
#'
#' This function performs an isolation by distance analysis based on a Mantel
#' test and also produces an isolation by distance plot. If a genlight object
#' with coordinates is provided, then geographic and genetic distance matrices
#' are calculated.
#' @importFrom vegan mantel
#' @importFrom MASS kde2d
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline title points
#' @importFrom stats as.dist lm
#' @importFrom StAMPP stamppFst stamppNeisD
#' @importFrom stats coef
#' @param x Genlight object used to calculate missing distances. Ignored when
#' both Dgen and Dgeo are supplied [default NULL].
#' @param distance Distance to calculate: population-based 'Fst'
#' [\link[StAMPP]{stamppFst}] or 'D' [\link[StAMPP]{stamppNeisD}], or
#' individual-based 'propShared' (1 - gl.propShared), 'euclidean'
#' [\link[stats]{dist}] or 'kosman' [gl.kosman] [default "Fst"].
#' @param coordinates 'latlon', 'xy', or a two-column data.frame named
#' lat/lon or x/y. Stored coordinates in x@other$latlon or x@other$xy follow
#' individual order. Explicit data.frames with row names are matched to
#' indNames(x); automatic row names use positional order. All used coordinates
#' must be numeric and finite. Longitude/latitude gives geodesic distances in
#' metres (population centres are the mean longitude and latitude); x/y gives
#' Euclidean distances in coordinate units [default "latlon"].
#' @param Dgen Genetic distances as a dist object or symmetric numeric square
#' matrix. If NULL, calculated from x [default NULL].
#' @param Dgeo Geographic distances as a dist object or symmetric numeric square
#' matrix. If NULL, calculated from coordinates [default NULL].
#' @param Dgeo_trans R expression transforming Dgeo, for example 'log(Dgeo)'.
#' The default leaves distances unchanged [default "Dgeo"].
#' @param Dgen_trans R expression transforming Dgen, for example
#' 'Dgen/(1-Dgen)'. The default leaves distances unchanged [default "Dgen"].
#' @param permutations Number of permutations in the Mantel test [default 999].
#' @param plot.out Display the isolation by distance plot
#' [default TRUE].
#' @param paircols Colour points by 'pop'ulation or 'ind'ividual pairs.
#' For individual distances calculated using x, 'pop' uses population membership.
#' With both distance matrices supplied, x is ignored and distance labels are
#' used. NULL uses uncoloured points [default NULL].
#' @param plot.theme ggplot2 theme for the plot
#' [default theme_dartR()].
#' @param plot.dir Directory in which to save files [default = working directory]
#' @param plot.file Name for the RDS binary file to save (base name only, exclude extension) [default NULL]
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#' @details
#' Both transformations default to the identity: Fst is not linearised and
#' geographic distance is not logged unless requested. The Euclidean genetic
#' option uses stats::dist on the genotype matrix. For pairs with missing loci,
#' squared differences are scaled by the total number of loci divided by the
#' number compared; this differs from gl.dist.ind. Population geographic
#' distances use the mean coordinates of each population.
#'
#' Labelled distance inputs must have unique, matching identities. Geographic
#' distances are reordered to match genetic distances. When either input lacks
#' labels, its supplied order is used; callers must ensure correspondence.
#' At least three individuals or populations, finite distances for every pair,
#' and variation in both distance vectors are required after transformation.
#' Missing pairs are rejected, not omitted from the Mantel test. Finite negative
#' Fst estimates and transformed distances are permitted.
#'
#' The Mantel test uses vegan::mantel. Terra is needed for geodesic distances.
#' If an explicitly requested log(Dgeo) produces log(0), choose a scientifically
#' appropriate transformation such as log(Dgeo + 1). No offset is added
#' automatically. A plot is constructed only when plot.out is TRUE or plot.file
#' is supplied. plot.file saves the plot as RDS even when plot.out is FALSE.
#' @return A list with Dgen and Dgeo (the transformed, aligned dist objects)
#' and mantel (the Mantel test result). The plot is displayed or saved separately.
#' @export
#' @author Bernd Gruber (bugs? Post to
#' \url{https://groups.google.com/d/forum/dartr})
#' @seealso \link[vegan]{mantel}, \link[StAMPP]{stamppFst}
#' @references
#' Rousset, F. (1997). Genetic differentiation and estimation of gene flow from
#' F-statistics under isolation by distance. Genetics, 145(4), 1219-1228.
#' @examples
#'  \donttest{
#' #because of speed only the first 100 loci
#' #' if (isTRUE(getOption("dartR_fbm"))) bandicoot.gl <- gl.gen2fbm(bandicoot.gl)
#' ibd <- gl.ibd(bandicoot.gl[,1:100], Dgeo_trans='log(Dgeo)' ,
#' Dgen_trans='Dgen/(1-Dgen)')
#' #because of speed only the first 10 individuals)
#' ibd <- gl.ibd(bandicoot.gl[1:10,], distance='euclidean', paircols='pop',
#' Dgeo_trans='Dgeo')
#' }
#' #only first 100 loci
#' ibd <- gl.ibd(bandicoot.gl[,1:100], paircols='pop')

gl.ibd <- function(x = NULL,
                   distance = "Fst",
                   coordinates = "latlon",
                   Dgen = NULL,
                   Dgeo = NULL,
                   Dgeo_trans = "Dgeo",
                   Dgen_trans = "Dgen",
                   permutations = 999,
                   plot.out = TRUE,
                   paircols = NULL,
                   plot.theme = theme_dartR(),
                   plot.file = NULL,
                   plot.dir = NULL,
                   verbose = NULL) {
    # SET VERBOSITY AND FLAG SCRIPT START
    verbose <- gl.check.verbosity(verbose)
    funname <- match.call()[[1]]
    if (is.function(funname)) funname <- "gl.ibd"
    utils.flag.start(func = funname, verbose = verbose)

    # CHECK INPUT MODE AND DISTANCE IDENTITIES
    supplied <- !is.null(Dgen) && !is.null(Dgeo)
    if (!supplied) {
        if (!is(x, "genlight")) {
            stop(error(paste0(
                "Provide both Dgen and Dgeo, or a genlight x to calculate missing ",
                "distances.\n")))
        }
        dt <- utils.check.datatype(x, verbose = 0)
        if (length(distance) != 1L || is.na(distance) ||
            !distance %in% c("Fst", "D", "propShared", "euclidean", "kosman")) {
            stop(error("distance must be Fst, D, propShared, euclidean or kosman.\n"))
        }
    }
    check.ids <- function(ids, label) {
        if (!is.null(ids) &&
            (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids))) {
            stop(error(label, "must have unique, non-missing labels.\n"))
        }
    }
    as.distance <- function(value, label) {
        if (inherits(value, "dist")) {
            size <- attr(value, "Size")
            if (!is.numeric(value) || !is.numeric(size) || length(size) != 1L ||
                is.na(size) || !is.finite(size) || size < 2 ||
                size != floor(size) || length(value) != size * (size - 1) / 2) {
                stop(error(label, "is not a valid dist object.\n"))
            }
            ids <- attr(value, "Labels")
            if (!is.null(ids) && length(ids) != size) {
                stop(error(label, "labels must match its size.\n"))
            }
            check.ids(ids, label)
            return(value)
        }
        if (!is.matrix(value) || !is.numeric(value) ||
            nrow(value) != ncol(value) || nrow(value) < 2) {
            stop(error(label, "must be a dist object or numeric square matrix.\n"))
        }
        rows <- rownames(value)
        cols <- colnames(value)
        check.ids(rows, label)
        check.ids(cols, label)
        if (!is.null(rows) && !is.null(cols)) {
            if (!setequal(rows, cols)) {
                stop(error(label, "row and column labels must match.\n"))
            }
            value <- value[, match(rows, cols), drop = FALSE]
        }
        ids <- if (!is.null(rows)) rows else cols
        dimnames(value) <- list(ids, ids)
        # Only off-diagonal pairs matter, including after log transformations.
        lower <- lower.tri(value)
        if (!isTRUE(all.equal(value[lower], t(value)[lower],
                              check.attributes = FALSE))) {
            stop(error(label, "must be symmetric.\n"))
        }
        return(stats::as.dist(value))
    }
    align.distances <- function(gen, geo) {
        if (attr(gen, "Size") != attr(geo, "Size")) {
            stop(error("Dgen and Dgeo must have the same number of observations.\n"))
        }
        gen.ids <- attr(gen, "Labels")
        geo.ids <- attr(geo, "Labels")
        if (!is.null(gen.ids) && !is.null(geo.ids)) {
            if (!setequal(gen.ids, geo.ids)) {
                stop(error("Dgen and Dgeo labels must identify the same observations.\n"))
            }
            if (!identical(gen.ids, geo.ids)) {
                order <- match(gen.ids, geo.ids)
                geo <- stats::as.dist(as.matrix(geo)[order, order, drop = FALSE])
            }
        }
        return(list(gen = gen, geo = geo))
    }
    if (!is.null(Dgen)) Dgen <- as.distance(Dgen, "Dgen")
    if (!is.null(Dgeo)) Dgeo <- as.distance(Dgeo, "Dgeo")

    # CALCULATE ONLY THE MISSING DISTANCES
    coordstring <- "Dgeo provided."
    if (supplied) {
        typedis <- "ind"
        distance <- "Dgen provided"
        if (verbose >= 2) {
            cat(report("Using supplied distance matrices; x is ignored.\n"))
        }
    } else {
        typedis <- if (distance %in% c("Fst", "D")) "pop" else "ind"
        if (typedis == "pop" && nPop(x) < 3) {
            stop(error("Population distances require at least three populations.\n"))
        }
        if (typedis == "ind" && nInd(x) < 3) {
            stop(error("Individual distances require at least three individuals.\n"))
        }
        if (is.null(Dgeo)) {
            explicit <- is.data.frame(coordinates)
            if (explicit) {
                coords <- coordinates
                if (setequal(names(coords), c("lon", "lat"))) {
                    projection <- TRUE
                } else if (setequal(names(coords), c("x", "y"))) {
                    projection <- FALSE
                } else {
                    stop(error("coordinates must have columns lat/lon or x/y.\n"))
                }
                coordstring <- "coordinates data.frame"
            } else if (is.character(coordinates) && length(coordinates) == 1L &&
                       !is.na(coordinates) && coordinates %in% c("latlon", "xy")) {
                projection <- coordinates == "latlon"
                coords <- if (projection) x@other$latlon else x@other$xy
                if (projection && !is.null(coords) &&
                    all(c("lon", "lat") %in% colnames(coords))) {
                    coords <- coords[, c("lon", "lat"), drop = FALSE]
                }
                coordstring <- paste0("x@other$", coordinates)
            } else {
                stop(error(paste0(
                    "coordinates must be latlon, xy or a two-column data.frame.\n")))
            }
            if (is.null(coords) || length(dim(coords)) != 2L ||
                nrow(coords) != nInd(x) || ncol(coords) != 2L) {
                stop(error("Provide two coordinates for every individual.\n"))
            }
            ids <- indNames(x)
            check.ids(ids, "Individual names")
            # Stored metadata follows genotype order. Only explicit tables carry
            # independent identities; automatic data.frame row names are positional.
            if (explicit && .row_names_info(coords) > 0L) {
                check.ids(rownames(coords), "Coordinate row names")
                if (!setequal(rownames(coords), ids)) {
                    stop(error("Coordinate row names must match indNames(x).\n"))
                }
                coords <- coords[match(ids, rownames(coords)), , drop = FALSE]
            }
            if (!all(vapply(as.data.frame(coords), is.numeric, logical(1)))) {
                stop(error("Coordinates must be numeric.\n"))
            }
            coords <- as.matrix(coords)
            check.coords <- function(value) {
                bad <- which(rowSums(!is.finite(value)) > 0)
                if (length(bad)) {
                    affected <- if (is.null(ids)) bad else ids[bad]
                    stop(error(paste0("Non-finite coordinates for individuals: ",
                                      paste(affected, collapse = ", "), ".\n")))
                }
            }
            check.coords(coords)
            if (projection) {
                if (!all(c("lon", "lat") %in% colnames(coords))) {
                    stop(error(paste0(
                        "Latitude/longitude coordinates need lon and lat columns.\n")))
                }
                if (!requireNamespace("terra", quietly = TRUE)) {
                    stop(error(paste0(
                        "Install package terra to calculate geodesic distances.\n")))
                }
                coords <- coords[, c("lon", "lat"), drop = FALSE]
                if (any(abs(coords[, "lon"]) > 180 | abs(coords[, "lat"]) > 90)) {
                    stop(error(paste0(
                        "Longitude/latitude must be WGS84 degrees (|lon| <= 180, ",
                        "|lat| <= 90).\n")))
                }
                coordstring <- paste(coordstring, "(geodesic distances)")
            }
            rownames(coords) <- ids
            # geodesic distances in metres for lon/lat, Euclidean for x/y
            geo.dist <- function(value) {
                if (projection) {
                    D <- as.matrix(terra::distance(value, lonlat = TRUE))
                    dimnames(D) <- list(rownames(value), rownames(value))
                    stats::as.dist(D)
                } else {
                    stats::dist(value)
                }
            }
            if (typedis == "pop") {
                pop.xy <- apply(coords, 2, function(a) tapply(a, pop(x), mean))
                if (any(!is.finite(pop.xy))) {
                    stop(error(paste0(
                        "Population mean coordinates must be finite; check population ",
                        "membership.\n")))
                }
                Dgeo <- geo.dist(pop.xy)
            } else {
                Dgeo <- geo.dist(coords)
            }
        }
        if (is.null(Dgen)) {
            helper.verbose <- if (verbose >= 2) verbose else 0
            if (distance %in% c("Fst", "D")) {
                if (methods::.hasSlot(x, "fbm") &&
                    !is.null(methods::slot(x, "fbm"))) {
                    x <- gl.fbm2gen(x, verbose = helper.verbose)
                }
                class(x) <- "genlight" # StAMPP requires the base class.
                Dgen <- if (distance == "Fst") {
                    stats::as.dist(StAMPP::stamppFst(x, nboots = 1))
                } else {
                    stats::as.dist(StAMPP::stamppNeisD(x, pop = TRUE))
                }
            } else if (distance == "propShared") {
                Dgen <- stats::as.dist(1 - gl.propShared(x))
            } else if (distance == "euclidean") {
                Dgen <- stats::dist(as.matrix(x))
            } else {
                Dgen <- stats::as.dist(gl.kosman(x, verbose = helper.verbose)$kosman)
            }
        }
        Dgen <- as.distance(Dgen, "Dgen")
        Dgeo <- as.distance(Dgeo, "Dgeo")
        # Preserve the historical alphabetical order for population results.
        if (typedis == "pop" && !is.null(attr(Dgen, "Labels"))) {
            order <- order(attr(Dgen, "Labels"))
            Dgen <- stats::as.dist(as.matrix(Dgen)[order, order, drop = FALSE])
            if (is.null(attr(Dgeo, "Labels"))) {
                Dgeo <- stats::as.dist(as.matrix(Dgeo)[order, order, drop = FALSE])
                attr(Dgeo, "Labels") <- NULL
            }
        }
    }
    aligned <- align.distances(Dgen, Dgeo)
    Dgen <- aligned$gen
    Dgeo <- aligned$geo

    # TRANSFORM, NORMALISE AND CHECK STATISTICAL PRECONDITIONS
    Dgen <- as.distance(eval(parse(text = Dgen_trans)), "Transformed Dgen")
    Dgeo <- as.distance(eval(parse(text = Dgeo_trans)), "Transformed Dgeo")
    aligned <- align.distances(Dgen, Dgeo)
    Dgen <- aligned$gen
    Dgeo <- aligned$geo
    if (attr(Dgen, "Size") < 3L) {
        stop(error("Mantel analysis requires at least three observations.\n"))
    }
    for (label in c("Dgen", "Dgeo")) {
        value <- get(label)
        if (any(!is.finite(value))) {
            stop(error(label, paste0(
                "contains non-finite pairwise distances after transformation. Check ",
                "missing data and transformations (including log(0)); no pairs are ",
                "omitted.\n")))
        }
        if (length(unique(as.numeric(value))) < 2L) {
            stop(error(label, "must vary across observation pairs for a Mantel test.\n"))
        }
    }
    manteltest <- if (verbose >= 2) {
        vegan::mantel(Dgen, Dgeo, na.rm = FALSE, permutations = permutations)
    } else {
        suppressMessages(vegan::mantel(Dgen, Dgeo, na.rm = FALSE,
                                      permutations = permutations))
    }

    # BUILD A PLOT ONLY WHEN DISPLAYING OR SAVING IT
    if (plot.out || !is.null(plot.file)) {
        lm_eqn <-
            function(df,
                     r = manteltest$statistic,
                     pp = manteltest$signif) {
                m <- lm(Dgen ~ Dgeo, df)
                eq <-
                    substitute(
                        italic(y) == a + b %.% italic(x) * "," ~ ~ italic(R) ^ 2 ~ "=" ~ r2 * "," ~ ~
                            italic(p) ~ "=" ~ pp,
                        list(
                            a = format(unname(coef(m)[1]),
                                       digits = 2),
                            b = format(unname(coef(m)[2]), digits = 2),
                            r2 = format(summary(m)$r.squared, digits = 3),
                            pp = format(pp, digits = 3)
                        )
                    )
                as.character(as.expression(eq))
            }

        # Plot the aligned pairwise distances.

        res <-
            data.frame(Dgen = as.numeric(Dgen), Dgeo = as.numeric(Dgeo))
        if (is.null(paircols)) {

            p3 <-
                ggplot(res, aes(x = Dgeo, y = Dgen)) +
              geom_point() +
              geom_smooth(method = "lm", se = TRUE) +
              ylab(Dgen_trans) +
              xlab(Dgeo_trans) +
                annotate(
                    "text",
                    label = lm_eqn(res),
                    x = Inf,
                    y = -Inf,
                    parse = TRUE,
                    hjust = 1.05,
                    vjust = 0) +
              plot.theme

        } else {
            Legend <- col2 <- NA  #ggplot bug
            cols <- which(lower.tri(as.matrix(Dgen)), arr.ind = T)
            c1 <- cols[, 2]
            c2 <- cols[, 1]
            cn <- colnames(as.matrix(Dgen))
            # if someone wants to color pairwise individuals by pairwise colors
            if (typedis == "ind" & paircols == "pop") {
                if (!supplied && is(x, "genlight"))
                    cn <- pop(x)[match(cn, indNames(x))]
                else
                    cn <-rownames(as.matrix(Dgen))
            }
            res <-
                data.frame(
                    Dgen = as.numeric(Dgen),
                    Dgeo = as.numeric(Dgeo),
                    Legend = cn[c1],
                    col2 = cn[c2]
                )
            p3 <-
                ggplot(res) +
              geom_point(aes(Dgeo, Dgen, col = Legend), size = 5) +
              geom_point(aes(Dgeo, Dgen, col = col2), size = 2) +
              geom_point(aes(Dgeo, Dgen),size = 2,shape = 1) +
              guides(size = "none",color = guide_legend(title = "Populations")) +
              geom_smooth(aes(x = Dgeo, y = Dgen),method = "lm", se = TRUE) +
              ylab(Dgen_trans) +
              annotate("text",label = lm_eqn(res),
                       x = Inf,
                       y = -Inf,
                       parse = TRUE,
                       hjust = 1.05,
                       vjust = 0) +
              xlab(Dgeo_trans) + plot.theme

        }

        if (plot.out) {
            suppressMessages(print(p3))
        }

    }
    if (verbose >= 3) {
        cat(report("  Coordinates used from:", coordstring, "\n"))
        cat(report("  Transformation of Dgeo:", Dgeo_trans, "\n"))
        cat(report("  Genetic distance:", distance, "\n"))
        cat(report("  Transformation of Dgen:", Dgen_trans, "\n"))
        print(manteltest)
    }
    if (!is.null(plot.file)) {
        plot.dir <- gl.check.wd(plot.dir, verbose = 0)
        utils.plot.save(p3, dir = plot.dir, file = plot.file, verbose = verbose)
    }
    if (verbose >= 1) cat(report("\nCompleted:", funname, "\n\n"))
    return(list(Dgen = Dgen, Dgeo = Dgeo, mantel = manteltest))
}
