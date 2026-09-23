#' @name gl2shp
#' @title Converts a genlight object to ESRI shapefiles or kml files
#' @family linker
#'
#' @description
#' This function exports the coordinates in a genlight object to a point shape
#' file or kml file, with the individual metrics as attributes.
#'
#' @details
#' Coordinates are taken from x@other$latlon (columns lat and lon; a column
#' named long is accepted for lon) and are assumed to be in WGS84 unless a
#' proj4 string is provided.
#'
#' Individuals with a missing lat or lon are removed. Missing values in other
#' individual metrics are kept and written as NA.
#'
#' The attribute table holds the columns of x@other$ind.metrics plus lat and
#' lon. If ind.metrics has no id column, an id column with the individual
#' names is added. ESRI shapefiles truncate column names to 10 characters.
#'
#' @param x Name of the genlight object containing the SNP data and location
#' data, lat longs [required].
#' @param type Type of output 'kml' or 'shp' [default 'shp'].
#' @param proj4 Proj4string of data set (see spatialreference.org for
#' projections) [default WGS84].
#' @param outfile Name (path) of the output shape file [default 'gl']. shp
#'  extension is added automatically.
#' @param outpath Path where to save the output file; the folder must exist
#' [default tempdir(), mandated by CRAN]. Use outpath=getwd() or outpath='.'
#' when calling this function to direct output files to your working directory.
#' @param verbose Verbosity: 0, silent or fatal errors; 1, begin and end; 2,
#' brief progress messages; 3, progress and results summary; 5, full report
#' [default 2, unless specified using gl.set.verbosity].
#'
#' @return A terra SpatVector with the exported points. The file is written to
#' outpath.
#'
#' @author Author(s): Bernd Gruber. Custodian: Bernd Gruber -- Post to
#' \url{https://groups.google.com/d/forum/dartr}
#'
#' @examples
#' out <- gl2shp(testset.gl, outpath=tempdir())
#'
#' @importFrom stats complete.cases
#' @export

gl2shp <- function(x,
                   type = "shp",
                   proj4 = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                   outfile = "gl",
                   outpath = tempdir(),
                   verbose = NULL) {

    # SET VERBOSITY
    verbose <- gl.check.verbosity(verbose)

    # FLAG SCRIPT START
    funname <- match.call()[[1]]
    utils.flag.start(func = funname,
                     verbose = verbose)

    # CHECK DATATYPE
    datatype <- utils.check.datatype(x, verbose = verbose)

    # CHECK IF PACKAGES ARE INSTALLED
    pkg <- "terra"
    if (!(requireNamespace(pkg, quietly = TRUE))) {
        stop(error(
            "Package",
            pkg,
            " needed for this function to work. Please install it.\n"
        ))
    }

    # FUNCTION SPECIFIC ERROR CHECKING

    if (length(type) != 1 || !(type %in% c("shp", "kml"))) {
        stop(error("Fatal Error: type must be 'shp' or 'kml'\n"))
    }

    if (!dir.exists(outpath)) {
        stop(error("Fatal Error: output folder does not exist:", outpath, "\n"))
    }

    if (is.null(x@other$latlon)) {
        stop(error(
            "Fatal Error: No coordinates provided in slot: gl@other$latlon\n"
        ))
    }

    # latlon may be stored as a matrix in objects not built by dartR
    latlon <- as.data.frame(x@other$latlon)

    if (nrow(latlon) != nInd(x)) {
        stop(
            error(
                "Fatal Error: Number of coordinates provided is different from the number of individuals in the data set\n"
            )
        )
    }

    # check if names are given as lat long instead lat lon
    if (!("lon" %in% names(latlon)) && "long" %in% names(latlon)) {
        if (verbose >= 2) {
            cat(warn(
                "  Warning: Names given as lat long, instead of lat lon. Rectifying\n"
            ))
        }
        names(latlon)[names(latlon) == "long"] <- "lon"
    }

    if (!all(c("lat", "lon") %in% names(latlon))) {
        stop(error(
            "Fatal Error: gl@other$latlon must have columns named lat and lon\n"
        ))
    }

    # DO THE JOB

    # Only the coordinates decide which individuals can be mapped; NA in other
    # individual metrics is written as NA
    keep <- complete.cases(latlon[, c("lat", "lon")])
    if (!any(keep)) {
        stop(error("Fatal Error: No individuals with complete coordinates\n"))
    }
    toremove <- which(!keep)
    if (verbose >= 1 && length(toremove) > 0) {
        cat(warn(
            "  Warning: Removed",
            length(toremove),
            "individual(s) due to missing coordinates:",
            paste(indNames(x)[toremove], collapse = ", "),
            "\n"
        ))
    }

    glpoints <- x@other$ind.metrics
    if (is.null(glpoints)) {
        glpoints <- data.frame(row.names = seq_len(nInd(x)))
    }
    glpoints <- as.data.frame(glpoints)
    if (!("id" %in% names(glpoints))) {
        glpoints <- cbind(id = indNames(x), glpoints)
    }
    glpoints$lat <- latlon$lat
    glpoints$lon <- latlon$lon
    glpoints <- glpoints[keep, , drop = FALSE]

    v <- terra::vect(
        glpoints,
        geom = c("lon", "lat"),
        crs = proj4,
        keepgeom = TRUE
    )

    outfilespec <- paste0(file.path(outpath, outfile), ".", type)
    filetype <- c(shp = "ESRI Shapefile", kml = "KML")[[type]]
    terra::writeVector(
        v,
        filename = outfilespec,
        filetype = filetype,
        overwrite = TRUE
    )

    if (verbose >= 2) {
        cat(report(
            paste(
                c(shp = "  Shapefile", kml = "  KML file")[[type]],
                "saved as:",
                paste0(outfile, ".", type),
                "\n  in folder:",
                outpath,
                "\n"
            )
        ))
    }

    # FLAG SCRIPT END

    if (verbose > 0) {
        cat(report("Completed:", funname, "\n"))
    }

    return(v)
}
