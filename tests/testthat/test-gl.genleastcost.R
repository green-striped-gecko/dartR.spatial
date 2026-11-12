# Characterisation captured before critical source review at f2cfb52.
# Snapshots record current behaviour, defects included.
# Approved snapshot diffs: RasterLayer input now runs (change 1); cost.mats
# and path lengths equal gl.costdistances, and rSPDistance at theta = 1
# underflows on this landscape (change 2); propShared is 1 - similarity
# (change 3); kosman runs (change 4). D, Gst and dist gen.mat unchanged.
summarise_glc <- function(result) {
  if (inherits(result, "error")) return(list(error = conditionMessage(result)))
  out <- list(names = names(result))
  for (nm in names(result)) {
    el <- result[[nm]]
    if (is.list(el) && !is.data.frame(el)) {
      out[[nm]] <- lapply(el, function(e) {
        if (is.numeric(e)) round(head(as.numeric(as.matrix(e)), 12), 5)
        else class(e)
      })
    } else if (is.numeric(el) || inherits(el, "dist")) {
      out[[nm]] <- list(dim = dim(as.matrix(el)),
                        head = round(head(as.numeric(as.matrix(el)), 12), 5))
    } else {
      out[[nm]] <- class(el)
    }
  }
  out
}

test_that("gl.genleastcost reference-data behaviour is unchanged", {
  local_edition(3)
  skip_if_not_installed("gdistance")
  possums <- dartR.data::possums.gl
  keep <- unlist(lapply(c("A", "B", "C"), function(p)
    head(which(pop(possums) == p), 10)))
  x <- possums[keep, 1:100]
  x@other$xy <- possums@other$xy[keep, ]
  fric <- readRDS(system.file("extdata", "landscape.sim.rdata",
                              package = "dartR.data"))
  fric <- raster::aggregate(fric, 5)
  fric_terra <- terra::rast(fric)
  before <- serialize(x, NULL)
  cases <- list(
    c("D", "leastcost", FALSE), c("Gst.Nei", "leastcost", FALSE),
    c("Gst.Hedrick", "leastcost", FALSE), c("kosman", "leastcost", FALSE),
    c("propShared", "leastcost", FALSE), c("Gst.Nei", "commute", FALSE),
    c("Gst.Nei", "rSPDistance", FALSE), c("D", "leastcost", TRUE),
    c("dist", "leastcost", FALSE)
  )
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (input in c("raster", "terra")) for (cs in cases) {
    if (input == "raster" && cs[1] != "Gst.Nei") next
    set.seed(2026)
    result <- tryCatch(
      gl.genleastcost(x, fric.raster = if (input == "raster") fric else fric_terra, gen.distance = cs[1],
                      pathtype = cs[2], plotpath = as.logical(cs[3]),
                      verbose = 0),
      error = identity)
    expect_identical(serialize(x, NULL), before)
    summary <- c(list(case = cs), summarise_glc(result))
    expect_snapshot(print(summary), variant = paste("glc", input, cs[1], cs[2],
                                                    cs[3], sep = "-"))
  }
})
