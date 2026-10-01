# Regression checks for approved findings F1-F9 and changes 11-13. These use a disposable
# executable placeholder and recorded process/plot calls; real integration
# checks remain opt-in in test-gl.run.eems.R.
eems_case <- function() {
  skip_if_not_installed("reemsplots2")
  skip_if_not_installed("sf")
  root <- tempfile("eems-fixes-")
  dir.create(root)
  binary <- file.path(root, if (.Platform$OS.type == "windows")
    "runeems_snps.exe" else "runeems_snps")
  file.create(binary)
  Sys.chmod(binary, "0755")
  calls <- new.env()
  calls$status <- 0L
  calls$omit_outputs <- FALSE
  calls$runs <- character()
  calls$plots <- 0L
  f <- gl.run.eems
  mock_system <- function(command, args, stdout, stderr) {
    calls$command <- command
    calls$args <- args
    params <- readLines(gsub("^['\"]|['\"]$", "", args[2]))
    output <- sub("mcmcpath = ", "", params[startsWith(params, "mcmcpath = ")],
                  fixed = TRUE)
    calls$runs <- c(calls$runs, dirname(output))
    writeLines("mock EEMS diagnostic", stdout)
    if (calls$status == 0L && !calls$omit_outputs) {
      dir.create(output)
      fixture <- system.file("extdata", "EEMS-example", package = "reemsplots2")
      stopifnot(nzchar(fixture))
      file.copy(list.files(fixture, full.names = TRUE), output)
    }
    calls$status
  }
  environment(f) <- list2env(list(system2 = mock_system),
                             parent = environment(gl.run.eems))
  plotter <- reemsplots2::make_eems_plots
  body(plotter) <- quote({
    calls$plots <- calls$plots + 1L
    calls$prob_level <- prob_level
    calls$add_abline <- add_abline
    cat("mock plotting progress\n")
    message("mock plot message")
    warning("mock plot warning")
    setNames(rep(list(ggplot2::ggplot()), 8),
             c("mrates01", "mrates02", "qrates01", "qrates02",
               "rdist01", "rdist02", "rdist03", "pilogl01"))
  })
  environment(plotter) <- environment()
  list(f = f, plotter = plotter, calls = calls, root = root,
       args = list(x = bandicoot.gl[1:12, 1:200], eems.path = root,
                   out.dir = root, plot.dir = root, verbose = 0,
                   nDemes = 20, numMCMCIter = 40, numBurnIter = 10,
                   numThinIter = 2, seed = 17, dpi = 10))
}

test_that("failed and incomplete runs cannot reach old plots (F1)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  expect_length(do.call(z$f, z$args), 8L)
  first <- z$calls$runs[1]
  z$calls$status <- 42L
  expect_error(do.call(z$f, z$args), "EEMS failed \\(exit status 42\\)")
  expect_equal(z$calls$plots, 1L)
  expect_false(identical(z$calls$runs[2], first))
  expect_true(file.exists(file.path(z$calls$runs[2], "eems.log")))
  expect_true(file.exists(file.path(z$calls$runs[2], "eems.diffs")))
  z$calls$status <- 0L
  z$calls$omit_outputs <- TRUE
  expect_error(do.call(z$f, z$args), "EEMS output is incomplete")
  expect_equal(z$calls$plots, 1L)
  expect_true(dir.exists(file.path(first, "data_eems")))
})

test_that("cleanup preserves unrelated files, prior runs and results (F2)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  sentinel <- file.path(z$root, "unrelated_eems_notes.txt")
  writeLines("keep", sentinel)
  do.call(z$f, z$args)
  do.call(z$f, z$args)
  expect_equal(length(unique(z$calls$runs)), 2L)
  expect_identical(readLines(sentinel), "keep")
  for (run in z$calls$runs) {
    expect_true(file.exists(file.path(run, "data_eems", "mcmcpilogl.txt")))
    expect_true(file.exists(file.path(run, "eems.log")))
    expect_false(file.exists(file.path(run, "eems.diffs")))
    expect_false(file.exists(file.path(run, "params.ini")))
  }
  z$args$cleanup <- FALSE
  do.call(z$f, z$args)
  expect_true(all(file.exists(file.path(tail(z$calls$runs, 1),
    c("eems.diffs", "eems.outer", "eems.coord", "params.ini")))))
})

test_that("ploidy and dependency checks fail before running (F3/F8)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  z$args$x <- testset.gs[1:8, 1:60]
  expect_error(do.call(z$f, z$args), "requires input ploidy 2")
  z$args$diploid <- NA
  expect_error(do.call(z$f, z$args), "diploid must be TRUE or FALSE")
  expect_length(z$calls$runs, 0L)
  z$args$diploid <- FALSE
  expect_length(do.call(z$f, z$args), 8L)
  for (pkg in c("reemsplots2", "sf")) {
    f <- z$f
    environment(f) <- list2env(list(requireNamespace = function(package, ...)
      if (package == pkg) FALSE else base::requireNamespace(package, ...)),
      parent = environment(z$f))
    expect_error(do.call(f, z$args), paste0("Package ", pkg, " is required"))
  }
  expect_length(z$calls$runs, 1L)
})

test_that("relative paths, spaces and NULL plot saving work (F4/F5/F7)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  withr::local_dir(z$root)
  dir.create("relative output")
  z$args$out.dir <- "relative output"
  z$args$plot.dir <- "relative output"
  do.call(z$f, z$args)
  # compare normalised paths: Windows reports short names and "\\"
  expect_identical(normalizePath(getwd(), winslash = "/"),
                   normalizePath(z$root, winslash = "/"))
  expect_identical(normalizePath(dirname(z$calls$runs[1]), winslash = "/"),
                   normalizePath("relative output", winslash = "/"))
  expect_false(file.exists(file.path("relative output", "eems.RDS")))
  z$args$plot.file <- "review output"
  expect_length(do.call(z$f, z$args), 8L)
  expect_true(file.exists(file.path("relative output", "review output.RDS")))
  expect_identical(z$calls$args[2],
    shQuote(file.path(tail(z$calls$runs, 1), "params.ini")))
  for (name in c("../bad", "sub/bad", "sub\\bad")) {
    z$args$plot.file <- name
    expect_error(do.call(z$f, z$args), "without path separators")
  }
  z$args$plot.file <- NULL
  z$args$out.dir <- "does-not-exist"
  expect_error(do.call(z$f, z$args), "out.dir must name an existing directory")
})

test_that("plot extras are used or rejected before execution (F6)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  do.call(z$f, c(z$args, list(prob_level = 0.75, add_abline = TRUE)))
  expect_equal(z$calls$prob_level, 0.75)
  expect_true(z$calls$add_abline)
  for (extra in list(list(unknown = TRUE), list(longlat = FALSE),
                     setNames(list(0.5, 0.6), c("prob_level", "prob_level")))) {
    expect_error(do.call(z$f, c(z$args, extra)),
                 "Extra plotting arguments must have unique supported names")
  }
  expect_length(z$calls$runs, 1L)
})

test_that("quiet mode logs routine diagnostics without printing them (F9)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  expect_output(expect_message(expect_warning(
    result <- do.call(z$f, z$args), NA), NA), NA)
  expect_length(result, 8L)
  log <- readLines(file.path(z$calls$runs[1], "eems.log"))
  expect_true(any(grepl("mock EEMS diagnostic", log)))
  expect_true(any(grepl("mock plotting progress", log)))
  expect_true(any(grepl("mock plot message", log)))
  expect_true(any(grepl("mock plot warning", log)))
  z$args$verbose <- 1
  run_eems <- z$f
  expect_output(do.call("run_eems", z$args), "Completed:")
})

test_that("EEMS receives lon/lat with great-circle distance (change 12)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  z$args$cleanup <- FALSE
  x <- z$args$x
  ll <- cbind(x@other$latlon$lon, x@other$latlon$lat)
  for (buffer in c(10000, 50000)) {
    do.call(z$f, c(z$args, list(buffer = buffer)))
    run <- tail(z$calls$runs, 1)
    coord <- as.matrix(utils::read.table(file.path(run, "eems.coord")))
    expect_equal(unname(coord), ll)
    expect_true("distance = greatcirc" %in%
                  readLines(file.path(run, "params.ini")))
    # buffer is ground metres: the samples on the hull sit buffer metres
    # from the outline whatever their latitude.
    outer <- as.matrix(utils::read.table(file.path(run, "eems.outer")))
    edge <- sf::st_sfc(sf::st_linestring(outer), crs = 4326)
    pts <- sf::st_as_sf(data.frame(lon = ll[, 1], lat = ll[, 2]),
                        coords = c("lon", "lat"), crs = 4326)
    gap <- min(as.numeric(sf::st_distance(pts, edge)))
    expect_equal(gap, buffer, tolerance = 0.02)
  }
})

test_that("habitat replaces the hull and is validated first (change 13)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  z$args$cleanup <- FALSE
  box <- rbind(c(110, -40), c(155, -40), c(155, -15), c(110, -15))
  do.call(z$f, c(z$args, list(habitat = box, buffer = 1e6)))
  outer <- as.matrix(utils::read.table(
    file.path(tail(z$calls$runs, 1), "eems.outer")))
  expect_equal(unname(outer), rbind(box, box[1, ]))
  # An sf polygon in a projected CRS is written in lon/lat.
  albers <- sf::st_transform(
    sf::st_sfc(sf::st_polygon(list(rbind(box, box[1, ]))), crs = 4326), 3577)
  do.call(z$f, c(z$args, list(habitat = sf::st_sf(geometry = albers))))
  outer <- as.matrix(utils::read.table(
    file.path(tail(z$calls$runs, 1), "eems.outer")))
  expect_equal(unname(outer), rbind(box, box[1, ]), tolerance = 1e-6)
  runs <- length(z$calls$runs)
  ring <- function(m) rbind(m, m[1, ])
  hole <- sf::st_polygon(list(ring(box), ring(rbind(
    c(120, -30), c(120, -25), c(130, -25), c(130, -30)))))
  two <- sf::st_multipolygon(list(list(ring(box)), list(ring(box + 50))))
  bowtie <- rbind(c(110, -40), c(155, -15), c(155, -40), c(110, -15))
  bad <- list(
    list(sf::st_sfc(sf::st_polygon(list(ring(box)))), "no coordinate"),
    list(sf::st_sfc(hole, crs = 4326), "must not contain holes"),
    list(sf::st_sfc(two, crs = 4326), "single polygon"),
    list(cbind(box, 1), "two-column matrix"),
    list(box * 2, "two-column matrix"),
    list(bowtie, "valid simple polygon"))
  for (case in bad) {
    expect_error(do.call(z$f, c(z$args, list(habitat = case[[1]]))),
                 case[[2]])
  }
  expect_length(z$calls$runs, runs)
})

test_that("samples outside the habitat are named in a warning (change 13)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  x <- z$args$x
  west <- adegenet::indNames(x)[x@other$latlon$lon < 130]
  east <- rbind(c(130, -45), c(160, -45), c(160, -10), c(130, -10))
  z$args$verbose <- 2
  # The mock plotter's own message and warning surface at verbose 2 (F9).
  out <- utils::capture.output(suppressMessages(suppressWarnings(
    result <- do.call(z$f, c(z$args, list(habitat = east))))))
  expect_length(result, 8L)
  expect_true(any(grepl(paste0(length(west),
                               " sample\\(s\\) fall outside the habitat"),
                        out)))
  expect_true(any(grepl(west[1], out, fixed = TRUE)))
  z$args$verbose <- 0
  expect_output(do.call(z$f, c(z$args, list(habitat = east))), NA)
})

test_that("do.call with the function object prints its name (change 11)", {
  z <- eems_case()
  local_mocked_bindings(make_eems_plots = z$plotter, .package = "reemsplots2")
  z$args$verbose <- 1
  expect_output(do.call(z$f, z$args), "Completed: gl.run.eems")
})
