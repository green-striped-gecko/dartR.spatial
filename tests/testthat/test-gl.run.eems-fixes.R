# Regression checks for approved findings F1-F9. These use a disposable
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
  for (pkg in c("reemsplots2", "sf", "dismo")) {
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
  expect_identical(getwd(), normalizePath(z$root))
  expect_identical(dirname(z$calls$runs[1]), normalizePath("relative output"))
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
