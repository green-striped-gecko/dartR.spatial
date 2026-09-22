# Run from the package root with the R 4.4 framework Rscript.
# Review evidence only: no production source is edited.
devtools::load_all(".", quiet = TRUE)
options(lifecycle_verbosity = "quiet")
evidence_dir <- normalizePath("function-review/evidence")
pdf(file.path(evidence_dir, "gl.run.eems-probes.pdf"))
facts <- list()
record <- function(name, value) {
  facts[[name]] <<- value
  cat("\nPROBE", name, ":", paste(value, collapse = " | "), "\n")
}
out <- tempfile("review-out-")
dir.create(out)
args <- list(x = bandicoot.gl[1:12, 1:200],
             eems.path = "/Users/mijangos/programs", out.dir = out,
             plot.dir = out, nDemes = 20, numMCMCIter = 40,
             numBurnIter = 10, numThinIter = 2, seed = 17,
             dpi = 10, cleanup = FALSE, verbose = 0)
invoke <- function(a = args, f = gl.run.eems) {
  tryCatch(do.call(f, a), error = function(e) e)
}
describe <- function(x) if (inherits(x, "error")) conditionMessage(x) else names(x)
before <- serialize(args$x, NULL)
first <- invoke()
record("real_SNP_run", describe(first))
record("input_unchanged", identical(before, serialize(args$x, NULL)))
record("default_plot_file_NULL_saves_RDS", file.exists(file.path(out, "eems.RDS")))
record("raw_output_copied", dir.exists(file.path(out, "data_eems")))
D <- as.matrix(read.table(file.path(tempdir(), "eems.diffs")))
G <- as.matrix(args$x)
for (j in seq_len(ncol(G))) G[is.na(G[, j]), j] <- mean(G[, j], na.rm = TRUE)
independent <- as.matrix(dist(G))^2 / ncol(G)
record("distance_max_absolute_error", max(abs(D - independent)))
record("distance_first_six", signif(D[lower.tri(D)][1:6], 10))
if (!inherits(first, "error")) {
  raw_before <- lapply(list.files(file.path(tempdir(), "data_eems"),
                                 full.names = TRUE), readBin, what = "raw", n = 1e7)
  bad <- args
  bad$numMCMCIter <- 0
  failed <- invoke(bad)
  raw_after <- lapply(list.files(file.path(tempdir(), "data_eems"),
                                full.names = TRUE), readBin, what = "raw", n = 1e7)
  record("failed_process_still_returns", describe(failed))
  record("failed_process_reuses_unchanged_output", identical(raw_before, raw_after))
}

# Local copies of the wrapper intercept only process execution and RDS writes.
# The original loaded function and source file remain unchanged.
make_probe <- function(...) {
  f <- gl.run.eems
  environment(f) <- list2env(list(...), parent = environment(f))
  f
}
calls <- new.env()
calls$plot <- list()
calls$commands <- character()
fake_plots <- function(...) {
  calls$plot <- list(...)
  setNames(rep(list(ggplot2::ggplot()), 8),
           c("mrates01", "mrates02", "qrates01", "qrates02",
             "rdist01", "rdist02", "rdist03", "pilogl01"))
}
testthat::with_mocked_bindings({
  f <- make_probe(system = function(command, ...) {
    calls$commands <- c(calls$commands, command)
    42L
  }, print = function(...) invisible(NULL),
  utils.plot.save = function(...) invisible(NULL))
  a <- args
  a$prob_level <- 0.75
  a$add_abline <- TRUE
  result <- invoke(a, f)
  record("dots_forwarded", intersect(c("prob_level", "add_abline"), names(calls$plot)))
  record("process_failure_reaches_plotter", !inherits(result, "error"))

  a <- args
  a$plot.file <- "review output"
  invoke(a, f)
  record("space_in_filename_command", tail(calls$commands, 1))
  prior <- getwd()
  setwd(tempdir())
  status <- system(tail(calls$commands, 1), ignore.stdout = TRUE, ignore.stderr = TRUE)
  setwd(prior)
  record("space_in_filename_exit_status", status)

  a <- args
  a$x <- testset.gs[1:8, 1:60]
  invoke(a, f)
  record("SilicoDArT_ploidy", unique(ploidy(a$x)))
  record("SilicoDArT_ini_diploid", grep("diploid", readLines(file.path(tempdir(),
         "param_eems.ini")), value = TRUE))

  # The destination exists relative to the caller, but not relative to tempdir().
  caller_dir <- tempfile("caller-")
  dir.create(caller_dir)
  dir.create(file.path(caller_dir, "results"))
  old <- getwd()
  setwd(caller_dir)
  a <- args
  a$out.dir <- "results"
  invoke(a, f)
  setwd(old)
  record("relative_outdir_caller_files", length(list.files(file.path(caller_dir, "results"))))
  record("relative_outdir_written_under_tempdir", file.exists(file.path(tempdir(), "results")))

  # Only disposable sentinels inside this R process's temporary directory.
  sentinel <- file.path(tempdir(), "unrelated_eems_notes.txt")
  writeLines("review-only sentinel", sentinel)
  a <- args
  a$cleanup <- TRUE
  a$out.dir <- tempdir()
  invoke(a, f)
  record("cleanup_deletes_unrelated_matching_file", !file.exists(sentinel))
  record("cleanup_deletes_default_raw_results", !dir.exists(file.path(tempdir(), "data_eems")))
}, make_eems_plots = fake_plots, .package = "reemsplots2")

f_missing <- make_probe(requireNamespace = function(package, ...)
  if (package == "sf") FALSE else base::requireNamespace(package, ...))
record("missing_sf_result", invoke(args, f_missing))
no_coords <- args
no_coords$x@other$latlon <- NULL
record("missing_coordinates_error", describe(invoke(no_coords)))
same <- args
same$x@other$latlon[] <- list(lon = rep(140, 12), lat = rep(-30, 12))
record("identical_coordinates_error", describe(invoke(same)))
dev.off()
jsonlite::write_json(facts, file.path(evidence_dir, "gl.run.eems-probes.json"),
                     pretty = TRUE, auto_unbox = TRUE, digits = 12)
