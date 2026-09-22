devtools::load_all(".", quiet = TRUE)
repo <- getwd()
evidence <- normalizePath("function-review/evidence")
root <- tempfile("eems-integration-")
dir.create(root)
dir.create(file.path(root, "results with spaces"))
dir.create(file.path(root, "binary folder"))
stopifnot(file.symlink("/Users/mijangos/programs/runeems_snps",
                      file.path(root, "binary folder", "runeems_snps")))
setwd(root)
pdf(file.path(evidence, "gl.run.eems-after.pdf"))
sentinel <- file.path("results with spaces", "unrelated_eems_notes.txt")
writeLines("preserve this", sentinel)
x <- bandicoot.gl[1:12, 1:200]
original <- serialize(x, NULL)
args <- list(x = x, eems.path = "binary folder", out.dir = "results with spaces",
             plot.dir = "results with spaces", nDemes = 20, numMCMCIter = 40,
             numBurnIter = 10, numThinIter = 2, seed = 17, dpi = 10,
             verbose = 0, cleanup = TRUE, prob_level = 0.75, add_abline = TRUE)
quiet <- capture.output(plots <- do.call("gl.run.eems", args))
stopifnot(length(quiet) == 0L, length(plots) == 8L,
          identical(original, serialize(x, NULL)),
          readLines(sentinel) == "preserve this",
          !file.exists(file.path("results with spaces", "eems.RDS")),
          identical(getwd(), normalizePath(root)))
runs <- list.dirs("results with spaces", recursive = FALSE, full.names = TRUE)
first <- runs[1]
trace <- readBin(file.path(first, "data_eems", "mcmcpilogl.txt"), "raw", n = 1e7)
stopifnot(file.exists(file.path(first, "eems.log")),
          !file.exists(file.path(first, "params.ini")))
args$numMCMCIter <- 0
bad <- tryCatch(do.call("gl.run.eems", args), error = identity)
stopifnot(inherits(bad, "error"), grepl("EEMS failed", conditionMessage(bad)),
          identical(trace, readBin(file.path(first, "data_eems", "mcmcpilogl.txt"),
                                   "raw", n = 1e7)))
args$numMCMCIter <- 40
args$verbose <- 3
args$plot.file <- "named plot with spaces"
plots3 <- do.call("gl.run.eems", args)
stopifnot(length(plots3) == 8L,
          file.exists(file.path("results with spaces", "named plot with spaces.RDS")),
          identical(names(plots), names(plots3)),
          identical(original, serialize(x, NULL)),
          readLines(sentinel) == "preserve this")
completed <- list.files("results with spaces", pattern = "mcmcpilogl.txt",
                        recursive = TRUE, full.names = TRUE)
stopifnot(length(completed) == 2L,
          identical(readBin(completed[1], "raw", n = 1e7),
                    readBin(completed[2], "raw", n = 1e7)))
# Compare the seeded chain against the original reviewed implementation.
legacy_env <- new.env(parent = environment(gl.run.eems))
legacy_source <- system2("git", c("-C", shQuote(repo), "show",
  "209ef9aaef4e61dbbd9c82d789ff3f94e53d620e:R/gl.run.eems.r"), stdout = TRUE)
eval(parse(text = legacy_source), legacy_env)
legacy_args <- args
legacy_args$verbose <- 0
legacy_args$cleanup <- FALSE
legacy_args$plot.file <- "legacy_comparison"
legacy_args$out.dir <- normalizePath("results with spaces")
legacy_args$plot.dir <- normalizePath("results with spaces")
capture.output(legacy_plots <- do.call(legacy_env$gl.run.eems, legacy_args))
legacy_trace <- file.path(tempdir(), "data_legacy_comparison", "mcmcpilogl.txt")
stopifnot(identical(trace, readBin(legacy_trace, "raw", n = 1e7)))
cat("ORIGINAL BASELINE: seeded posterior trace is byte-identical to 209ef9a.\n")
# Retain a successful and failed executable log with the verification record.
file.copy(file.path(first, "eems.log"), file.path(evidence, "eems-success.log"),
          overwrite = TRUE)
failed_log <- sub(".*See log: ", "", conditionMessage(bad))
file.copy(failed_log, file.path(evidence, "eems-failure.log"), overwrite = TRUE)
dev.off()
cat("INTEGRATION PASS: real executable; relative paths and spaces; silent level 0;\n",
    "level 3 completion; eight returned plots; optional named RDS; owned cleanup;\n",
    "failed run errors; prior trace preserved; unchanged seeded inference and input.\n")
