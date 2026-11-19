# Diagnose F11 without changing the existing gl.ibd regression file.
devtools::load_all(".", quiet = TRUE)
requireNamespace("proxy", quietly = TRUE)
candidate <- tempfile("ibd-edition3-", fileext = ".R")
writeLines(c("local_edition(3)", readLines("tests/testthat/test-gl.ibd-fixes.R")), candidate)
result <- testthat::test_file(candidate, reporter = "summary", stop_on_failure = TRUE)
cat("Candidate assertions passed:", sum(as.data.frame(result)$passed), "\n")
unlink(candidate)
