devtools::load_all(".", quiet = TRUE)
evidence <- "function-review/evidence"
pdf(file.path(evidence, "gl.ibd-verification.pdf"))
facts <- list()
run <- function(...) {
  set.seed(2026)
  gl.ibd(..., permutations = 19, plot.out = FALSE, verbose = 0)
}
D <- dist(matrix(c(0, 1, 4, 6, 10, 17), ncol = 1,
                 dimnames = list(LETTERS[1:6], "position")))
M <- as.matrix(D)
shuffle <- c(3, 6, 1, 5, 2, 4)
result <- run(Dgen = D, Dgeo = M[shuffle, shuffle])
facts$reordered_distances <- c(r = result$mantel$statistic, p = result$mantel$signif)
x <- bandicoot.gl[1:6, 1:100]
indNames(x) <- LETTERS[1:6]
x@other$xy <- data.frame(x = c(0, 1, 4, 6, 10, 17), y = c(0, 1, 0, 4, 6, 2),
                         row.names = indNames(x))
base <- run(x, distance = "euclidean", coordinates = "xy")
reordered <- run(x, distance = "euclidean", coordinates = x@other$xy[shuffle, ])
facts$coordinate_alignment <- c(reference_r = base$mantel$statistic,
  reordered_r = reordered$mantel$statistic,
  maximum_distance_difference = max(abs(base$Dgeo - reordered$Dgeo)))
coords <- x@other$xy
coords$x[3] <- NA
facts$missing_coordinate <- tryCatch(run(x, distance = "euclidean", coordinates = coords),
                                     error = conditionMessage)
z <- bandicoot.gl[, 1:100]
groups <- split(seq_len(nInd(z)), pop(z))
z <- z[unlist(lapply(groups[1:4], head, 4), use.names = FALSE), ]
pop(z) <- droplevels(pop(z))
# Original probe results were saved before any production-source edits.
# Hexadecimal literals preserve every binary digit for exact comparisons.
before <- dget(file.path(evidence, "gl.ibd-before-methods.R"))
for (method in c("Fst", "D", "propShared", "euclidean", "kosman")) {
  result <- run(z, distance = method)
  original <- before[[paste0("method_", method)]]
  facts[[paste0("unchanged_", method)]] <- c(
    genetic_max_error = max(abs(as.numeric(result$Dgen) - as.numeric(original$Dgen))),
    geographic_max_error = max(abs(as.numeric(result$Dgeo) - as.numeric(original$Dgeo))),
    statistic_error = abs(result$mantel$statistic - original$statistic),
    p_error = abs(result$mantel$signif - original$signif))
  stopifnot(all(facts[[paste0("unchanged_", method)]] == 0))
}
set.seed(2026)
shown <- gl.ibd(z, permutations = 19, verbose = 3, paircols = "pop")
stopifnot(inherits(shown$mantel, "mantel"))
facts$verbose_3_plot <- "completed"
# Execute the three documented examples with shorter permutation counts.
for (args in list(
    list(x = bandicoot.gl[, 1:100], Dgeo_trans = "log(Dgeo)", Dgen_trans = "Dgen/(1-Dgen)"),
    list(x = bandicoot.gl[1:10, ], distance = "euclidean", paircols = "pop", Dgeo_trans = "Dgeo"),
    list(x = bandicoot.gl[, 1:100], paircols = "pop"))) {
  set.seed(2026)
  result <- do.call(gl.ibd, c(args, list(permutations = 19, verbose = 0)))
  stopifnot(inherits(result$mantel, "mantel"))
}
facts$documented_examples <- "all three completed (19 permutations)"
jsonlite::write_json(facts, file.path(evidence, "gl.ibd-verification.json"),
                     pretty = TRUE, auto_unbox = TRUE, digits = 12)
dev.off()
cat("All five valid method fixtures retain exactly the original distances, Mantel statistics and p-values.\n")
