suppressMessages(devtools::load_all(quiet = TRUE))
grDevices::pdf(NULL)
set.seed(42)
n <- 80; L <- 400
lon <- sort(runif(n, 150, 151))
slope <- rnorm(L, 0, 3); p0 <- runif(L, 0.2, 0.8)
freq <- sapply(seq_len(L), function(l) plogis(qlogis(p0[l]) + slope[l] * (lon - 150.5)))
g <- matrix(rbinom(n * L, 2, freq), n)
mk <- function(m, ploidy) {
  x <- new("dartR", m, ploidy = ploidy)
  indNames(x) <- paste0("i", 1:n); locNames(x) <- paste0("l", 1:L)
  pop(x) <- rep("P", n); x@other$latlon <- data.frame(lat = -30, lon = lon)
  gl.compliance.check(x, verbose = 0)
}
sim <- mk(g, 2)
geo <- as.matrix(dist(dismo::Mercator(sim@other$latlon[, c("lon", "lat")])))
ref <- function(D) round(utils.spautocor(as.matrix(D), geo, bins = 5)$r.uc[1:3] + 1 / (n - 1), 4)
cat("=== Q1 SNP distances passed without reversal (expect positive first class) ===\n")
for (m in c("Simple", "Absolute")) {
  D <- as.matrix(gl.dist.ind(sim, method = m, verbose = 0)); diag(D) <- 0
  cat(sprintf("%-9s raw r = %s | reversed (as function) r = %s\n", m,
              paste(ref(D), collapse = " "), paste(ref({R <- 1 - D; diag(R) <- 0; R}), collapse = " ")))
}
G <- as.matrix(gl.grm2(sim, plotheatmap = FALSE, verbose = 0))
cat("grm as distance (as function): r =", ref({G2 <- G; diag(G2) <- 0; G2}),
    "| grm converted to distance (max - G): r =", ref(max(G) - G), "\n")

cat("\n=== Q2 SilicoDArT (presence/absence) ===\n")
pa <- (g > 0) * 1
simgs <- mk(pa, 1)
for (m in c("Euclidean", "Simple", "Jaccard", "Bray-Curtis")) {
  r <- gl.spatial.autoCorr(simgs, Dgen_method = m, bins = 5, permutation = FALSE,
                           bootstrap = FALSE, plot.out = FALSE, verbose = 0)
  D <- as.matrix(gl.dist.ind(simgs, method = m, verbose = 0)); diag(D) <- 0
  cat(sprintf("%-11s function r = %s | raw distance r = %s\n", m,
              paste(round(r[[1]]$r[1:3], 4), collapse = " "), paste(ref(D), collapse = " ")))
}

cat("\n=== Q3 automatic bins when the minimum distance is far from zero ===\n")
set.seed(3)
xy <- cbind(runif(30, 50000, 60000), runif(30, 0, 1))
Dgeo <- as.matrix(dist(xy)) + 50000; diag(Dgeo) <- 0
Dgen <- as.matrix(dist(matrix(rbinom(30 * 50, 2, 0.4), 30)))
out <- utils.spautocor(Dgen, Dgeo, bins = 5)
print(out[, c("Bin", "N")])
off <- Dgeo[lower.tri(Dgeo)]
cat("distance range:", round(range(off)), " pairs:", length(off), "\n")

cat("\n=== Q4 list inputs (x = NULL) ===\n")
D1 <- as.matrix(dist(xy)); D2 <- Dgen
try1 <- function(lbl, e) cat(lbl, ":", tryCatch({e; "ok"}, error = function(err) conditionMessage(err)), "\n")
try1("unnamed list of dist", gl.spatial.autoCorr(x = NULL, Dgeo = list(as.dist(D1)), Dgen = list(as.dist(D2)), bins = 3, reps = 5, plot.out = FALSE, verbose = 0))
try1("named list of dist", gl.spatial.autoCorr(x = NULL, Dgeo = list(a = as.dist(D1)), Dgen = list(a = as.dist(D2)), bins = 3, reps = 5, plot.out = FALSE, verbose = 0))
try1("unnamed list of matrices", gl.spatial.autoCorr(x = NULL, Dgeo = list(D1), Dgen = list(D2), bins = 3, reps = 5, plot.out = FALSE, verbose = 0))
