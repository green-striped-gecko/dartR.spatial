suppressMessages(devtools::load_all(quiet = TRUE))
p <- function(label, expr) {
  cat("\n=== ", label, " ===\n")
  r <- tryCatch(expr, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (is.character(r)) print(r)
  invisible(r)
}
# Independent Kosman: mean over shared non-missing loci of |a-b|/ploidy
indep <- function(x) {
  m <- as.matrix(x); pl <- unique(ploidy(x)); n <- nrow(m)
  d <- matrix(NA, n, n)
  for (i in 1:n) for (j in 1:n) {
    ok <- !is.na(m[i, ]) & !is.na(m[j, ])
    d[i, j] <- mean(abs(m[i, ok] - m[j, ok]) / pl)
  }
  d
}
for (nm in c("testset.gl", "testset.gs", "possums.gl")) {
  x <- get(nm)[1:15, 1:200]
  r <- gl.kosman(x, verbose = 0)
  ref <- indep(x)
  lo <- lower.tri(ref, diag = TRUE)
  cat(nm, ": max |diff| vs independent =", max(abs(r$kosman[lo] - ref[lo])), "\n")
}
# P1 multiple ploidies
x <- testset.gl[1:6, 1:20]; x2 <- x; ploidy(x2) <- c(2, 2, 2, 4, 4, 4)
p("P1 mixed ploidy", print(gl.kosman(x2, verbose = 0)))
# P2 pair with no shared loci
x3 <- testset.gl[1:4, 1:10]; m <- as.matrix(x3); m[1, 1:5] <- NA; m[2, 6:10] <- NA
x3b <- new("genlight", m, ploidy = 2); indNames(x3b) <- indNames(x3)
p("P2 no shared loci", print(gl.kosman(x3b, verbose = 0)$kosman[1:2, 1:2]))
# P3 verbose output
p("P3 verbose=3", invisible(gl.kosman(testset.gl[1:4, 1:10], verbose = 3)))
# P4 non-genlight
p("P4 matrix input", gl.kosman(matrix(0, 3, 3), verbose = 0)) |> print()
# P5 return type vs @return claim
r <- gl.kosman(testset.gl[1:4, 1:10], verbose = 0); cat("\nP5 class:", class(r), "| names:", names(r), "\n")
# P6 scaling: time and peak memory
for (nl in c(250, 1000)) {  # nInd = 81
  x <- platypus.gl[, 1:nl]; gc(reset = TRUE)
  t <- system.time(gl.kosman(x, verbose = 0))["elapsed"]
  mem <- sum(gc()[, 6])
  cat(sprintf("P6 nInd=81 nLoc=%d: %.2f s, peak %.0f MB\n", nl, t, mem))
}
# P7 vectorised equivalent for comparison
kos_vec <- function(x) {
  m <- as.matrix(x); pl <- unique(ploidy(x)); ok <- !is.na(m); m0 <- m; m0[!ok] <- 0
  nl <- tcrossprod(ok * 1)
  # sum |a-b| over shared loci, via indicator of each dosage value
  s <- 0; vals <- 0:pl
  for (a in vals) for (b in vals) if (a != b) s <- s + abs(a - b) * tcrossprod((m0 == a) * ok, (m0 == b) * ok)
  list(kosman = s / pl / nl, nloci = nl)
}
x <- platypus.gl; gc(reset = TRUE)
t <- system.time(v <- kos_vec(x))["elapsed"]; mem <- sum(gc()[, 6])
r <- gl.kosman(x, verbose = 0); lo <- lower.tri(r$kosman, diag = TRUE)
cat(sprintf("P7 vectorised nLoc=1000: %.2f s, peak %.0f MB, max diff %g, nloci equal %s\n",
            t, mem, max(abs(v$kosman[lo] - r$kosman[lo])), all(v$nloci[lo] == r$nloci[lo])))
