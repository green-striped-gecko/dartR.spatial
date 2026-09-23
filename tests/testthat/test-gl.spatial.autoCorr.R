# Characterisation captured before critical source review at c685f00.
# Snapshots record current behaviour, defects included.
# Approved snapshot diffs: distances used as distances and grm converted
# (change 1); squared Euclidean (change 2); automatic breaks from the minimum
# distance (change 3); geodesic distances (change 4); matrix and list inputs
# run (change 5); p.one.tail counts the observed value (change 6).
summarise_sac <- function(result) {
  if (inherits(result, "error")) return(list(error = conditionMessage(result)))
  rnd <- function(d) {
    if (!is.data.frame(d)) return(class(d))
    d[] <- lapply(d, function(v) if (is.numeric(v)) signif(v, 6) else v)
    d
  }
  if (is.data.frame(result)) return(list(class = class(result), table = rnd(result)))
  list(class = class(result), names = names(result), tables = lapply(result, rnd))
}

run_case <- function(label, expr) {
  set.seed(2026)
  result <- tryCatch(expr, error = identity)
  expect_snapshot(print(summarise_sac(result)), variant = paste0("sac-", label))
}

test_that("gl.spatial.autoCorr reference-data behaviour is unchanged", {
  local_edition(3)
  withr::local_dir(withr::local_tempdir())
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  platy <- dartR.data::platypus.gl[, 1:300]
  one <- platy[pop(platy) == "TENTERFIELD", ]
  before <- serialize(platy, NULL)
  for (m in c("Euclidean", "propShared", "Simple", "grm")) {
    run_case(paste0("platy-", m),
             gl.spatial.autoCorr(one, Dgen_method = m, bins = 5, reps = 19,
                                 plot.out = FALSE, verbose = 0))
  }
  expect_identical(serialize(platy, NULL), before)
  run_case("platy-pops-together",
           gl.spatial.autoCorr(platy, bins = seq(0, 10000, 2000), reps = 19,
                               plot.pops.together = TRUE, plot.out = TRUE,
                               verbose = 0))
  run_case("platy-no-resampling",
           gl.spatial.autoCorr(one, bins = 4, permutation = FALSE,
                               bootstrap = FALSE, plot.out = FALSE,
                               verbose = 0))
  gs <- dartR.data::testset.gs[, 1:200]
  gs <- gs[pop(gs) == "EmmacMaclGeor", ]
  run_case("gs-Jaccard",
           gl.spatial.autoCorr(gs, Dgen_method = "Jaccard", bins = 3,
                               reps = 19, plot.out = FALSE, verbose = 0))
  # matrix and list inputs
  set.seed(1)
  xy <- matrix(runif(40, 0, 100), 20)
  g <- matrix(rbinom(20 * 50, 2, 0.4), 20)
  Dgeo <- as.matrix(dist(xy))
  Dgen <- as.matrix(dist(g))
  run_case("matrix",
           gl.spatial.autoCorr(Dgeo = Dgeo, Dgen = Dgen, bins = 4, reps = 19,
                               plot.out = FALSE, verbose = 0))
  run_case("dist-list",
           gl.spatial.autoCorr(Dgeo = list(a = as.dist(Dgeo), b = as.dist(Dgeo)),
                               Dgen = list(a = as.dist(Dgen), b = as.dist(Dgen / 2)),
                               bins = 4, reps = 19, plot.out = FALSE,
                               verbose = 0))
  run_case("matrix-xnull",
           gl.spatial.autoCorr(x = NULL, Dgeo = Dgeo, Dgen = Dgen, bins = 4,
                               reps = 19, plot.out = FALSE, verbose = 0))
  run_case("dist-list-xnull",
           gl.spatial.autoCorr(x = NULL,
                               Dgeo = list(a = as.dist(Dgeo), b = as.dist(Dgeo)),
                               Dgen = list(a = as.dist(Dgen), b = as.dist(Dgen / 2)),
                               bins = 4, reps = 19, plot.out = FALSE,
                               verbose = 0))
  run_case("utils-plain", utils.spautocor(Dgen, Dgeo, bins = 4, reps = 19))
  run_case("utils-resampling",
           utils.spautocor(Dgen, Dgeo, permutation = TRUE, bootstrap = TRUE,
                           bins = 4, reps = 19))
})
