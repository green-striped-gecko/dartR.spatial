devtools::load_all(".", quiet = TRUE)
evidence <- normalizePath("function-review/evidence")
pdf(file.path(evidence, "gl.ibd-probes.pdf"))
facts <- list()
results <- new.env()
probe <- function(id, args) {
  warnings <- messages <- character()
  set.seed(2026)
  output <- capture.output(value <- withCallingHandlers(
    tryCatch(do.call("gl.ibd", modifyList(list(permutations = 19,
      plot.out = FALSE, verbose = 0), args)), error = identity),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")},
    message = function(m) {messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage")}))
  results[[id]] <- value
  facts[[id]] <<- if (inherits(value, "error")) {
    list(error = conditionMessage(value), output = output,
         warnings = warnings, messages = messages)
  } else {
    list(class = class(value), names = names(value),
         statistic = if(is.list(value)) unname(value$mantel$statistic) else NULL,
         p = if(is.list(value)) value$mantel$signif else NULL,
         output = output, warnings = warnings, messages = messages)
  }
  invisible(value)
}
record <- function(id, value) facts[[id]] <<- value
points <- matrix(c(0, 1, 4, 6, 10, 17), ncol = 1,
                  dimnames = list(LETTERS[1:6], "position"))
D <- dist(points)
M <- as.matrix(D)
shuffle <- c(3, 6, 1, 5, 2, 4)
probe("ordered_matrices", list(Dgen = D, Dgeo = D))
probe("reordered_geographic_matrix", list(Dgen = D,
  Dgeo = as.dist(M[shuffle, shuffle])))
record("aligned_reference_r", cor(as.numeric(D), as.numeric(D)))
wrong_labels <- M
dimnames(wrong_labels) <- list(c(LETTERS[1:5], "Z"), c(LETTERS[1:5], "Z"))
probe("different_sample_sets", list(Dgen = D, Dgeo = wrong_labels))
probe("matrix_transform", list(Dgen = D, Dgeo = D,
  Dgen_trans = "as.matrix(Dgen)"))
probe("no_inputs", list())
probe("only_one_distance", list(Dgen = D))
probe("two_samples", list(Dgen = dist(matrix(1:2)), Dgeo = dist(matrix(1:2))))
probe("constant_distance", list(Dgen = as.dist(matrix(1, 6, 6) - diag(6)), Dgeo = D))
inf <- D; inf[1] <- Inf
probe("infinite_genetic_distance", list(Dgen = inf, Dgeo = D))
na <- D; na[1] <- NA
probe("missing_genetic_distance", list(Dgen = na, Dgeo = D))
probe("missing_geographic_distance", list(Dgen = D, Dgeo = na))

x <- bandicoot.gl[1:6, 1:100]
indNames(x) <- LETTERS[1:6]
x@other$xy <- data.frame(x = c(0, 1, 4, 6, 10, 17), y = c(0, 1, 0, 4, 6, 2),
                         row.names = indNames(x))
probe("provided_distances_and_x", list(x = x, Dgen = D, Dgeo = D))
no_coords <- x
no_coords@other$latlon <- NULL
probe("provided_distances_and_x_without_coordinates",
      list(x = no_coords, Dgen = D, Dgeo = D))
one_pop <- x
pop(one_pop) <- factor(rep("one population", nInd(one_pop)))
probe("provided_matrices_with_single_population_x", list(x=one_pop, Dgen=D, Dgeo=D))
probe("partial_matrix_input", list(x = x, distance = "euclidean",
  coordinates = "xy", Dgen = M))
probe("individual_reference", list(x = x, distance = "euclidean", coordinates = "xy"))
probe("reordered_coordinate_table", list(x = x, distance = "euclidean",
  coordinates = x@other$xy[shuffle, , drop = FALSE]))
record("reordered_coordinate_max_distance_change", max(abs(as.numeric(
  results$individual_reference$Dgeo) - as.numeric(results$reordered_coordinate_table$Dgeo))))
missing_xy <- x@other$xy
missing_xy$x[3] <- NA
probe("missing_coordinate", list(x = x, distance = "euclidean", coordinates = missing_xy))
record("missing_coordinate_distance_A_C", as.numeric(results$missing_coordinate$Dgeo)[2])
record("original_coordinate_distance_A_C", as.numeric(results$individual_reference$Dgeo)[2])
probe("unknown_distance", list(x = x, distance = "not-a-method"))

# Independent Euclidean calculation, explicitly using stats::dist's NA rule.
y <- testset.gl[1:8, 1:60]
G <- as.matrix(y)
hand <- matrix(0, nrow(G), nrow(G))
for(i in seq_len(nrow(G))) for(j in seq_len(nrow(G))) {
  ok <- !is.na(G[i,]) & !is.na(G[j,])
  hand[i,j] <- sqrt(sum((G[i,ok] - G[j,ok])^2) * ncol(G) / sum(ok))
}
probe("euclidean_SNP", list(x = y, distance = "euclidean"))
record("independent_Euclidean_max_error", max(abs(as.numeric(as.dist(hand)) -
  as.numeric(results$euclidean_SNP$Dgen))))
reference_dartr <- gl.dist.ind(y, method = "euclidean", plot.display = FALSE, verbose = 0)
record("documented_gl_dist_ind_max_difference", max(abs(as.numeric(reference_dartr) -
  as.numeric(results$euclidean_SNP$Dgen))))
record("Euclidean_pair_1_2", c(wrapper = as.numeric(results$euclidean_SNP$Dgen)[1],
  gl_dist_ind = as.numeric(reference_dartr)[1]))

# Independently compute the Mantel statistic and one-sided permutation p.
set.seed(11)
perms <- permute::shuffleSet(6, nset=19)
Y <- dist(cbind(c(4,1,8,2,5,9),c(1,4,0,8,2,3)))
attr(Y,"Labels") <- LETTERS[1:6]
probe("fixed_permutation_reference", list(Dgen=D,Dgeo=Y,permutations=perms))
r <- cor(as.numeric(D),as.numeric(Y))
rperm <- apply(perms,1,function(ordering)
  cor(as.numeric(as.dist(M[ordering,ordering])),as.numeric(Y)))
pvalue <- (sum(rperm >= r - sqrt(.Machine$double.eps)) + 1)/(nrow(perms)+1)
record("independent_Mantel", c(r=r,p=pvalue,
  statistic_error=abs(r-results$fixed_permutation_reference$mantel$statistic),
  p_error=abs(pvalue-results$fixed_permutation_reference$mantel$signif)))

# All five routes on a small reference dataset, including NA handling.
for (method in c("Fst", "D", "propShared", "euclidean", "kosman")) {
  z <- bandicoot.gl[, 1:100]
  pops <- split(seq_len(nInd(z)), pop(z))
  selected <- unlist(lapply(pops[seq_len(min(4,length(pops)))], head, 4), use.names = FALSE)
  z <- z[selected, ]
  pop(z) <- droplevels(pop(z))
  probe(paste0("method_", method), list(x = z, distance = method))
}
z <- bandicoot.gl[,1:100]
pops <- split(seq_len(nInd(z)),pop(z))
z <- z[unlist(lapply(pops[1:4],head,4),use.names=FALSE),]
pop(z) <- droplevels(pop(z))
probe("default_transformations", list(x=z))
record("default_Fst_matches_untransformed_StAMPP", isTRUE(all.equal(
  as.matrix(results$default_transformations$Dgen),
  as.matrix(as.dist(StAMPP::stamppFst(as(z,"genlight"),nboots=1))))))

# Computation should not depend on constructing an unused plot.
f <- gl.ibd
environment(f) <- list2env(list(ggplot = function(...) stop("injected plot-construction failure")),
                           parent = environment(f))
record("plot_false_still_constructs_plot", tryCatch({
  f(Dgen=D,Dgeo=D,permutations=19,plot.out=FALSE,verbose=0); "returned"
}, error=conditionMessage))
f <- gl.ibd
environment(f) <- list2env(list(requireNamespace=function(package,...)
  if(package=="dismo") FALSE else base::requireNamespace(package,...)),
  parent=environment(f))
record("missing_dismo_result", f(Dgen=D,Dgeo=D,plot.out=FALSE,verbose=0))
probe("verbose_1", list(Dgen=D,Dgeo=D,verbose=1))
probe("verbose_0_kosman", list(x=x,distance="kosman"))
record("function_object_call_verbose_1", tryCatch(
  do.call(gl.ibd,list(Dgen=D,Dgeo=D,permutations=19,plot.out=FALSE,verbose=1)),
  error=conditionMessage))

# Small disk-backed fixture; no production data is converted.
if (requireNamespace("bigsnpr",quietly=TRUE)) {
  backed <- gl.gen2fbm(x, backingfile=tempfile("ibd-fbm-"), verbose=0)
  for (method in c("euclidean","Fst")) {
    probe(paste0("ordinary_",method), list(x=x,distance=method))
    probe(paste0("FBM_",method), list(x=backed,distance=method))
  }
  record("FBM_vs_ordinary", vapply(c("euclidean","Fst"), function(method)
    isTRUE(all.equal(as.numeric(results[[paste0("FBM_",method)]]$Dgen),
                     as.numeric(results[[paste0("ordinary_",method)]]$Dgen))), logical(1)))
  record("FBM_input_preserved", isTRUE(all.equal(as.matrix(backed),as.matrix(x))))
}
jsonlite::write_json(facts,file.path(evidence,"gl.ibd-probes.json"),
                    pretty=TRUE,auto_unbox=TRUE,digits=12,na="string")
saveRDS(as.list(results),file.path(evidence,"gl.ibd-probe-results.rds"))
dev.off()
cat("IBD probes completed.\n")
