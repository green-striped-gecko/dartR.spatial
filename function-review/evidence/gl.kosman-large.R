suppressMessages(devtools::load_all(quiet = TRUE))
set.seed(1)
m <- matrix(sample(c(0, 1, 2, NA), 300 * 20000, TRUE, prob = c(.45, .3, .2, .05)), 300)
x <- new("genlight", m, ploidy = 2)
gc(reset = TRUE)
t <- system.time(r <- gl.kosman(x, verbose = 0))["elapsed"]
g <- gc()
cat(sprintf("nInd=300 nLoc=20000: %.1f s, max Vcells used %.0f MB\n", t, g["Vcells", grep("max used", colnames(g)) + 1]))
