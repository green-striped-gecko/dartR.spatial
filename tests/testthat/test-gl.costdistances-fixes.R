cost_raster <- function(values = rep(1, 9), nrows = 3, ncols = 3, width = 3,
                        crs = "+proj=utm +zone=55 +datum=WGS84 +units=m") {
  r <- raster::raster(nrows = nrows, ncols = ncols, xmn = 0, xmx = width,
                     ymn = 0, ymx = nrows * width / ncols, crs = crs)
  raster::setValues(r, values)
}
cost_run <- function(r, xy, method = "leastcost", NN = 4, ...) {
  gl.costdistances(r, xy, method, NN, verbose = 0, ...)
}
cost_reference <- function(r, NN = 4) {
  # Independent graph construction, shortest paths and pseudoinverse.
  xy <- raster::xyFromCell(r, seq_len(raster::ncell(r)))
  v <- raster::getValues(r)
  n <- length(v)
  edge <- matrix(Inf, n, n)
  diag(edge) <- 0
  for (i in seq_len(n)) for (j in seq_len(n)) {
    delta <- abs(xy[i, ] - xy[j, ]) / raster::res(r)
    adjacent <- if (NN == 4) sum(delta) == 1 else max(delta) == 1
    if (i != j && adjacent) {
      edge[i, j] <- mean(v[c(i, j)]) * sqrt(sum((xy[i, ] - xy[j, ])^2))
    }
  }
  weights <- 1 / edge
  weights[!is.finite(weights)] <- 0
  shortest <- edge
  for (k in seq_len(n)) for (i in seq_len(n)) for (j in seq_len(n)) {
    shortest[i, j] <- min(shortest[i, j], shortest[i, k] + shortest[k, j])
  }
  eigen <- eigen(diag(rowSums(weights)) - weights, symmetric = TRUE)
  positive <- eigen$values > 1e-10
  vectors <- eigen$vectors[, positive, drop = FALSE]
  inverse <- vectors %*% diag(1 / eigen$values[positive]) %*% t(vectors)
  effective <- outer(diag(inverse), diag(inverse), "+") - 2 * inverse
  list(leastcost = shortest, commute = sum(weights) * effective)
}

test_that("mean resistance and commute match independent graph calculations (F1 F6)", {
  skip_if_not_installed("gdistance")
  for (values in list(rep(1, 9), c(1, 9, 1, 3, 5, 7, 2, 4, 6))) {
    r <- cost_raster(values)
    xy <- raster::xyFromCell(r, 1:9)
    for (NN in c(4, 8)) {
      reference <- cost_reference(r, NN)
      for (method in c("leastcost", "commute")) {
        result <- cost_run(r, xy, method, NN)
        expect_equal(unname(result), reference[[method]], tolerance = 1e-10)
        expect_equal(result, t(result), tolerance = 1e-12)
        expect_equal(diag(result), rep(0, 9), tolerance = 1e-12)
      }
    }
  }
  r <- cost_raster(c(1, 9, 1), 1, 3)
  xy <- raster::xyFromCell(r, 1:3)
  expect_equal(cost_run(r, xy), matrix(c(0,5,10,5,0,5,10,5,0), 3, 3))
  for (method in c("leastcost", "commute")) {
    reversed <- cost_run(r, xy[3:1, ], method)
    expect_equal(reversed, cost_run(r, xy, method)[3:1, 3:1])
  }
})

test_that("grounded, repeated and single-cell locations retain order (F6)", {
  skip_if_not_installed("gdistance")
  r <- cost_raster()
  xy <- raster::xyFromCell(r, c(9, 1, 5, 9))
  rownames(xy) <- c("last", "first", "middle", "last-again")
  result <- cost_run(r, xy, "commute")
  expected <- cost_reference(r)$commute[c(9,1,5,9), c(9,1,5,9)]
  expect_equal(unname(result), expected, tolerance = 1e-10)
  expect_identical(rownames(result), rownames(xy))
  expect_identical(colnames(result), rownames(xy))
  for (method in c("leastcost", "rSPDistance", "commute")) {
    expect_equal(unname(cost_run(r, xy[1,,drop=FALSE], method)), matrix(0,1,1))
    expect_equal(unname(cost_run(r, xy[c(1,4),], method)), matrix(0,2,2))
  }
  r <- cost_raster(1, 1, 1, 1)
  expect_equal(cost_run(r, matrix(c(.5,.5),1), "commute"), matrix(0,1,1))
  scaled <- raster::setValues(cost_raster(), rep(17,9))
  expect_equal(cost_run(scaled, xy, "commute"), result, tolerance = 1e-10)
})

test_that("genlight centres are named lon-lat and projected without mutation (F2)", {
  skip_if_not_installed("gdistance")
  skip_if_not_installed("terra")
  x <- testset.gl[1:6, 1:20]
  pop(x) <- factor(rep(LETTERS[1:3], each = 2))
  coords <- x@other$latlon[, c("lon", "lat")]
  r <- raster::raster(nrows=8,ncols=8,xmn=min(coords$lon)-1,
    xmx=max(coords$lon)+1,ymn=min(coords$lat)-1,ymx=max(coords$lat)+1,
    crs="+proj=longlat +datum=WGS84")
  raster::values(r) <- 1
  before <- serialize(x,NULL)
  raster.before <- serialize(r,NULL)
  centres <- apply(coords,2,function(a) tapply(a,pop(x),mean))
  result <- cost_run(r,x)
  expect_equal(result,cost_run(r,centres))
  y <- x; y@other$latlon <- coords
  expect_equal(result,cost_run(r,y))
  expect_identical(serialize(x,NULL),before)
  expect_identical(serialize(r,NULL),raster.before)
  projected <- raster::projectRaster(r,
    crs="+proj=utm +zone=55 +datum=WGS84 +units=m",method="ngb")
  projected.centres <- terra::project(centres,from="EPSG:4326",
                                      to=raster::projection(projected))
  rownames(projected.centres) <- rownames(centres)
  expect_equal(cost_run(projected,x),cost_run(projected,projected.centres))
  bad <- x; bad@other$latlon <- coords; bad@other$latlon[2,1] <- NA
  expect_error(cost_run(r,bad),"non-finite coordinates")
  bad <- x; pop(bad) <- factor(c("A","A","B","B","C",NA))
  expect_error(cost_run(r,bad),"population assignment")
  raster::crs(r) <- NA
  expect_error(cost_run(r,x),"known landscape CRS")
})

test_that("invalid observations and resistance fail before omission (F3)", {
  skip_if_not_installed("gdistance")
  r <- cost_raster()
  xy <- raster::xyFromCell(r,c(1,5,9)); rownames(xy) <- LETTERS[1:3]
  for (value in c(NA,NaN,Inf,99)) {
    bad <- xy; bad[2,1] <- value
    expect_error(cost_run(r,bad),"non-finite coordinates|outside the raster")
  }
  bad <- xy; rownames(bad) <- c("A","A","C")
  expect_error(cost_run(r,bad),"unique")
  expect_error(cost_run(r,xy[FALSE,,drop=FALSE]),"at least one")
  expect_error(cost_run(r,cbind(xy,1)),"numeric x/y pair")
  expect_error(cost_run(r,matrix(letters[1:6],ncol=2)),"numeric x/y pair")
  expect_error(cost_run(r,xy,method="other"),"method must")
  expect_error(cost_run(r,xy,method=NA),"method must")
  for (NN in list(NA,5,c(4,8))) expect_error(cost_run(r,xy,NN=NN),"NN must")
  expect_error(cost_run(raster::stack(r,r),xy),"single RasterLayer")
  for (value in c(0,-1,-Inf)) {
    bad <- raster::setValues(r,rep(value,9))
    expect_error(cost_run(bad,xy),"strictly positive")
  }
  expect_error(cost_run(raster::setValues(r,rep(NA,9)),xy),"no finite")
  for (value in c(NA,Inf)) {
    bad <- r; raster::values(bad)[5] <- value
    expect_error(cost_run(bad,xy),"on barriers")
  }
  for (NN in list(8,16,"bishop")) expect_type(cost_run(r,xy,NN=NN),"double")
  raster::crs(r) <- NA
  expect_equal(cost_run(r,xy),cost_run(cost_raster(),xy))
})

test_that("barriers retain least-cost Inf and diagnose disconnected walks (F3)", {
  skip_if_not_installed("gdistance")
  r <- cost_raster(); raster::values(r)[c(2,5,8)] <- NA
  xy <- raster::xyFromCell(r,c(1,9))
  expect_identical(cost_run(r,xy)[1,2],Inf)
  expect_error(cost_run(r,xy,"commute"),"connected traversable")
  expect_error(cost_run(r,xy,"rSPDistance"),"connected traversable")
  # A connected graph with a barrier still supports all three methods.
  r <- cost_raster(); raster::values(r)[5] <- NA
  for (method in c("leastcost","rSPDistance","commute")) {
    expect_true(all(is.finite(cost_run(r,xy,method))))
  }
})

test_that("geographic correction follows the method (F4)", {
  skip_if_not_installed("gdistance")
  r <- raster::raster(nrows=4,ncols=4,xmn=0,xmx=40,ymn=40,ymx=80,
                       crs="+proj=longlat +datum=WGS84")
  raster::values(r) <- 1
  xy <- raster::xyFromCell(r,c(1,6,11))
  transition <- gdistance::transition(r,function(a)1/mean(a),4)
  expected <- as.matrix(gdistance::commuteDistance(
    gdistance::geoCorrection(transition,type="r"),xy))
  expect_equal(unname(cost_run(r,xy,"commute")),unname(expected),tolerance=1e-10)
  expected <- gdistance::costDistance(
    gdistance::geoCorrection(transition,type="c"),xy,xy)
  expect_equal(cost_run(r,xy),expected)
})

test_that("theta is optional and numerical RSP failure is explicit (F5)", {
  skip_if_not_installed("gdistance")
  r <- cost_raster(width=3000)
  xy <- raster::xyFromCell(r,c(1,5,9))
  expect_error(cost_run(r,xy,"rSPDistance"),"RSP produced non-finite")
  result <- cost_run(r,xy,"rSPDistance",theta=.001)
  expect_true(all(is.finite(result)))
  tr <- gdistance::geoCorrection(gdistance::transition(r,function(a)1/mean(a),4),type="c")
  expect_equal(result,gdistance::rSPDistance(tr,xy,xy,theta=.001))
  for(theta in list(0,20,-1,NA,Inf,c(1,2),"1")) {
    expect_error(cost_run(r,xy,"rSPDistance",theta=theta),"theta must")
  }
  expect_named(formals(gl.costdistances),c("landscape","locs","method","NN","verbose","theta"))
  small <- cost_raster()
  expect_equal(cost_run(small,raster::xyFromCell(small,c(1,5,9)),"rSPDistance"),
    cost_run(small,raster::xyFromCell(small,c(1,5,9)),"rSPDistance",theta=1))
})

test_that("dependency errors, verbosity and closure calls behave consistently (F8 F9 F10)", {
  skip_if_not_installed("gdistance")
  r <- cost_raster(); xy <- raster::xyFromCell(r,c(1,5,9))
  f <- gl.costdistances
  environment(f) <- list2env(list(requireNamespace=function(...) FALSE),parent=environment(f))
  expect_error(f(r,xy,"leastcost",4,verbose=0),"Install package gdistance")
  expect_silent(cost_run(r,xy))
  output <- capture.output(result <- do.call(gl.costdistances,
    list(landscape=r,locs=xy,method="commute",NN=4,verbose=1)))
  expect_true(any(grepl("Starting gl.costdistances",output)))
  expect_true(any(grepl("Completed:",output)))
  expect_false(any(grepl("Returned|Calculating",output)))
  expect_type(result,"double")
  output <- capture.output(gl.costdistances(r,xy,"leastcost",4,verbose=3))
  expect_true(any(grepl("Returned 3 by 3",output)))
  x <- testset.gl[1:3,1:10]; pop(x) <- NULL
  coords <- x@other$latlon
  r <- raster::raster(nrows=5,ncols=5,xmn=min(coords$lon)-1,
    xmx=max(coords$lon)+1,ymn=min(coords$lat)-1,ymx=max(coords$lat)+1,
    crs="+proj=longlat +datum=WGS84"); raster::values(r) <- 1
  expect_silent(cost_run(r,x))
})

test_that("small file-backed inputs agree and retain genotype metadata (F2)", {
  skip_if_not_installed("gdistance")
  skip_if_not_installed("bigsnpr")
  skip_if_not_installed("terra")
  x <- testset.gl[1:6,1:20]
  pop(x) <- factor(rep(LETTERS[1:3],each=2))
  coords <- x@other$latlon
  r <- raster::raster(nrows=8,ncols=8,xmn=min(coords$lon)-1,
    xmx=max(coords$lon)+1,ymn=min(coords$lat)-1,ymx=max(coords$lat)+1,
    crs="+proj=longlat +datum=WGS84"); raster::values(r) <- 1
  directory <- tempfile("cost-fbm-"); dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  backed <- gl.gen2fbm(x,backingfile=file.path(directory,"genotypes"),verbose=0)
  before <- as.matrix(backed)
  metadata <- list(ploidy=ploidy(backed),loc=backed@other$loc.metrics,
                   ind=backed@other$ind.metrics)
  expect_equal(cost_run(r,backed),cost_run(r,x))
  expect_equal(as.matrix(backed),before)
  expect_identical(list(ploidy=ploidy(backed),loc=backed@other$loc.metrics,
                        ind=backed@other$ind.metrics),metadata)
})
