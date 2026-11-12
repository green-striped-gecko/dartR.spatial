devtools::load_all(".", quiet = TRUE)
facts <- list()
line <- raster::raster(nrows=1,ncols=3,xmn=0,xmx=3,ymn=0,ymx=1,
  crs="+proj=utm +zone=55 +datum=WGS84 +units=m")
raster::values(line) <- c(1,9,1)
xy <- raster::xyFromCell(line,1:3)
facts$mean_resistance <- as.numeric(gl.costdistances(line,xy,"leastcost",4,verbose=3))
facts$grounded_commute <- as.numeric(gl.costdistances(line,xy,"commute",4,verbose=3))
stopifnot(isTRUE(all.equal(facts$mean_resistance,c(0,5,10,5,0,5,10,5,0))))
stopifnot(isTRUE(all.equal(facts$grounded_commute,c(0,4,8,4,0,4,8,4,0))))
# Reference genlight end-to-end, including explicit theta suitable for this scale.
x <- testset.gl[1:6,1:20]
pop(x) <- factor(rep(LETTERS[1:3],each=2))
coords <- x@other$latlon
land <- raster::raster(nrows=8,ncols=8,xmn=min(coords$lon)-1,xmx=max(coords$lon)+1,
  ymn=min(coords$lat)-1,ymx=max(coords$lat)+1,crs="+proj=longlat +datum=WGS84")
raster::values(land) <- 1
for(method in c("leastcost","commute","rSPDistance")) {
  result <- gl.costdistances(land,x,method,8,verbose=3,theta=1e-6)
  stopifnot(all(is.finite(result)),identical(dim(result),c(3L,3L)))
  facts[[paste0("reference_",method)]] <- as.numeric(result)
}
# Run the documented example as written, on a raster without a CRS.
landscape.sim <- readRDS(system.file("extdata","landscape.sim.rdata",package="dartR.data"))
xy <- apply(possums.gl@other$xy,2,function(a) tapply(a,pop(possums.gl),mean))
result <- gl.costdistances(landscape.sim,xy,"leastcost",8,verbose=3)
stopifnot(all(is.finite(result)),identical(dim(result),c(10L,10L)))
facts$documented_example <- list(cells=raster::ncell(landscape.sim),dimensions=dim(result),pair_A_B=result[1,2])
# Moderate sparse solve; not a claim of arbitrary large-landscape scalability.
r <- raster::raster(nrows=80,ncols=80,xmn=0,xmx=80,ymn=0,ymx=80,
  crs="+proj=utm +zone=55 +datum=WGS84 +units=m")
raster::values(r) <- 1
xy <- raster::xyFromCell(r,c(1,3200,6400))
elapsed <- system.time(result <- gl.costdistances(r,xy,"commute",8,verbose=3))
stopifnot(all(is.finite(result)),max(abs(result-t(result)))<1e-7,
          all(diag(result)==0))
facts$moderate_commute <- list(cells=6400,locations=3,elapsed_seconds=unname(elapsed["elapsed"]),distances=as.numeric(result))
jsonlite::write_json(facts,"function-review/evidence/gl.costdistances-verification.json",
                    pretty=TRUE,auto_unbox=TRUE,digits=16)
cat("All integration assertions passed.\n")
