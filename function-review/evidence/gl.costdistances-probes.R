devtools::load_all(".", quiet = TRUE)
evidence <- "function-review/evidence"
facts <- list()
results <- list()
probe <- function(id, landscape, locs, method = "leastcost", NN = 4, verbose = 0,
                  fun = gl.costdistances) {
  warnings <- messages <- character()
  output <- capture.output(value <- withCallingHandlers(
    tryCatch(fun(landscape, locs, method, NN, verbose = verbose), error = identity),
    warning = function(w) {warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")},
    message = function(m) {messages <<- c(messages, conditionMessage(m)); invokeRestart("muffleMessage")}))
  results[[id]] <<- value
  facts[[id]] <<- list(class = class(value), output = output,
                       warnings = warnings, messages = messages)
  if (inherits(value, "error")) facts[[id]]$error <<- conditionMessage(value)
  else {
    facts[[id]]$dimensions <<- dim(value)
    facts[[id]]$labels <<- dimnames(value)
    facts[[id]]$values <<- as.numeric(value)
  }
  invisible(value)
}
make <- function(values = rep(1, 9), nrows = 3, ncols = 3, width = 3,
                 crs = "+proj=utm +zone=55 +datum=WGS84 +units=m") {
  r <- raster::raster(nrows = nrows, ncols = ncols, xmn = 0, xmx = width,
                       ymn = 0, ymx = nrows * width / ncols, crs = crs)
  raster::values(r) <- values
  r
}
r <- make()
xy <- raster::xyFromCell(r, c(1,5,8)); rownames(xy) <- c("A","B","C")
for(method in c("leastcost","rSPDistance","commute")) probe(paste0("uniform_",method),r,xy,method)
probe("commute_last_cell",r,raster::xyFromCell(r,c(1,5,9)),method="commute")
# Independent all-pairs shortest paths and graph Laplacian for uniform unit cells.
coords <- raster::xyFromCell(r,1:9)
L <- matrix(Inf,9,9); diag(L)<-0
for(i in 1:9) for(j in 1:9) if(sum(abs(coords[i,]-coords[j,]))==1) L[i,j]<-1
W <- 1/L; W[!is.finite(W)] <- 0
shortest <- L
for(k in 1:9) for(i in 1:9) for(j in 1:9) shortest[i,j] <- min(shortest[i,j],shortest[i,k]+shortest[k,j])
E <- eigen(diag(rowSums(W))-W,symmetric=TRUE)
v <- which(E$values > 1e-10)
Q <- E$vectors[,v,drop=FALSE] %*% diag(1/E$values[v]) %*% t(E$vectors[,v,drop=FALSE])
resistance <- outer(diag(Q),diag(Q),"+") - 2*Q
facts$independent_uniform <- list(
  leastcost_max_error = max(abs(results$uniform_leastcost-shortest[c(1,5,8),c(1,5,8)])),
  commute_max_error = max(abs(results$uniform_commute-(sum(W)*resistance)[c(1,5,8),c(1,5,8)])),
  effective_resistance_A_C = resistance[1,8], commute_A_C = results$uniform_commute[1,3],
  graph_volume = sum(W))
# Symmetric landscape: equal resistance on either side of the central cell.
line <- make(c(1,9,1),1,3)
lxy <- raster::xyFromCell(line,1:3); rownames(lxy)<-LETTERS[1:3]
probe("symmetric_landscape",line,lxy)
facts$symmetric_reference <- as.numeric(matrix(c(0,5,10,5,0,5,10,5,0),3,3))
tr <- gdistance::transition(line,function(z) 1/mean(z),4)
correct <- gdistance::geoCorrection(tr,type="c")
facts$symmetric_gdistance_reference <- as.numeric(gdistance::costDistance(correct,lxy,lxy))
# Geographic unit cells must use geographic inter-cell distances.
geo <- raster::raster(nrows=3,ncols=3,xmn=0,xmx=3,ymn=-1.5,ymx=1.5,
                       crs="+proj=longlat +datum=WGS84")
raster::values(geo)<-1
gxy <- raster::xyFromCell(geo,c(4,5)); rownames(gxy)<-c("A","B")
probe("geographic_equator",geo,gxy)
facts$geographic_reference_metres <- raster::pointDistance(gxy[1,],gxy[2,],lonlat=TRUE)
facts$geographic_preserved_crs <- as.numeric(gdistance::costDistance(
  gdistance::geoCorrection(gdistance::transition(geo,function(z) 1/mean(z),4),type="c"),gxy,gxy))
# Genlight uses latitude first in packaged reference data.
x <- testset.gl[1:6,1:20]
pop(x) <- factor(rep(LETTERS[1:3],each=2))
coords <- x@other$latlon[,c("lon","lat")]
g <- raster::raster(nrows=8,ncols=8,xmn=min(coords$lon)-1,xmx=max(coords$lon)+1,
                     ymn=min(coords$lat)-1,ymx=max(coords$lat)+1,crs="+proj=longlat +datum=WGS84")
raster::values(g)<-1
before<-serialize(x,NULL)
probe("genlight_lat_lon",g,x)
y <- x; y@other$latlon <- coords
probe("genlight_lon_lat",g,y)
facts$input_unchanged <- identical(serialize(x,NULL),before)
# Geographic genlight coordinates on a projected raster are not transformed.
projected <- raster::projectRaster(g,crs="+proj=utm +zone=55 +datum=WGS84 +units=m",method="ngb")
probe("genlight_projected_landscape",projected,y)
# Missing/invalid inputs and barriers.
probe("unknown_method",r,xy,method="other")
probe("invalid_NN",r,xy,NN=5)
probe("NA_NN",r,xy,NN=NA)
probe("missing_method",r,xy,method=NA)
probe("one_point",r,xy[1,,drop=FALSE])
probe("no_points",r,xy[FALSE,,drop=FALSE])
bad<-xy; bad[2,1]<-NA; probe("missing_coordinate",r,bad)
bad<-xy; bad[2,]<-c(99,99); probe("outside_raster",r,bad)
bad<-xy; rownames(bad)<-c("A","A","C");probe("duplicate_labels",r,bad)
probe("three_columns",r,cbind(xy,z=1))
probe("non_numeric",r,matrix(letters[1:6],ncol=2))
for(val in c(0,-1,Inf,NA)) {
  bad<-r; raster::values(bad)<-val
  probe(paste0("resistance_",val),bad,xy)
}
barrier<-r; raster::values(barrier)[c(2,5,8)]<-NA
for(method in c("leastcost","rSPDistance","commute"))
  probe(paste0("barrier_",method),barrier,raster::xyFromCell(barrier,c(1,9)),method)
no_crs <- r; raster::crs(no_crs)<-NA
probe("missing_crs",no_crs,xy)
probe("multi_layer_raster",raster::stack(r,r),xy)
# Fixed theta=1 underflows as ordinary map distances grow.
large <- make(width=3000)
bigxy<-raster::xyFromCell(large,c(1,5,8));rownames(bigxy)<-LETTERS[1:3]
probe("rsp_kilometre_cells",large,bigxy,method="rSPDistance")
bigtr<-gdistance::geoCorrection(gdistance::transition(large,function(z) 1/mean(z),4),type="c")
facts$rsp_smaller_theta <- as.numeric(gdistance::rSPDistance(bigtr,bigxy,bigxy,theta=0.001))
# Controlled dependency failure and messaging behaviour.
f<-gl.costdistances
environment(f)<-list2env(list(requireNamespace=function(...) FALSE),parent=environment(f))
probe("missing_dependency",r,xy,fun=f)
np<-y;pop(np)<-NULL
probe("no_population_verbose_0",g,np)
probe("verbose_1",r,xy,verbose=1)
probe("verbose_3",r,xy,verbose=3)
facts$function_object_call <- tryCatch(do.call(gl.costdistances,
  list(landscape=r,locs=xy,method="leastcost",NN=4,verbose=1)),error=conditionMessage)
# Documented example and small file-backed reference.
landscape.sim <- readRDS(system.file("extdata","landscape.sim.rdata",package="dartR.data"))
possxy <- apply(possums.gl@other$xy,2,function(a) tapply(a,pop(possums.gl),mean))
probe("documented_example",landscape.sim,possxy,NN=8)
if(requireNamespace("bigsnpr",quietly=TRUE)) {
  backed <- gl.gen2fbm(y,backingfile=tempfile("cost-fbm-"),verbose=0)
  probe("small_FBM",g,backed)
  facts$FBM_matches <- identical(results$small_FBM,results$genlight_lon_lat)
}
jsonlite::write_json(facts,file.path(evidence,"gl.costdistances-probes.json"),
                    pretty=TRUE,auto_unbox=TRUE,digits=16,na="string")
saveRDS(results,file.path(evidence,"gl.costdistances-probe-results.rds"))
cat("Cost-distance probes completed.\n")
