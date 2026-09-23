devtools::load_all(".",quiet=TRUE)
out <- list()
geo <- raster::raster(nrows=3,ncols=3,xmn=0,xmx=3,ymn=-1.5,ymx=1.5,
                      crs="+proj=longlat +datum=WGS84")
raster::values(geo)<-1
xy<-raster::xyFromCell(geo,c(4,5));rownames(xy)<-c("A","B")
legacy<-geo;comment(legacy@crs)<-NULL
out$modern_is_lonlat <- raster::isLonLat(geo)
out$legacy_is_lonlat <- raster::isLonLat(legacy)
out$modern_cost <- gl.costdistances(geo,xy,"leastcost",4,verbose=0)[1,2]
out$legacy_cost <- gl.costdistances(legacy,xy,"leastcost",4,verbose=0)[1,2]
# Isolate the inherited final-cell defect using gdistance directly.
r<-raster::raster(nrows=3,ncols=3,xmn=0,xmx=3,ymn=0,ymx=3,
                   crs="+proj=utm +zone=55 +datum=WGS84 +units=m")
raster::values(r)<-1
tr<-gdistance::geoCorrection(gdistance::transition(r,function(z)1/mean(z),4),type="c")
out$upstream_last_cell_error<-tryCatch(gdistance::commuteDistance(tr,
  raster::xyFromCell(r,c(1,5,9))),error=conditionMessage)
# Geographic random-walk area correction at high latitude.
high<-raster::raster(nrows=4,ncols=4,xmn=0,xmx=40,ymn=40,ymx=80,
                      crs="+proj=longlat +datum=WGS84")
raster::values(high)<-1
hxy<-raster::xyFromCell(high,c(1,6,11))
htr<-gdistance::transition(high,function(z)1/mean(z),4)
out$high_latitude_wrapper<-as.numeric(gl.costdistances(high,hxy,"commute",4,verbose=0))
out$high_latitude_random_walk_correction<-as.numeric(gdistance::commuteDistance(
  gdistance::geoCorrection(htr,type="r"),hxy))
# Population labels and missing metadata affect aggregation.
x<-bandicoot.gl[1:6,1:20];pop(x)<-factor(c("A","A","B","B","C",NA))
z<-x@other$latlon[,c("lon","lat")];x@other$latlon<-z
land<-raster::raster(nrows=8,ncols=8,xmn=min(z$lon)-1,xmx=max(z$lon)+1,
                      ymn=min(z$lat)-1,ymx=max(z$lat)+1,crs="+proj=longlat +datum=WGS84")
raster::values(land)<-1
out$missing_population_result<-tryCatch(as.numeric(gl.costdistances(land,x,"leastcost",4,verbose=0)),error=conditionMessage)
# Dependency source and provenance.
writeLines(capture.output(get(".rD",envir=asNamespace("gdistance"))),
           file.path("function-review/evidence","gl.costdistances-commute-engine.log"))
jsonlite::write_json(out,"function-review/evidence/gl.costdistances-followup.json",pretty=TRUE,auto_unbox=TRUE,digits=16,na="string")
print(out)
