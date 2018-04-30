library(rgeos)

#=======================================================================
# Functions to support GIS operations
#=======================================================================
set_do_poly_check(FALSE)
# UTM Zone 17 Meters
UTM17m = CRS("+init=epsg:26917") 
# lat-long projection
llwgs84  = CRS("+init=epsg:4326") 
# NAD83 HARN StatePlane Florida East FIPS 0901 Feet
HARNSP17ft  = CRS("+init=epsg:2881") 

clipToExtent <- function(sp, extent) {
  require(rgeos)
  keep <-
    gContains(extent, sp, byid = TRUE) |
    gOverlaps(extent, sp, byid = TRUE)
  stopifnot(ncol(keep) == 1)
  sp[drop(keep), ]
}
gClip <- function(shp, bb) {
  if (class(bb) == "matrix")
    b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else
    b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}
DistrictBnds <- function() {
  plot(clpBnds2, bg = "transparent", add = TRUE)
}
