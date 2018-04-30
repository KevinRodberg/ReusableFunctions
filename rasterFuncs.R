
#=======================================================================
# Functions define a few types of series used in color ramps at setttings
#=======================================================================

# Eliminate Scientific Notation
options(scipen = 1000000)
myTheme = rasterTheme(region = brewer.pal('Oranges', n = 9))
yourTheme = rasterTheme(region = brewer.pal('Blues', n = 9))
newTheme = rasterTheme(region = brewer.pal('RdBu', n = 9))


geomSeries <- function(base, max) {
  (base ^ (0:floor(log(max, base))) - 1) / 100
}
equalIntSeries <- function(base, max) {
  a <- c(base, max)
  adj2dig <- sign(a) * ceiling(abs(a) * 100) / 100
  interval <- (adj2dig[2] - adj2dig[1]) / 10
  seq(adj2dig[1], adj2dig[2], interval)
}
normSeries <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

buildWMDbnd<- function(projStr){
  WMDbnd.Path <- "//whqhpc01p/hpcc_shared/krodberg/NexRadTS"
  WMDbnd.Shape <- "CntyBnds.shp"
  setwd(WMDbnd.Path)
  WMDbnd <- readShapePoly(WMDbnd.Shape, proj4string = projStr)
  return(WMDbnd)
}

