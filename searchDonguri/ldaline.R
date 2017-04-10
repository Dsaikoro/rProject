ldaline <- function(res) {
  x0 <- (res$means[1,1]*res$count[1]+res$means[2,1]*res$count[2])/res$N
  y0 <- (res$means[1,2]*res$count[1]+res$means[2,2]*res$count[2])/res$N
  a0 <- res$scaling[1]*x0+res$scaling[2]*y0
  a  <- c(a0/res$scaling[2],-res$scaling[1]/res$scaling[2])
  return(a)
}
