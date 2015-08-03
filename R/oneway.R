#'Estimae the relevant parameters 
#'
#'Estimae the relevant parameters in a one-way AOV with possibly unequal sample sizes.
#' @title  oneway
#' @param Z a list of (possibly) named components
#' @param ... other arguments
#' @rdname oneway
#' @export oneway
#' @method oneway default
#' @S3method oneway default
oneway.default <- function(z, ...){
#
}
#' @rdname oneway
#' @method oneway factor
#' @S3method oneway factor
oneway.factor <- function(z, y, ...) {
  #
}
  oneway <- function(z, ...) UseMethod("oneway")
  oneway.default <- function(z, ...) {
  k <- length(z)
  n <- sapply(z, length)
  N <- sum(n)
  
  y <- unlist(z)
  group.sum <- sapply(z, sum)
  group.mean <- sapply(z, mean)
  
  SSB <- sum(group.sum^2/n) - sum(y)^2/N #Sum square between
  SSE <- sum(y^2) - sum(group.sum^2/n)   #sum square within/error
  
  output<-list(means = group.mean, SSB=SSB, SSE=SSE,     dfB=(k-1),dfE=(N-k))
  output$call<-match.call()
  class(output)<-"oneway"
  output
}
  oneway.factor <- function(z, y, ...) {
  z<-tapply(y,z,list)
  oneway.default(z)
}
