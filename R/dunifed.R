#' The unifed distribution
#'  
#' @description Density, distribution function, quantile function and
#'     random generation for the unifed distribution.
#'
#' @param x A vector of quantiles. They must be numbers between 0 and 1.
#'
#' @param theta The value of the canonical parameter. It must be of length one.
#'
#' @return \code{dunifed} gives the density function.
#'
#' @references{
#' Quijano Xacur, O.A. The unifed distribution. J Stat Distrib App 6, 13 (2019).
#' doi:10.1186/s40488-019-0102-6.
#' 
#' }
#'
#' @examples
#'
#' dunifed( c(0.1,0.3,0.7), 10)
#' 
#' @export
dunifed <- function(x,theta){
    if(length(theta) > 1)
        stop("thetha must be of length one")
    ifelse( x < 0 | x > 1,
           0,
           exp( x * theta - unifed.kappa(theta) ))
}

#' @rdname dunifed
#  @name unifed.lcdf
#'
#' @return \code{unifed.lcdf} returns the log of the cumulative
#'     distribution function of the unifed.
#'
#' @examples
#' 
#' x <- c(0.3,0.6,0.9)
#' unifed.lcdf(x,5)
#'
#' @export
unifed.lcdf <- function(x,theta){
     if(length(theta) > 1)
        stop("theta must be of length one")    
    ret <- numeric(length(x))
    less.than.zerop <- x<=0
    greater.than.onep <- x>=1
    between.zero.and.onep <- !( less.than.zerop  | greater.than.onep )
    ret[less.than.zerop] <- -Inf
    ret[greater.than.onep] <- 0

    if ( abs(theta) <= sqrt(.Machine$double.eps) )
        ret[ between.zero.and.onep ] <- log(x[between.zero.and.onep])
    else{
        x.subset <- x[between.zero.and.onep]
        if(theta <= 50)
            ret[ between.zero.and.onep ] <- log( expm1(x.subset*theta)   / expm1(theta)  )
        else
            ret[ between.zero.and.onep ] <- (x.subset-1)*theta + log(-expm1(-theta*x.subset)) - log(-expm1(-theta))
    }
    
    ret
}

#' @rdname dunifed
#' @name punifed
#'
#' @param q A vector of quantiles.
#'
#' @return \code{punifed} gives the distribution function.
#'
#' @examples
#'
#' x <- c(0.1,0.4,0.7,1)
#' punifed(x,-5)
#' 
#' @export
punifed <- function(q,theta){
    exp(unifed.lcdf(q,theta))    
}


#' @rdname dunifed
#' @name qunifed
#'
#' @param p A vector of probabilities.
#'
#' @return \code{qunifed} gives the quantile function.
#'
#' @examples
#'
#' p <- 1:9/10
#' qunifed(p,5)
#' 
#' @export
#' 
qunifed <- function(p,theta){
    if( T %in% (p<0 | p>1) )
        stop("The values of p must be a number between 0 and 1")
    if(length(theta) > 1)
        stop("thetha must be of length one")
    if ( abs(theta) <= sqrt(.Machine$double.eps) )
        p
    else
        log(p * ( exp(theta) -1 ) + 1 ) / theta
    
}

#' @rdname dunifed
#' @name runifed
#'
#' @param n number of observations
#'
#' @return \code{runifed} generates random observations.
#'
#' @examples
#'
#' runifed(20,-3.3)
#' 
#' @export 
runifed <- function(n,theta){
    qunifed( runif(n) , theta  )
}

##  LocalWords:  quantile
