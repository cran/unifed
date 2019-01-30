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
#' Quijano Xacur, Oscar Alberto (2018). The Unifed Distribution. ArXiv. \url{http://arxiv.org/abs/1812.00251}.
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

    if(length(theta) > 1)
        stop("thetha must be of length one")    
    ret <- numeric(length(q))
    less.than.zerop <- q<=0
    greater.than.onep <- q>=1
    between.zero.and.onep <- !( less.than.zerop  | greater.than.onep )
    ret[less.than.zerop] <- 0
    ret[greater.than.onep] <- 1

    if ( abs(theta) <= sqrt(.Machine$double.eps) )
        ret[ between.zero.and.onep ] <- q[between.zero.and.onep]
    else{
        q.subset <- q[between.zero.and.onep]
        ret[ between.zero.and.onep ] <- ( ( exp(q.subset*theta) -1 )  / ( exp(theta) -1 )  )
    }
    
    ret
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
