#' Maximum Likelihood Estimate for the unifed distribution
#'
#' @param x A numeric vector with values in the interval [0,1].
#'
#' Computes the maximum likelihood estimator of the canonical
#' parameter of the unifed distribution. It is assumed that the
#' elements of \code{x} come from independent and identically
#' distributed unifed random variables.
#'
#' @examples
#' a.unifed.sample <- runifed(1000,10)
#' theta.mle <- unifed.mle(a.unifed.sample)
#' 
#' @export
unifed.mle <- function(x){
    
    domain.test <- sum( x < 0 | x > 1 )
    
    if( domain.test > 1)
        stop("x contains elements that are not in the interval [0,1]")
    
    unifed.kappa.prime.inverse(mean(x))
}
