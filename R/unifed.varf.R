#' Variance function of the unifed distribution
#' 
#' @param mu A vector with numbers between 0 and 1.
#'
#' @return It returns
#'     \code{unifed.kappa.double.prime(unifed.kappa.prime.inverse(mu))}. 
#' @export

unifed.varf <- function(mu){
    unifed.kappa.double.prime(unifed.kappa.prime.inverse(mu))    
}
