#' Deviance of the unifed distribution
#'
#' @param y.v A numeric vector with values between 0 and 1
#'
#' @param mu.v A numeric vector with values between 0 and 1
#'
#' @param wt (default value: 1) The weight vector. It contains the
#'     weight of each observation. It must contain positive integers
#'     only.
#'
#' @param ... Additional parameters of \code{\link{unifed.kappa.prime.inverse.one}}
#'
#' @return \code{unifed.deviance} returns the deviance of a GLM with a
#'     unifed response distribution. This is
#'
#' \deqn{
#'  D(\bm{y},\bm{\mu})=\sum_{i=1}^m w_i d(y_i,\mu_i)
#' }{%
#'            __ m                   
#'D(y,mu) = \          w_i  d(y_i ,mu_i )
#'          /__ i = 1         
#'
#' }
#'
#' Where \eqn{d(y_i,\mu_i)}{d(y_i,mu_i)} is the unit deviance of the
#' unifed distribution between the i-th entry of \eqn{\bm{y}}{y} and
#' \eqn{\bm{\mu}}{mu}. \eqn{w_i}{w_i} is the i-th entry of the weight
#' vector. \code{\link{unifed.unit.deviance}} is used to get the value
#' of \eqn{d}{d}.
#'
#' @export
unifed.deviance <- function(y.v,mu.v,wt=1,...){
    sum( wt * mapply( function(x,y){unifed.unit.deviance(x,y,...)}, y.v, mu.v) )
}


#' @rdname unifed.deviance
#' 
#' @name unifed.unit.deviance
#'
#' @param y A vector with values between 0 and 1.
#'
#' @param mu A vector with values betwee0 and 1.
#'
#' @param tol Tolerance level for the Newton-Raphson algorithm for
#'     computing the inverse of the derivative of the cumulant
#'     generator of the family.
#'
#' @param maxit Maximum number of iterations for the Newton-Raphson
#'     algorithm for computing the inverse of the derivative of the
#'     cumulant generator of the family.
#'
#' @details \code{unifed.unit.deviance} uses the following expression
#'     for the deviance of regular exponential dispersion families
#'
#' \deqn{
#'     d(y,\mu)=2\left[y\{\dot{\kappa}^{-1}(y)-\dot{\kappa}^{-1}(\mu)\}-\kappa(\dot{\kappa}^{-1}(y))+\kappa(\dot{\kappa}^{-1}(\mu))\right]}{                    - 1         - 1           - 1           - 1
#'  d(y,mu) =  2[y( (k')  (y) - (k') (mu) ) - k((k') (y)) + k((k') (mu))].
#'
#' }
#'
#' \eqn{\dot{\kappa}^{-1}}{(k')^(-1)} is computed with the function
#' \code{\link{unifed.kappa.prime.inverse}} from this package.
#'
#' @return \code{unifed.unit.deviance}
#' @export
#' @useDynLib unifed unit_deviance
unifed.unit.deviance <- function(y,mu,tol=1e-7,maxit=50){    
    .Call(unit_deviance,y,mu,tol,maxit)
}


##  LocalWords:  unifed cumulant
