#' Irwin-Hall density
#'
#' @param x A number between 0 and \code{n}.
#'
#' @param n Number of uniform distributions in the unit interval to sum.
#'
#' @param log If it evaluates to \code{TRUE} it returns the log of the
#' density instead of the density.
#'
#' @details Gives the density of the Irwin-Hall distribution. It is
#'     the density of the sum of \code{n} uniform distributions on the
#'     interval (0,1).
#'
#' \deqn{
#'   h(y;n) = \frac{1}{(n-1)!}\sum_{k=0}^{ \left\lfloor y \right\rfloor } (-1)^k {n \choose k} (y-k)^{n-1}
#' }{
#'                 1       __ |_y_|       k /  n \         n - 1 
#' h(y;n)  =  ---------  \         ( - 1)   |    |  (y - k)      
#'             (n - 1)!  /__ k = 0          \ k  /               
#' }
#'
#' where \eqn{x \in [0,1]} and \eqn{n} is a positive integer.
#'
#' This function is not numerically stable. The examples have some cases of this.
#'
#' @examples
#'
#' dirwin.hall(2,5)
#' 
#' # Numerically unstable example
#' # Run the following one after the other
#' # See how it goes from positive to negative (which means overflowing )
#' dirwin.hall(35,50)
#' dirwin.hall(36,50)
#' dirwin.hall(37,50)
#' dirwin.hall(38,50)
#' 
#' @export

dirwin.hall <- function(x,n,log=FALSE){
    
    ret1 <- 0
    if ( x < 0 | x > n)
        stop("x must be greater or equal to 0 and less or equal to n")
    for(k in 0:floor(x))
        ret1 <- ret1+ (-1)^k*choose(n,k)*(x-k)^(n-1)
    if(log){
        if( n==1)
            return( log(ret1) )
        else
            return( log(ret1)-sum(log(1:(n-1)))  )
    }else{
        if( n==1)
            return( ret1   )
        else
            return( ret1 / (factorial(n-1)) )
    }
}
