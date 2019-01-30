#'Cumulant generator of the unifed distribution
#'
#' @param theta A numeric vector.
#'
#' @details The cumulant generator of the unifed distribution is
#'     defined as
#'
#' \deqn{
#' \kappa(\theta)=\left\{
#'   \begin{array}{ll}
#'     \log\left(\frac{e^\theta-1}{\theta}\right)& if \theta \neq
#'                                                 0\\
#'     0 & \mbox{if }\theta=0
#'   \end{array}
#'   \right..}{                     /                                       
#'                     |     / theta    \                      
#'                     |     |e      - 1|                      
#'kappa(theta) =      | log |----------|   if theta ! =   0   
#'                     |     \   theta  /                      
#'                     |                                       
#'                      \  0                 if theta = 0  
#' 
#' }
#'
#' @return \code{unifed.kappa} returns a vector that contains the
#'     cumulant generator of the unifed distribution applied to each
#'     element of theta.
#'
#' @references{
#' Quijano Xacur, Oscar Alberto (2018). The Unifed Distribution. ArXiv. \url{http://arxiv.org/abs/1812.00251}.
#'
#' Jørgensen, Bent (1997). The  Theory  of  Dispersion  Models.
#' Chapman  &  Hall, London.
#' }
#'
#' @examples
#'
#' unifed.kappa(1)
#' unifed.kappa(-5:5)
#' 
#' @export
#' @useDynLib unifed unifed_kappa
unifed.kappa <- function(theta){
    .Call(unifed_kappa,as.numeric(theta))
}


#' @rdname unifed.kappa
#' 
#' @name unifed.kappa.prime
#'
#' @inheritParams unifed.kappa
#' 
#' @return \code{unifed.kappa.prime} returns a vector that contains
#'     the derivative of the cumulant generator of the unifed
#'     distribution for each element of theta.
#'
#' @examples
#'
#' unifed.kappa.prime(4.5)
#' 
#' @export
unifed.kappa.prime <- function(theta){
    tol <- sqrt(.Machine$double.eps)
    ifelse( abs(theta) <= tol ,
           0.5,
           1/(1-exp(-theta)) - 1/theta )                  
}


#' @rdname unifed.kappa
#'
#' @name unifed.kappa.double.prime
#'
#' Second derivative of the cumulant generator of the unifed distribution
#'
#' @inheritParams unifed.kappa
#'
#' @return \code{unifed.kappa.double.prime} returns a vector that
#'     contains the second derivative of the cumulant generator of the
#'     unifed distribution for each element of theta.
#'
#' @examples
#'
#' unifed.kappa.double.prime(0)
#' 
#' @export
unifed.kappa.double.prime <- function(theta){
    tol <- sqrt(.Machine$double.eps)
    ifelse( abs(theta) <= tol,
           1/12,
           1/theta^2  - exp(-theta)/(exp(-theta)-1)^2 )
}

#' @rdname unifed.kappa
#'
#' @name unifed.kappa.prime.inverse
#' 
#' @param mu A vector of numbers between 0 and 1
#'
#' @param ... Other parameters of \code{\link{unifed.kappa.prime.inverse.one}}
#'
#' @return \code{unifed.kappa.prime.inverse} returns a vector of the
#'     same size then mu containing
#'     \code{unifed.kappa.prime.inverse.one} evaluated at each entry
#'     of \code{mu}.
#'
#' @examples
#'
#' unifed.kappa.prime.inverse(0.5)
#' unifed.kappa.prime.inverse(c(0.1,0.7,0.9))
#' 
#' 
#' @export
#' 
unifed.kappa.prime.inverse <- function(mu,...){
    sapply(mu,function(x){unifed.kappa.prime.inverse.one(x,...)})
}


#' @rdname unifed.kappa
#'
#' @name unifed.kappa.prime.inverse.one
#'
#' @param tol Tolerance level. The algorithm stops if the proportional
#'     difference between the new and old value of an iteration is
#'     less or equal than this number.
#'
#' @param maxit Maximum number of iterations of the algorithm to look
#'     for convergence.
#'
#' @details \code{unifed.kappa.prime.inverse.one} uses the
#'     Newthon-Raphson method for finding the inverse of
#'     \code{unifed.kappa.prime} for a single value.
#'
#' @return \code{unifed.kappa.prime.inverse.one} if the tolerance
#'     level is reached within \code{maxit} iterations, the function
#'     returns the value of the last iteration. Otherwise it returns
#'     \code{NA}.
#'
#' @export
#' @useDynLib unifed unifed_kappa_prime_inverse
unifed.kappa.prime.inverse.one <- function(mu,tol=1e-7,maxit=1e7){
        
    if(mu < 0 | mu > 1)
        stop("mu must have a value between 0 and 1")

    xinit <- 0
                   
    ret <- .Call(unifed_kappa_prime_inverse,mu,tol,maxit,xinit)
    
    if(is.na(ret)){
        print("NA returned")
        print(mu)
    }
    
    ret
}


##  LocalWords:  Cumulant unifed cumulant describeIn
