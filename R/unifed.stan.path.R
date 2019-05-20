#' Unifed Stan function paths
#'
#' The unifed.stan provided by the file contains functions for using
#' the unifed distribution in stan. The file can be included (with
#' #include) insided the functions block of a stan program or its
#' contents can be copied and pasted.
#'
#' @return The full path to the unifed.stan file provided by the
#'     package.
#' 
#' @export
unifed.stan.path <- function(){
    system.file("stan/unifed.stan",package="unifed")
}

#' @rdname unifed.stan.path
#' @name unifed.stan.folder
#'
#' @return \code{unifed.stan.folder} returns a string containing the
#'     path to the folder containing the unifed.stan file. This can be
#'     used as the \code{isystem} parameter in stan functions.
#' @export
unifed.stan.folder <- function(){
    system.file("stan/",package="unifed")
}
