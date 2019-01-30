#' Summarizing Generalized Linear Model Fits
#'
#' Wrapper function for summary.glm.
#'
#' @param object an object of class "glm".
#'
#' @param ... Other arguments for \code{stats::summary.glm}.
#' 
#' This wrapper function was created in order to automatically set to
#' 1 the dispersion parameter of a fitted unifed GLM. When the package
#' is loaded the summary method of the glm class is rewritten using
#' this function.
#' 
#' @export 
summary_unifed_glm<- function(object,...){
    args.list <- list(...)
    if( object$family$family=="unifed" &
        ( !("dispersion" %in% names(args.list)) | is.null(args.list$dispersion)) ){
        stats::summary.glm(object,dispersion=1,...)
    }else{
        stats::summary.glm(object,...)
    }    
}
