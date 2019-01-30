
#' @importFrom methods setMethod 
.onLoad <- function(libname, pkgname){    
    setMethod(f="summary",
              signature="glm",
              definition=function(object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, ...){
                  if( object$family$family=="unifed" & is.null(dispersion) ){
                      unifed::summary_unifed_glm(object,dispersion=1 ,correlation=correlation, symbolic.cor=symbolic.cor, ...)
                  }else{
                      unifed::summary_unifed_glm(object,dispersion=dispersion ,correlation=correlation, symbolic.cor=symbolic.cor, ...)
                  }},
              where=.GlobalEnv
              )
}
