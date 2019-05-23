#' Check if R is starting up.
#' 
#' @export
is_r_startup <- function(){
    root <- sys.call(1)
    !is.null(root) && (deparse(root) == ".First.sys")
}

#' Intelligently load libraries
#' 
#' Load a list of libraries.  
#' If the library is not installed install it and load it.
#' If using is called during R startup, such as in a
#' [.Rprofile] file append the package(s) to the `"defaultPackages"`
#' option to induce loading at the appropriate time.
#' 
#' @param ... a list of packages to install, evaluated lazily.
#' @param install.missing Should an attempt be made to install 
#'                        packages if they are not installed?
#' @param quiet run without messages.
#' 
#' @examples
#' \dontrun{
#' using(tools)
#' }
#' @export
using <- 
function( ...
        , install.missing=getOption("using::install.missing", !is.null(getOption("repos")))
        , quiet = getOption("using::quiet", interactive())
        ){
    libs <- as.character(substitute(c(...)))[-1]
    if(is_r_startup()){
        options(defaultPackages = c(getOption("defaultPackages"), libs))
        invisible(FALSE)
    } else {
        is.loaded <- suppressWarnings(sapply(libs, require, quietly=FALSE, character.only=TRUE))
        if(any(!is.loaded)){
            if(install.missing){
                need.to.install <- libs[!is.loaded]
                if (!quiet)
                    message( "Installing requested packages:"
                           , paste(need.to.install, collapse= ", ")
                           )
                utils::install.packages(need.to.install, type=if(.Platform$OS.type=="unix") 'source' else 'both', quiet=TRUE)
                is.loaded <- sapply(libs, require, quietly=quiet, character.only=T)

            } else {
                stop("The following package(s) are not installed: ", paste(libs[!is.loaded], collapse=", "))
            }
        }
        invisible(is.loaded)
	}
}
