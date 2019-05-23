#' Filter a character vector
#' 
#' `sift` filters the list to elements matching the pattern.
#' `sieve` filters them out.
#' 
#' @inheritParams base::grepl
#' @param ... Passed on
#' @param on.names Perform filtering on the names of the vector instead.
#' 
#' @export
sift <- 
function( x
        , pattern, value=TRUE, perl=TRUE
        , ignore.case=TRUE, on.names = FALSE, invert=FALSE
        , ...){
    if(on.names){
        . <- grepl(pattern=pattern, x=names(x), ignore.case=ignore.case, perl=perl, ...)
        if(invert) . <- !.
        x[.]
    } else {
        unique(grep( pattern=pattern, x=x, ignore.case=ignore.case, perl=perl
                   , value=value, invert=invert, ...))
    }
}
#' @rdname sift
#' @export
sieve <- function(..., invert=TRUE){
    sift(..., invert=invert)
}
