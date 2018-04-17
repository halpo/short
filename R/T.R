#' Lazily create a character vector
#' 
#' Lazily create a character vector.
#' Any argument passed to .T will be converted to
#' a string without evaluating the symbols.
#' 
#' @param ... objects to quote
#' 
#' @examples 
#' .T(hell0, world)
#' .T(1+2)
#' .T("this how you include spaces")
#' 
#' @rdname T
#' @export
.T <- function (...) 
{
    c <- as.character(x <- substitute(c(...)))[-1]
    names(c) <- names(as.list(x))[-1]
    return(c)
}