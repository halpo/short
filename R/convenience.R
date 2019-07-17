#' Convenience Wrappers
#' 
#' These functions are for convenience and modify the original 
#' function to exclude missing by default.
#' These are also highly useful to avoid warnings when converting to 
#' SQL when using [dbplyr][dbplyr::dbplyr-package] functions and remote tables.
#' 
#' @param ... Other Arguments
#' @param na.rm Remove missing?
#' 
#' @export
Min <- function(..., na.rm=TRUE)min(..., na.rm=na.rm)
#' @rdname Min
#' @export
Max <- function(..., na.rm=TRUE)max(..., na.rm=na.rm)
#' @rdname Min
#' @export
Mean <- function(..., na.rm=TRUE)mean(..., na.rm=na.rm)
#' @rdname Min
#' @export
Median <- function(..., na.rm=TRUE)stats::median(..., na.rm=na.rm)
