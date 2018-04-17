#' Along a table dimension
#' 
#' functions as \code{\link{seq_along}} but for rows or columns
#' of a table like structure.
#' 
#' @param x a table like structure.
#' 
#'  
#' @export
along_rows <- function(x){if(nrow(x)>0L) seq(1L, nrow(x), by=1L) else integer(0)}
#' @rdname along_rows
#' @export
along_cols <- function(x){if(ncol(x)>0L) seq(1L, ncol(x), by=1L) else integer(0)}

