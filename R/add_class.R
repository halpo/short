#' Add to object helpers
#' 
#' @param x   object to alter
#' @param new new characteristic
#' 
#' @description
#' These function make using magrittr easier when altering an object.
#' 
#' @export
add_class <- function(x, new)structure(x, class = c(new, class(x)))

#' @describeIn add_class Overwrite the class
#' @export
set_class <- function(x, new)structure(x, class = c(new, class(x)))

#' @describeIn add_class Add a comment
#' @export
add_comment<- function(x, new) structure(x, comment = c(comment(x), new))

#' @describeIn add_class Overwrite the comment
#' @export
set_comment<- function(x, new) structure(x, comment = new)
