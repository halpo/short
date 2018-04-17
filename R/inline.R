#' @name inline
#' @title Inline shortcuts and aliases
#' 
#' @description 
#' A collection of inline operators to simplify and make your code
#' prettier.
#' 
#' @param lhs left hand side
#' @param rhs right hand side
#' 
#' @examples 
#' "Hello" %\n% "world"
#' "Hello" %<<% "world"
#' "Hello" %<<<% "world"
#' .T(Hello, world)  %~% "world"
#' character(0) %or% "pick me"
#' NULL %or% "no pick me"
#' "Hello world" %inherits% "character"
#' 
#' 
NULL


#' @describeIn inline Concatenate with new line.
#' @export
`%\\n%` <- function(lhs, rhs)paste(lhs, rhs, sep="\n")

#' @describeIn inline Concatenate with space
#' @export
`%<<%` <- function(lhs, rhs)paste(lhs, rhs, sep=" ")

#' @describeIn inline Concatenate without space
#' @export
`%<<<%` <- function(lhs, rhs)paste(lhs, rhs, sep="")

#' @describeIn inline inline pattern match
#' @export
`%~%`  <- function(lhs, rhs)grepl(pattern=rhs, x=lhs, ignore.case=TRUE, fixed=TRUE)

#' @describeIn inline inline pattern "does not match"
#' @export
`%!~%` <- function(lhs, rhs)!grepl(pattern=rhs, x=lhs, ignore.case=TRUE, fixed=TRUE)

#' @describeIn inline Return x unless \code{length(lhs)==0} then return \code{rhs}.
#' @export
`%or%` <- function(lhs, rhs) if (length(lhs)) lhs else rhs

#' @describeIn inline Inline alias of \code{\link[base]{inherits}}
#' @export
`%inherits%` <- function(lhs, rhs)inherits(lhs, rhs)

#' @describeIn inline Alternative form of inherits.
#' @export
`%is a%` <- `%inherits%`

assertthat::on_failure(`%inherits%`) <- 
assertthat::on_failure(`%is a%`) <- function(call, env){
    # browser()
    paste0(deparse(call$x)," is not a ", deparse(call$what))
}
if(F){
    x <- list(1,2)
    expect_true(see_if(x %is a% 'list'))
    expect_false(see_if(x %is a% 'function'))
    expect_equal(attr(see_if(x %is a% 'function'), 'msg'), 'x is not a "function"')
}

