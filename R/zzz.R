#' @import assertthat
#' @import dplyr
#' @import methods
#' @importFrom rlang quos quo enquo expr quo_expr eval_tidy set_names names2 as_function
#' @importFrom magrittr %$% %<>% %>% %T>%
#' @importFrom pkgcond %\% %<<% %<<<%
NULL

assert_that <- pkgcond::assert_that
message <- pkgcond::pkg_message
warning <- pkgcond::pkg_warning
stop <- pkgcond::pkg_error
