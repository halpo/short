
ensure_named_quos <- function(Q){
    assert_that(is.list(Q))
    set_names(Q, coalesce(na_if(names2(Q), ''), purrr::map_chr(Q, quo_name)))
}
if(F){
    Q <- quos(a=b,c)
    expect_identical(ensure_named_quos(Q),quos(a=b,c=c))
}


#' Operate over margins
#' 
#' Wrap a function to operate over groups and all possible
#' marginals of groupings.
#' 
#' @param FUN the functino to wrap.
#' @param all.name the string to use to represent that a variable was marginalized over.
#' 
#' @export
with_margins <- 
function( FUN #< Function to compute one groups and margins.
        , all.name = "(All)"
        ){
    function(.data, ...){
        fix_not_present <- 
            function(vars_not_present){
                if (length(vars_not_present) == 0) return(rlang::exprs())
                if (inherits(.data, 'tbl_lazy')) {
                    vars_not_present %>% 
                        rlang::rep_along(all.name) %>% 
                        rlang::set_names(vars_not_present)
                } else {
                    vars_not_present %>%  
                        purrr::map(~
                            if (is.factor(pull(.data, .))){
                                old.levels <- levels(pull(.data, .))
                                assert_that(!(all.name %in% old.levels))
                                new.levels <- c(all.name, old.levels)
                                rlang::expr(
                                    factor( all.name
                                          , levels = !!new.levels
                                          , ordered = !!is.ordered(pull(.data, .))
                                          )
                                )
                            } else 
                                all.name
                        ) %>% rlang::set_names(vars_not_present)
                }
            }
        fix_present <- 
            function(vars){
                if (length(vars) == 0 ) return(rlang::exprs())
                if (inherits(.data, 'tbl_lazy')) {
                    rlang::exprs()
                } else {
                    vars %>% rlang::syms() %>% purrr::map(~rlang::expr(
                        if (is.factor(!!.)) 
                            forcats::fct_expand(!!., all.name) %>% 
                            forcats::fct_relevel(all.name)
                        else 
                            !!.
                    )) %>% rlang::set_names(vars)
                }
            }
        g   <- group_vars(.data)
        com <- Reduce(c,lapply(seq(0, length(g)), utils::combn, x=g, simplify=FALSE)) 
        com <- com[order(desc(sapply(com, length)))]
        lapply(com, function(x, .data){
            vars_present  <- intersect(g,x)
            vars_not_present <- setdiff(g,x)
            FUN(group_by(ungroup(.data), !!!rlang::syms(x), add=FALSE), ...) %T>%
                { assert_that( !any(tbl_vars(.) %in% as.character(setdiff(g, x)))
                             , msg = "Unexpected grouping variable in output of {deparse(substitute(FUN))}" %>% glue::glue()
                             )
                } %>%
            mutate(!!!fix_not_present(vars_not_present)) %>% 
            ungroup() %>%
            mutate(!!!fix_present(vars_present))
        }, .data) %>%
        purrr::reduce(dplyr::union_all)
    }
}
if(F){
    .data <- expand.grid( x = .T( a, b, c)
                        , y = .T( d, e, f)
                        , .rep = 1:10
                        ) %>%
             mutate( v = rnorm(90)) %>%
             select(-.rep)
    
    with_margins(summarise)(group_by(.data, x, y), N=n(), sum=sum(v))
}


as_quos <- . %>% purrr:::map(., rlang::parse_quosure) %>% set_class('quosures')


levels2 <- function(x){
    if (x %is a% 'factor')
        return(levels(x))
    else if (x %is a% 'character')
        return(sort(unique(x)))
    else 
        return(sort(unique(as.character(x))))
}
if(F){
    x <- ordered(c('b', 'a', 'c'), levels=.T(c, b, a))
    expect_equal(levels2(x), levels(x))

    x <- .T(c, b, a)
    expect_equal(levels2(x), c('a', 'b', 'c'))
    
    x <- 1:3
    expect_equal(levels2(x), c('1','2','3'))
}

#' Spreach multiple variables
#' 
#' This is a multiple variable version of the tidyr function 
#' \code{\link[tidyr]{spread}}.
#' 
#' @inheritParams tidyr::spread
#' @param ... the columns to act as the values to spread out.
#' @param .   The separator between the key levels and the value column names.
#' 
#' @export
spread_each <- 
function( data                          #< A tibble or compatible object.
        , key                           #< key to be used as for the 'super-columns'
        , ...                           #< Value variables to be spread
        , fill=NA                       #< a single value or a named list of values to fill in structural missing.
        , convert=FALSE                 #< See <spread>
        , drop=FALSE                    #< See <spread>    
        , sep='.'                       #< the separator to be used to separate the names of the super- and sub-columns.
        ){
    key <- rlang::enquo(key)
    dots <- rlang::quos(...)
    assert_that( is.flag(convert)
               , is.flag(drop)
               , is.string(sep)
               )
    key.var <- select_var(tbl_vars(data), !!key)
    value.cols      <- dplyr::select_vars(tbl_vars(data), !!!dots)
    retained.groups <- group_vars(data) %>% setdiff(key.var)
    grouping.cols   <- tbl_vars(data) %>% setdiff(key.var) %>% setdiff(value.cols)
    
    assert_that(rlang::is_dictionaryish(value.cols))
    if (!is.null(names(fill))) {
        if (all(. <- rlang::have_name(fill))) {
            assert_that( rlang::is_dictionaryish(fill)
                       , all(names(value.cols) %in% names(fill))
                       )
        } else {
            assert_that(sum(!.)==1L, msg='`fill` should have only one default/unnamed value')
            fill <- fill[match(value.cols, names(fill), nomatch = which(!.))] %>% 
                rlang::set_names(value.cols)
        }
    } else {
        fill <- rlang::rep_along(value.cols, fill) %>% rlang::set_names(value.cols)
    }
    
    key.levels <- pull(data, key.var) %>% levels2
    
    f <- function(col, name){
        data %>% dplyr::ungroup() %>%
        dplyr::select( key.var, col, grouping.cols) %>%
        tidyr::spread( key   = key.var
                     , value = col
                     , fill  = fill[[name]]
                     , sep   = NULL
                     ) %>%
        dplyr::rename(!!!( rlang::set_names(key.levels, paste(key.levels, name, sep=sep))))
    }
    value.cols %>% 
        purrr::imap(f) %>% 
        purrr::reduce(full_join, by=grouping.cols) %>%
        dplyr::select( !!!grouping.cols
                     , !!!( purrr::map(key.levels, ~rlang::expr(starts_with(!!.))) )
                     ) %>% 
        dplyr::group_by(!!!retained.groups)
}
if(F){
    
    .dots <- lazyeval::lazy_dots(Age_Group)
    debugonce(dplyr:::group_by_prepare)
    
    debugonce(dplyr:::select_.grouped_df)
    select(data, 1, 4)
    
    
    
    debug(spread_each)
    debug(spread_each_)
    .data <- expand.grid( x = .T( a, b, c)
                        , y = .T( d, e, f)
                        , .rep = 1:10
                        ) %>%
             mutate( v = rnorm(90)) %>%
             select(-.rep)
    .data %>%
        group_by(x, y) %>%
        summarise(N=n(), sum=sum(v)) %>%
        spread_each(y, N, sum)
    
}


