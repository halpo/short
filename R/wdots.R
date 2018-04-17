
wargs2 <- function(f, ...){
    f2 <- f
    dots <- list(...)
    stopifnot(all(names(dots)!=""))
    env <- as.environment(dots)
    parent.env(env) <- environment(f)
    environment(f2) <- env
    formals(f2) <- formals(f2)[setdiff(names(formals(f2)), ls(env))]
    f2
}
wdots <- function(fun, ..., envir=environment(fun)){
    call <- match.call()
    a <- formals(fun)
    w <- which(names(a)=="...")
    call[[1]] <- call[[2]]
    names(call)[[2]] <- ""
    call[[2]] <- as.name("...")
    as.function(c(alist(...=), call), envir=envir)
}
if(F){
    wdots(summarize, mean=mean(x))
    wdots(dnorm, mean=100, sd=100)
    
    library(dplyr)
    S <- specify_dots( summarize
                     , Sepal.area = mean(Sepal.Length * Sepal.Width)
                     )
    iris %>% group_by(Species) %>% S
    
    S2 <- wargs2(summarize
                , Sepal.area = mean(Sepal.Length * Sepal.Width)
                )
    
}





