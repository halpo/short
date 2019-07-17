
get_release_info <- function(pkg = '.'){
    requireNamespace('lubridate')

    release.file <- file.path(pkg, "CRAN-RELEASE")
    assert_that(file.exists(release.file)
               , msg="Cannot find 'CRAN-RELEASE' file." )

    content <- readLines(release.file)

    pat.release.date <- "^This package was submitted to CRAN on (\\d{4}-\\d{2}-\\d{2})\\.$"
    pat.commit <- "^Once it is accepted.*\\(commit ([a-f0-9]+)\\)\\.$"

    if (assert_that( grepl(pat.release.date, content[[1]])
                   , grepl(pat.commit, content[[2]])
                   , msg="'CRAN-RELEASE' does not appear to be valid."))
        list( content = content
            , released.on = lubridate::ymd(gsub(pat.release.date, "\\1", content[[1]]))
            , commit = gsub(pat.commit,"\\1", content[[2]])
            )
}

git_show <- function(file, commit = 'HEAD'){
    git <- Sys.which('git')
    assert_that(git != '', msg="Could not find git.")
    system2(git, list('show', paste0(commit, ":", file)), stdout = TRUE)
}

#' Read a package DESCRIPTION file
#' 
#' Read a package "DESCRIPTION" file, returning
#' the results as a [tibble()] with the appropriate fields
#' parsed and evaluated. List fields, depends, imports, suggests
#' and enhances, are converted into nested lists.
#' 
#' @param file the path to the "DESCRIPTION" file.
#' @param ... passed to [read.dcf()].
#' @param text optional to specify content as a character vector of lines.
#' 
#' @export
read_description <- function(file, ..., text = NULL){
    if(!missing(text)){
        assert_that(missing(file) || is.null(file))
        file <- textConnection(text)
        on.exit(try(close(file), silent = TRUE))
    }
    description <- read.dcf(file, ...)
    mutate_present <- function(., .vars, ...)
        dplyr::mutate_at(., intersect(dplyr::tbl_vars(.), .vars), ...)
    eval_text <- rlang::as_function(~eval(parse(text=.), envir=globalenv()))

    tibble::as_tibble(description) %>% dplyr::rename_all(tolower) %>%
        mutate_present(c('authors@r', 'roxygen'), purrr::map, eval_text) %>%
        mutate_present(c('depends', 'enhances', 'imports', 'suggests')
                      , stringi::stri_split, regex=",\\s*")
}


#' Tag a package release
#' 
#' Use the CRAN-RELEASE file to automate the tagging of the 
#' commit with the version.
#' 
#' @param pkg the base directory of the package
#' @param commit the string identifying the git commit to tag with the 
#'               release.  By default this is read from the 
#'               "CRAN-RELEASE" file.
#' @param ... passed to the [git2r::tag()] function
#' @param dev.version Should the version number be incremented 
#'                    after committing the tag?
#' @param push Should the changes be pushed to the default git repository?
#' 
#' @seealso [git2r::tags()], [usethis::use_dev_version()], [git2r::push()]
#' 
#' @export
tag_release <-
function( pkg = '.'
        , commit = info$commit
        , ...
        , dev.version = TRUE
        , push = TRUE
        ){
    assert_that( requireNamespace('lubridate')
               , requireNamespace('git2r')
               , dir.exists(pkg)
               , file.exists(file.path(pkg, "DESCRIPTION"))
               , file.exists(file.path(pkg, "CRAN-RELEASE"))
               )
    info <- get_release_info(pkg)

    description <- read_description(text=git_show('DESCRIPTION', commit))
    version = description$version

    tag = paste0("v", version)
    msg = "Version" %<<% version %<<% "released\n" %\%
          info$content[[1]] %\%
          "The version was tagged on" %<<% lubridate::today()

    val <- git2r::tag(pkg, tag, msg, ...)

    file.remove(file.path(pkg, "CRAN-RELEASE"))

    if (dev.version) try(usethis::use_dev_version(pkg))
    if (push) try(git2r::push(pkg))
    invisible(val)
}
