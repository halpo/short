
# Names are modes.
# first argument should be name of reader function and should be named reader.

known.file.readers <- new.env()

known.file.readers$xlsx = list( reader = "read_xlsx"
                 , description = "Open XML for Microsoft Excel 2007 or newer."
                 , mime = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet "
                 , ext = "xlsx"
                 , default.args  = list(sheetIndex=1)
                 )
known.file.readers$xls  = list( reader = "read.xls"
                 , library= "gdata"
                 , description = "Microsoft Office Excel, Old Format."
                 , mime = "application/vnd.ms-excel"
                 , ext = "xls"
                 )
known.file.readers$csv  = list( reader = "read.csv"
                 , description = "Comma separated file"
                 , mime = "text/csv"
                 , ext  = ".csv"
                 , library = "utils"
                 , default.args = list(strip.white=TRUE)
                 )

find_mime_reader <- function(mimetype){
    for(info in known.file.readers) {
        if(!is.null(m <- info$mime))
            if(m == mimetype) return(info$reader)
    }
    return(NULL)
}
find_ext_reader <- function(ext){
    for(info in known.file.readers) {
        if(!is.null(e <- info$ext))
            if(e == ext) return(info$reader)
    }
    return(NULL)
}
add_reader <- function(name, ...){
#TODO

}


#' A "Lazy" read version
#' 
#' We often find that we are reading the same type of files over and 
#' over again.  Read is a lazy version of the read.* functions.
#' It tries to intelligently pick the appropriate reader from 
#' file extension or mime type info.
#' 
#' @param file The file(s) or pattern of files to read.
#' @param ...  options to pass to the reader functions.
#' @param verbose Print messages?
#' @param glob interpret patterns as globs?
#' 
#' @details 
#' read will first check if the file exists as specified.
#' If the file is not found then it will check if the file 
#' represents a pattern converted to a regular expression 
#' through \code{\link[utils]{glob2rx}} if glob=TRUE (default).
#' Note that only one pattern can be considered if file is
#' of length greater than 1 it will assume that exact file 
#' names are given.
#' 
#' If multiple files are given or found through pattern 
#' match then the results are given in a list, otherwise
#' a single object is returned. For elements 
#' of said list, as in the case of a single result,
#' an attempt will be made to convert results to 
#' a [tibble] through \code{\link[tibble]{as_tibble}}.
#' 
#' Read will first look for readers 
#' with the following priority
#' \enumerate{
#'   \item Known readers with the given name
#'   \item Known readers registered to handle the given mime-type
#'   \item Known readers registered to handle the given file extension
#'   \item Any loaded function of the form \code{read.<<ext>>}
#'   \item Any loaded function of the form \code{read_<<ext>>}
#'   \item Any loaded function of the form \code{read<<ext>>}
#' }
#' where \code{<<ext>>} is replaced by the extension of the file.
#' 
#' @examples
#' \dontrun{
#' # reads all the xlsx files in a directory
#' read("*.xlsx")
#' 
#' # reads all the xlsx files in the data directory
#' read("data/*.xlsx")
#' 
#' 
#' }
#' 
#' @importFrom rlang %||%
#' @export
read <- 
    function( file #< File to read.
            , ...  #< Passed to read.* method
            , verbose = FALSE  #< print informational messages.
            , glob = TRUE      #< does the pattern represent a glob.
            ){
    if(length(file)==1 && !file.exists(file)){
        if(verbose) message("File not found looking for pattern...")
        dir <- dirname(file)
        pat <- if (glob) utils::glob2rx(basename(file)) else basename(file)
        .f <- list.files(dir, pat)
        if (length(.f))
            file <- file.path(dir, .f)
        else stop("Could not find file!")
    }
    if(length(file)>1) return(lapply(file, read))
    ext  <- tools::file_ext(basename(file))
    if(.Platform$OS.type=="windows" && ext == ".lnk"){
        if(verbose) message("Link file provided, reading link.")
        file <- wingui::read_lnk(file)
        ext  <- tools::file_ext(basename(file))
    }
    
    class <- if(ext == "") basename(file) else gsub("^\\.", '', ext)
    fun <- 
        if(class %in% names(known.file.readers)){
            info <- known.file.readers[[class]]
            if("package" %in% names(info))
                stopifnot(require(info$package, character.only =TRUE))
            match.fun(info$reader %||% info[[1]])
        } else if(!is.null(.f <- find_mime_reader(mime <- mimetype(file)))) .f
        else if(!is.null(.f   <- find_ext_reader(ext)))  .f
        else if(exists(.f <- paste0("read.", class))) match.fun(.f)
        else if(exists(.f <- paste0("read_", class))) match.fun(.f)
        else if(exists(.f <- paste0("read" , class))) match.fun(.f)
        else stop("Could not find an appropriate reader")
    result <- fun(file, ...)
    result2 <- try(as_tibble(result))
    if (result2 %inherits% 'tibble') return (result2) else return(result)
}


mimetype_windows <- 
    function(ext){
        ext = tools::file_ext(ext)
        if(!grepl("^\\.", ext)) ext <- paste0(".", ext)
        reg.data <- try(utils::readRegistry(ext, "HCR"),  silent=TRUE)
        if(inherits(reg.data, "try-error")) return("application/octet-stream")
        if("Content Type" %in% names(reg.data)){
            rtn <- reg.data[["Content Type"]]
        } else if("PerceivedType" %in% names(reg.data)){
            rtn <- paste0(reg.data[["PerceivedType"]], "/", gsub("^\\.", "", ext))
        } else {
            rtn <- "application/octet-stream"
        }
        attributes(rtn) <- reg.data
        return(rtn)
    }

#' MIME Type
#' 
#' Infer the mime type of a file, if on windows will attempt to 
#' read the mime type info from the registry.
#' If a matching type cannot be found the generic
#' "application/octet-stream" will be returned.
#' 
#' @param file file to get information for.
#' 
#' @export
mimetype <- 
    function(file){
        #! Find the file's mime type.
        rtn <- if((mt <- Sys.which("mimetype"))!=""){
            system2(mt, file, TRUE)
        } else if(.Platform$OS.type=="windows"){
            mimetype_windows(file)
        } else  "application/octet-stream"
        return(structure(rtn, class=c("mimetype", "character")))
    }
