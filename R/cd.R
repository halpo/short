
#' Change Directory
#' 
#' Change the current working directory or get the current working directory
#' will also follow windows link files.
#' 
#' @param new new working directory
#' @return 
#' Always returns the current working directory.
#' @export
cd <- 
function(new=NULL) {
    if(!is.null(new)){
        new <- normalizePath(new, mustWork=TRUE) 
        if(tools::file_ext(basename(new)) == "lnk")
            if(!requireNamespace("wingui"))
                stop("wingui is required to read a windows shortcut.")
            new <- wingui::read_lnk(new)
        if(new == normalizePath(getwd())){
            message("Current working directory is already `", new,"`")
        } else {
            setwd(new)
        }
        return(invisible(getwd()))
    } else 
        return(getwd())
}
if(FALSE){#! development
    setwd("P:\\ORD_GUNDLAPALLI_201007100D\\Stats\\MST-Stats\\MST-Templates Extraction")
    new <- "Emily-MST.lnk"
    mimetype(new)
}
