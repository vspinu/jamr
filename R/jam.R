
##' Serialize R objects into binary files.
##'
##' Fast serialization of R objects into binary archives. Currently supported
##' types are: atomic vectors and arrays (except raw and complex), arbitrarily
##' nested lists and data frames with primitive and list columns. Attributes are
##' also serialized as long as long as they are of supported types. Attributes
##' of non supported types are silently dropped.
##' 
##' @param obj atomic vector or list, with or without attributes
##' @param file archive file name. Defaults to "./data/[obj_name].rjam"
##' @export 
##' @return \code{unjam} returns de-serialized object; \code{jam} returns input
##'     object invisibly.
##' @author Vitalie Spinu
##' @examples
##' \dontrun{
##'   jam(iris, "./data/iris.rjam")
##'   all.equal(iris, unjam("./data/iris.rjam"))
##' }
jam <- function(obj, file = sprintf("./data/%s.rjam", deparse(substitute(obj)))){
    file <- normalizePath(file)
    dir <- dirname(file)
    if (dir.exists(dir))
        dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    c_jam(obj, file)
    invisible(obj)
}

##' @rdname jam
##' @export
unjam <- function(file){
    file <- normalizePath(file)
    if (!file.exists(file))
        stop(sprintf("Archive file '%s' does not exist.", file))

    c_unjam(file)
}
