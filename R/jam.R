#' @useDynLib jam
#' @importFrom Rcpp sourceCpp
NULL


##' Serialize R objects to files
##'
##' Fast serialization of R objects into binary archives. All types of atomic
##' vectors, arbitrarly nested lists and data frames are supported. Atributes of
##' supported data types are also saved except of the row.names. Non suported
##' attribute types are silently dropped.
##' 
##' @param obj atomic vector or list, with or without attributes
##' @param file archive file name. Defaults to "./data/[obj_name].jam"
##' @export 
##' @author Vitalie Spinu
##' @examples
##' \dontrun{
##'   jam(iris, "./data/iris.jam")
##'   all.equal(iris, unjam("./data/iris.jam"))
##' }
jam <- function(obj, file = sprintf("./data/%s.jam", deparse(substitute(obj)))){
    if (dir.exists(dirname(file)))
        dir.create(file, showWarnings = FALSE, recursive = TRUE)
    c_jam(obj)
    invisible(ofile)
}

##' @rdname jam
##' @export
unjam <- function(file){

    if (!file.exists(ifile))
        stop(sprintf("Archive file '%s' does not exist.", ifile))

    c_unjam(ifile)
}


## ## first type is the default
## R2C_TYPES <- list(integer = c("int", "uint", "byte", "ubyte", "short", "ushort", "long", "ulong", "bool"),
##                   double = c("double", "float"),
##                   logical = c("bool"),
##                   character = c("string"))

## JAM_TYPES <- list(
##     bool   = 0L,
##     byte   = 1L,
##     ubyte  = 2L,
##     short  = 3L,
##     ushort = 4L,
##     int    = 5L,
##     uint   = 6L,
##     long   = 7L,
##     ulong  = 8L,
##     float  = 9L,
##     double = 10L,
##     utF8   = 11L,
##     bitset = 12L, 
##     undefined = 255L)

