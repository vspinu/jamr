
##' Serialize data.frames by rows.
##'
##' This format is intended to be accessed from standalone C++ for row by row
##' processing without loading the full data into memory. Only
##' \code{data.frames} are currently supported.
##'
##' Because columns are not stored in contiguous blocks the speed of \code{jar}
##' serialization is considerably lower than that of \code{\link{jam}}, but
##' still comparable with the speed of \code{writeRDS(..., compress=F)}.
##'
##' List columns are not currently supported.
##' 
##' The term \code{jar} has nothing to do with java archives, it's about
##' "JAmming by Rows" and about those cylindrical objects where you store your
##' jam in.
##' 
##' @param obj Atomic vector or list, with or without attributes
##' @param file Archive file name.
##' @param append If \code{TRUE} the data is appended to existing file resulting
##'     in multi-chunk archive. No attributes are stored for appended chunks
##'     which could result in wrong or even corrupted de-serialization of factor
##'     levels. See also \code{\link{jar_csv}} for a common use case.
##' @param rows_per_chunk If too small, serialization or deserialization will be
##'     slower but can result in smaller archive sizes because type-size
##'     optimization is performed on smaller chunks. Default is to write
##'     everything in one chunk.
##' @export
##' @return \code{unjar} returns de-serialized \code{data.frame}; \code{jar}
##'     returns input object invisibly.
##' @author Vitalie Spinu
##' @examples
##' \dontrun{
##'   jar(iris)
##'   all.equal(iris, unjar("./data/iris.rjar"))
##' }
jar <- function(obj, file, append = FALSE, rows_per_chunk = -1) {
    if (!inherits(obj, "data.frame"))
        stop("Only data.frames are supported; try 'jam'.")
    file <- normalizePath(file)
    dir <- dirname(file)
    if (dir.exists(dir))
        dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    c_jar(obj, file, append, rows_per_chunk)
    invisible(obj)
}

##' @rdname jar
##' @export
unjar <- function(file, chunks = 0, bind = TRUE){
    file <- normalizePath(file)
    if (!file.exists(file))
        stop(sprintf("Archive file '%s' does not exist.", file))
    if (bind)
        c_unjar_bind(file, chunks)
    else
        c_unjar_nobind(file, chunks)
}

.check_df_struct <- function(df_ref, df) {
    if (!inherits(df, "data.frame"))
        stop("Result must be a data.frame.")
    if (!identical(ncol(df_ref), ncol(df)))
        stop("Number of columns in this chunk is not the same as in reference chunk.")
    ref_types <- sapply(df_ref, typeof)
    types <- sapply(df, typeof)
    if (!identical(ref_types, types))
        stop("Classes of this chunk's columns are not the same as of reference columns.",
             sprintf("(%s) vs. (%s)", paste(types, collapse = ", "), paste(ref_types, collapse = ", ")))
}

## This is awkward but it's what we have to do to please readr :(
.readr_callback <- function(callback = NULL, out_file){
    if (is.null(callback))
        callback <-  function(chunk, index) chunk

    if (!requireNamespace("readr", quietly = T))
        stop("Package 'readr' is required for this functionality.", call. = F)
    if (!is.function(callback))
        stop("Callback must be a function of two parameters.")
    ## readr imports R6, so it must be there
    loadNamespace("R6")

    cbgen <-
        R6::R6Class("JarCallback", inherit = readr::SideEffectChunkCallback,
                    private = list(
                        first_chunk = NULL
                    ),
                    public = list(
                        receive = function(data, index) {
                            chunk <- private$callback(data, index)
                            if (identical(chunk, FALSE)) {
                                private$cancel <- TRUE
                            } else {
                                result <- private$callback(data, index)
                                if (is.null(private$first_chunk)) {
                                    private$first_chunk <- result
                                    jar(result, out_file)
                                } else {
                                    .check_df_struct(private$first_chunk, result)
                                    jar(result, out_file, append = T)
                                }
                            }
                        })
                    )
    cbgen$new(callback)
}

##' Serialize csv, tsv and other delimited files. 
##'
##' These functions use \code{read_delim_chunked} functionality from
##' \code{readr} package to convert large delimited text files into jar binary
##' without loading it into memory. \cr\cr ‘jar_csv’ and ‘jar_tsv’ are special
##' cases of the general ‘jar_delim’. ‘jar_csv2’ uses ‘;’ for separators,
##' instead of ‘,’.
##'
##' @param in_file Input delimited text file. Can be R connection or an archive
##'     files supported by \code{readr} package.
##' @param out_file Output archive file. By default it is \code{in_file} with
##'     \code{.rjar} extension.
##' @param callback A function that receives two arguments \code{chunk}, a data
##'     frame,  and \code{row}, row index of the curent chunk in the input
##'     file. This function can return logical \code{FALSE} to indicate that
##'     processing should stop. If not provided, \code{callback} is an identity
##'     function.
##' @param chunk_size The number of rows to process in each chunk.
##' @param delim Single character used to separate fields within a record
##' @param ... Other arguments passed directly to \code{read_delim}.
##' @export
jar_delim <- function(in_file, out_file = paste0(in_file, ".rjar"),
                      callback = NULL, chunk_size = 1e6, delim, ...) {
    r6cb <- .readr_callback(callback, out_file)
    readr::read_delim_chunked(file = in_file, callback = r6cb,
                              chunk_size = chunk_size, delim = delim, ...)
}

##' @rdname jar_delim
##' @export
jar_csv <- function(in_file, out_file = paste0(in_file, ".rjar"), 
                    callback = NULL, chunk_size = 1e6, ...) {
    r6cb <- .readr_callback(callback, out_file)
    readr::read_csv_chunked(file = in_file, callback = r6cb,
                            chunk_size = chunk_size, ...)
}

##' @rdname jar_delim
##' @export
jar_csv2 <- function(in_file, out_file = paste0(in_file, ".rjar"), 
                     callback = NULL, chunk_size = 1e6, ...) {
    r6cb <- .readr_callback(callback, out_file)
    readr::read_csv2_chunked(file = in_file, callback = r6cb,
                             chunk_size = chunk_size, ...)
}

##' @rdname jar_delim
##' @export
jar_tsv <- function(in_file, out_file = paste0(in_file, ".rjar"), 
                    callback = NULL, chunk_size = 1e6, ...) {
    r6cb <- .readr_callback(callback, out_file)
    readr::read_tsv_chunked(file = in_file, callback = r6cb,
                            chunk_size = chunk_size, ...)
}


