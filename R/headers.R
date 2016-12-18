
## tothink: use wisker for templating https://github.com/edwindj/whisker ?

C_INCLUDES <- list(string = "#include <string>\n#include <cereal/types/string.hpp>",
                   unordered_map = "#include <unordered_map>\n#include <cereal/types/unordered_map.hpp>",
                   map = "#include <map>#include <cereal/types/unordered_map.hpp>")


jamr_otype <- function(ctype) {
    sprintf("vector<%s>", ctype)
}

jamr_ctypedef <- function(ctype) {
    switch(ctype,
           ubyte = "typedef unsigned char ubyte;",
           uint = "typedef unsigned int uint;",
           ushort = "typedef unsigned short ushort;",
           ulong = "typedef unsigned long ulong;",
           "")
}

jamr_includes <- function(ctypes){
    out <- unlist(C_INCLUDES[ctypes])
    out <- out[!sapply(out, is.null)]
    paste(out, collapse = "\n")
}

jamr_header <- function(obj, type = NULL){
    if (is.atomic(obj)) {
        jamr_header.vector(obj, type)
    } else {
        stop(sprintf("Serialization of objects of class '%s' not supported", class(obj)), call. = F)
    }
}

jamr_header.vector <- function(obj, ctype = NULL){
    if (!is.atomic(obj)) {
        stop("Can only jam atomic vectors", call. = FALSE)
    }
    tfile <- "~/dev/jamr/inst/templates/atomic.hpp"
    ## tfile <- system.file("template/atomic.hpp", package = "jamr")
    H <- readChar(tfile, file.info(tfile)$size)
    
    ctypes <- R2C_TYPES[[typeof(obj)]]
    if (is.null(ctypes))
        stop(sprintf("unsuported R objectet of '%s' (suported types: %s)",
                     typeof(obj), paste(names(R2C_TYPES), collapse = ", ")))
    if (is.null(ctype))
        ctype <- ctypes[[1]]
    if (!ctype %in% ctypes)
        stop(sprintf("object of type '%s' cannot be converted to C type '%s' (allowed C types: %s)",
                     typeof(obj), ctype, paste(ctypes, collapse = ", ")))
    H <- gsub("{{{typedefs}}}", jamr_ctypedef(ctype), H, fixed = T)
    H <- gsub("{{{otype}}}", jamr_otype(ctype), H, fixed = T)
    H <- gsub("{{{includes}}}", jamr_includes(ctype), H, fixed = T)
    H <- gsub("{{{.+}}}\n?", "", H, perl = T)
    H
}

jamr_main <- function(obj, file = NULL) {
    if (is.null(file)){
        nm <- deparse(substitute(obj))
        file <- sprintf("./data/%s.jam", nm)
    }
    file <- gsub("[\"']", "", file)
    if (is.atomic(obj)) {
        jamr_main.atomic(obj, file)
    } else {
        stop(sprintf("Serialization of objects of class '%s' not supported", class(obj)), call. = F)
    }
}

jamr_main.atomic <- function(obj, file) {
    ## H <- jamr_header()
    hfile <- sub("\\.[^.]+?$", ".hpp", file)
    tfile <- "~/dev/jamr/inst/templates/atomic_main.cpp"
    ## tfile <- system.file("template/atomic.hpp", package = "jamr")
    M <- readChar(tfile, file.info(tfile)$size)
    M <- sub("{{{archive}}}", file, M, fixed = T)
    M <- sub("{{{include_jamr}}}",  sprintf("#include \"%s\"", hfile), M, fixed = T)
    M
}


## cat(jamr_main(1:5))
## cat(jamr_header("A"))
## cat(jamr_main("A"))
