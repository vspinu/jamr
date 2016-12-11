
## first type is the default
R2C_TYPES <- list(integer = c("int", "uint", "byte", "ubyte", "short", "ushort", "long", "ulong", "bool"),
                  double = c("double", "float"),
                  logical = c("bool"),
                  character = c("string"))

JAM_TYPES <- list(
    bool   = 0L,
    byte   = 1L,
    ubyte  = 2L,
    short  = 3L,
    ushort = 4L,
    int    = 5L,
    uint   = 6L,
    long   = 7L,
    ulong  = 8L,
    float  = 9L,
    double = 10L,
    utF8   = 11L,
    bitset = 12L, 
    undefined = 255L)

jam <- function(obj,
                ofile = sprintf("./data/%s.jam", deparse(substitute(obj))), 
                otype = NULL){

    if (!is.atomic(obj))
        stop("`obj` must be a data frame or an atomic vector", call. = FALSE)

    if (is.null(otype))
        otype <- R2C_TYPES[[typeof(obj)]][[1]]

    if (is.null(otype))
        stop(sprintf("Cannot jam object of type '%s'", typeof(obj)))

    dir <- dirname(ofile)
    if (!dir.exists(dir))
        dir.create(dir, recursive = T)

    c_jam_vector(obj, JAM_TYPES[[otype]], ofile)

    invisible(ofile)
}

unjam <- function(ifile){

    if (!file.exists(ifile))
        stop(sprintf("Archive file '%s' does not exist.", ifile))

    c_unjam_vector(ifile)
}

int <- 1:500
int[30:100] <- NA
unjam(jam(int))

unjam(jam(as.numeric(int)))

