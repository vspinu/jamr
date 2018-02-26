
NOTE: this is an experimental, yet unfinished library

High performance serialization library for R objects. It's 3-4x faster than
`saveRDS(..., compress=FALSE)` on standard 550MB SSDs.

Two serialization routines are provided:

 - `jam`: Fast serialization of R objects into binary archives. Currently
    supported types are: atomic vectors and arrays (except raw and complex),
    arbitrarily nested lists and data frames with primitive and list
    columns. Attributes are also serialized as long as long as they are of
    supported types. Attributes of non supported types are silently dropped.
    
 - `jar`: This format is intended to be accessed from standalone C++ code for
    row by row processing without loading the full data into memory. Only
    \code{data.frames} are currently supported. It currently uses [cerial][] for
    some parts but this dependency might be eventually dropped.
    
    
[cerial]: https://github.com/wjwwood/serial

