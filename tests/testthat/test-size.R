context("size")
set.seed(100)

size_jam <- function(obj, bytes) {
    file <- tempfile()
    on.exit(unlink(file))
    c_jam(obj, file)
    size <- file.size(file)
    print(size)
    expect_equal(bytes, size)
}

expect_that("Integer vectors are saved with minimal size", {
    size_jam(0:127, 128 + 12)
    size_jam(0:129, 130 + 12)
    size_jam(0:254, 255 + 12) # byte storage
    size_jam(0:255, 256*2 + 12) # short storage
    size_jam(0:(2^16 - 2), (2^16 - 1)*2 + 12) # short storage
    size_jam(0:(2^16 - 1), 2^16*4 + 12) # int storage
    size_jam(-10:126, 137 + 12)
    size_jam(-10:129, 140*2 + 12)
    size_jam(-10:(2^15 - 2), (2^15 - 2 + 11)*2 + 12) # short storage
    size_jam(-10:(2^15 - 1), (2^15 - 1 + 11)*4 + 12) # int storage
})

expect_that("Strings are saved with minimal nchar length", {
    size_jam(LETTERS, 4*2 + 8*2 + length(LETTERS)*2)
})
