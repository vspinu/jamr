context("cycle")

set.seed(100)

cycle_jam <- function(o1) {
    file <- tempfile()
    c_jam(o1, file)
    o2 <- c_unjam(file)
    unlink(file)
    ## str(o1)
    ## str(o2)
    ## print(attributes(o1))
    ## print(attributes(o2))
    ## print(data.frame(o1, o2))
    expect_equal(o1, o2)
}

test_that("primitive objects are serialized correctly", {
    cycle_jam(20:400)
    cycle_jam(runif(20))
    cycle_jam(LETTERS)
    cycle_jam(sample(c(T, F), 20, T))
    cycle_jam(sample(c(T, F), 21, T))
})

test_that("factors are serialized correctly", {
    cycle_jam(as.factor(-10:30))
    cycle_jam(as.ordered(runif(20)))
    cycle_jam(as.factor(LETTERS))
})

test_that("lists are serialized correctly", {
    cycle_jam(list(1, 2, 3, 4))
    cycle_jam(list(1L, 2L, 3L))
    cycle_jam(list("1L", "2L", "3L"))
    cycle_jam(list(a = 1, b = 2, c = 3, d = 4))
    cycle_jam(list("a:s233" = 1, "b:4$%^" = 2, c = 3, d = 4))
    cycle_jam(list(a = list(1, 2, 1:3), b = 2, c = 3, d = 4))
    cycle_jam(list(a = list(1, c = "sfdf", 1:3), b = 2, c = 3, d = 4))
    cycle_jam(list(a = list(list(1, 2), c = "sfdf", 1:3), b = 2, c = 3, d = 4))
})

test_that("objects with attributes are serialized correctly", {
    ##
    ## integer
    tt <- 20:400
    attr(tt, "a") <- 1:20
    attr(tt, "b") <- LETTERS
    attr(tt, "c") <- list(a = 1, b = 20, c = list(1, 1:20))
    cycle_jam(tt)
    ## double
    tt <- as.numeric(20:400)
    attr(tt, "a") <- 1:20
    attr(tt, "b") <- LETTERS
    attr(tt, "c") <- list(a = 1, b = 20, c = list(1, 1:20))
    cycle_jam(tt)
    ## double
    tt <- sample(c(T, F), 20, T)
    attr(tt, "a") <- 1:20
    attr(tt, "b") <- LETTERS
    attr(tt, "c") <- list(a = 1, b = 20, c = list(1, 1:20))
    cycle_jam(tt)
    ##
    ## character
    tt <- as.character(20:400)
    attr(tt, "a") <- 1:20
    attr(tt, "b") <- LETTERS
    attr(tt, "c") <- list(a = 1, b = 20, c = list(1, 1:20))
    cycle_jam(tt)
    ## 
    ## list
    tt <- as.list(20:30)
    attr(tt, "a") <- 1:20
    attr(tt, "b") <- LETTERS
    attr(tt, "c") <- list(a = 1, b = 20, c = list(1, 1:20))
    cycle_jam(tt)
})

test_that("data.frames are serialized correctly", {
    cycle_jam(iris)
    cycle_jam(mtcars)
    attr(iris, "a") <- 1:10
    attr(iris, "b") <- LETTERS
    attr(iris, "c") <- list(a = 1, b = list(1, 2, 1:20))
    cycle_jam(iris)
})
