context("cycle")

library(datasets)
set.seed(100)

cycle_jam <- function(o1) {
    file <- tempfile()
    jam(o1, file)
    o2 <- unjam(file)
    unlink(file)
    ## str(o1)
    ## str(o2)
    ## print(attributes(o1))
    ## print(attributes(o2))
    ## print(data.frame(o1, o2))
    expect_equal(o1, o2)
}

cycle_jar <- function(o1) {
    file <- tempfile()
    jar(o1, file)
    o2 <- unjar(file)
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
    cycle_jam(state.name)
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

    cycle_jam(Harman23.cor)
    cycle_jam(ability.cov)
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

test_that("mattrices are jammed correctly", {
    cycle_jam(USPersonalExpenditure)
    cycle_jam(WorldPhones)
    cycle_jam(uspop)
    cycle_jam(Titanic)
    cycle_jam(Seatbelts)
    cycle_jam(EuStockMarkets)
})


test_that("data.frames are jammed correctly", {
    cycle_jam(mtcars)
    attr(iris, "a") <- 1:10
    attr(iris, "b") <- LETTERS
    attr(iris, "c") <- list(a = 1, b = list(1, 2, 1:20))
    cycle_jam(iris)
    cycle_jam(swiss)
    cycle_jam(npk)
})


test_that("data.frames are jammed correctly", {
    cycle_jar(mtcars)
    attr(iris, "a") <- 1:10
    attr(iris, "b") <- LETTERS
    attr(iris, "c") <- list(a = 1, b = list(1, 2, 1:20))
    cycle_jar(iris)
    cycle_jar(swiss)
    cycle_jar(npk)
})

test_that("jar append works as expected", {
    file <- tempfile()
    jar(iris, file)
    jar(iris, file, append = T)
    iris2 <- unjar(file)
    expect_identical(rbind(iris, iris), iris2)
    jar(iris, file, append = T)
    iris3 <- unjar(file)
    expect_identical(rbind(iris, iris, iris), iris3)
})

## test_that("data.frames are jarred correctly", {
##     jar(iris, "./tmp/iris.jar")
##     unjar("./tmp/iris.jar")
##     cycle_jam(mtcars)
##     attr(iris, "a") <- 1:10
##     attr(iris, "b") <- LETTERS
##     attr(iris, "c") <- list(a = 1, b = list(1, 2, 1:20))
##     cycle_jam(iris)
## })
