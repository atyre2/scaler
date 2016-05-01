context("Centering")

testdf <- data.frame(x = rnorm(100),
                     y = rnorm(100),
                     z = factor(sample(letters, 100, replace = TRUE)))

testdf2 <- data.frame(x = 1:5,
                      y = letters[1:5],
                      z = 6:10,
                      a = factor(letters[6:10]))

c_testdf <- center_df(testdf)
c_testdf2 <- center_df(testdf2)

test_that("means are zero",{
  expect_equal(sapply(c_testdf, mean), c(x = 0, y = 0, z = NA))
  expect_equal(sapply(c_testdf2, mean), c(x = 0, y = NA, z = 0, a = NA))
})

test_that("structure maintained",{
  expect_equal(ncol(c_testdf), ncol(testdf))
  expect_equal(nrow(c_testdf), nrow(testdf))
  expect_match(class(c_testdf), "data.frame")
  expect_true(!is.null(attr(c_testdf, "centers")))
  expect_equal(ncol(c_testdf2), ncol(testdf2))
  expect_equal(nrow(c_testdf2), nrow(testdf2))
  expect_match(class(c_testdf2), "data.frame")
  expect_true(!is.null(attr(c_testdf2, "centers")))
})

test_that("rejects false inputs", {
  expect_error(center_df(numeric(10)))
  expect_error(center_df(testdf, by = 2))
})
