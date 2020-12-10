context("Output converter in Estim_diagnost")

test_that("Output converter handles numeric vectors", {

  LL <- as.vector(c(1,2,3,4))
  names(LL) <- c('a','b','c','d')
  res <- estim_output_converter(LL)

  expect_is(res, class="data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 4)
  expect_equal(colnames(res), c('a','b','c','d'))
})

test_that("Output converter handles lists", {

  LL <- list("a"=1, 'b'=2, "c"=3, "d"=4)
  res <- estim_output_converter(LL)

  expect_is(res, class="data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 4)
  expect_equal(colnames(res), c('a','b','c','d'))
})

test_that("Output converter handles 1x4 data frames", {

  LL <- data.frame("a"=1, 'b'=2, "c"=3, "d"=4)
  res <- estim_output_converter(LL)

  expect_is(res, class="data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(ncol(res), 4)
  expect_equal(colnames(res), c('a','b','c','d'))
})

test_that("Output converter handles 4x1 data frames", {

  LL <- t(data.frame("a"=1, 'b'=2, "c"=3, "d"=4))
  LL <- as.data.frame(LL)
  res <- estim_output_converter(LL)

  expect_is(res, class="data.frame")
  expect_equal(nrow(res), 4)
  expect_equal(ncol(res), 1)
  expect_equal(rownames(res), c('a','b','c','d'))
})

