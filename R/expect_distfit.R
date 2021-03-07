
####  ####
#' Test a parametric distribution
#'
#' Expectation checking whether a given sample comes from a certain parametric distribution. The underlying procedure is Anderson-Darling test of goodness-of-fit \code{\link[goftest]{ad.test}}.
#' The expectation throws an error when the test's p-value is smaller than the threshold p-value.
#' @return Invisibly returns a p-value of the test.
#' @param sample to test
#' @param p_value threshold p-value of the test
#' @param nulldist null distribution
#' @param ... parameters to pass to the null distribution
#' @examples
#'
#'  # Gaussianity test
#' \dontrun{
#' x<-rnorm(n=1e4,5,6)
#' expect_distfit(sample=x, nulldist="pnorm", mean=5, sd=6.3)
#' expect_distfit(sample=x, nulldist="pnorm", mean=5, sd=6)
#' }
#'
#' # Uniformity test
#' x<-runif(n=1e4,-1,6)
#' expect_distfit(sample=x, nulldist="punif", min=-1, max=6)
#'
#' @export
#' @importFrom goftest ad.test
#' @importFrom rlang enquo
#' @importFrom testthat expect
#' @importFrom testthat quasi_label
#' @importFrom stats shapiro.test
#' @importFrom stats t.test
expect_distfit <- function(sample, p_value=1e-3, nulldist, ...)
{
  # 0. Perform the test
  tst<-goftest::ad.test(sample, null = nulldist, estimated=FALSE, ...)
  object<-tst$p.value
  expected<-p_value

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object),
                               label = as.character(signif(object,3)), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  # 2. Call expect()
  testthat::expect(
                   act$val >= exp$val,
                   sprintf("Test failed. The test's p-value approx. %s is smaller than the treshold %s", act$lab, exp$lab)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}


####  ####
#' Test a Gaussian distribution
#'
#' Expectation checking whether a given sample comes from Gaussian distribution with arbitrary parameters. The underlying procedure is Shapiro- Wilk's test of normality \code{\link[stats]{shapiro.test}}.
#' The expectation throws an error when the test's p-value is smaller than the threshold p-value.
#' @return Invisibly returns a p-value of the test.
#' @inheritParams expect_distfit
#' @details shapiro.test allows the number of non-missing values to be between 3 and 5000.
#' @examples
#'
#' x<-rnorm(n=1e3,5,6)
#' expect_gaussian(sample=x)
#'
#' #The following test doesn't pass
#' \dontrun{
#' x<-runif(n=1e2,-1,6)
#' expect_gaussian(sample=x)
#' }
#' @export
#'
expect_gaussian <- function(sample, p_value=1e-3)
{
  # 0. Perform the test
  tst<-stats::shapiro.test(sample)
  object<-tst$p.value
  expected<-p_value

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object),
                               label = as.character(signif(object,3)), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  # 2. Call expect()
  testthat::expect(
                   act$val >= exp$val,
                   sprintf("Test failed. The test's p-value approx. %s is smaller than the treshold %s", act$lab, exp$lab)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}



####  ####
#' Test a mean-value using t-test
#'
#' Expectation checking whether values from a given sample have a certain mean or that two samples have the same mean. The underlying procedure is Student's t-test \code{\link[stats]{t.test}}.
#' The expectation throws an error when the test's p-value is smaller than the threshold p-value.
#' @return Invisibly returns a p-value of the test
#' @param ... parameters to pass to t.test function including data sample(s)
#' @inheritParams expect_distfit
#' @examples
#' # This test doesn't pass
#' \dontrun{
#' x<-1:1e3
#' expect_mean_equal(x=x)
#' }
#'
#' # This one passes, but shouldn't
#' x<-rnorm(1e3) + 0.01
#' expect_mean_equal(x=x)
#'
#' x<-rnorm(1e3)
#' expect_mean_equal(x=x)
#'
#' # check if 2 samples have the same mean
#' x<-rnorm(1e3, mean=10)
#' y<-rnorm(1e3, mean=10)
#' expect_mean_equal(x=x, y=y)
#' @export
#'
expect_mean_equal <- function(p_value=1e-3, ...)
{
  # 0. Perform the test
  tst <- stats::t.test(...)

  object<-tst$p.value
  expected<-p_value

  # 1. Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object),
                               label = as.character(signif(object,3)), arg = "object")
  exp <- testthat::quasi_label(rlang::enquo(expected), arg = "expected")

  # 2. Call expect()
  testthat::expect(
                   act$val >= exp$val,
                   sprintf("Test failed. The test's p-value approx. %s is smaller than the treshold %s", act$lab, exp$lab)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

