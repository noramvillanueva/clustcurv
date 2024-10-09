#testthat::context("test-regclustcurves")


testthat::test_that("Regression fit computed correctly", {
  expected <- c(2,1,1,1,2)
  fit <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                       algorithm = "kmeans", nboot = 5, seed = 300716)
  testthat::expect_equal(fit$cluster, expected)
  testthat::expect_equal(fit$num_groups, max(expected))
  testthat::expect_s3_class(fit, "clustcurves")
  testthat::expect_match(mode(fit), "list")
})



testthat::test_that("It throw an error if seed argument is not an object of type numeric", {
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                              algorithm = "kmeans", nboot = 5, seed = "300716"))
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                              algorithm = "kmeans", nboot = 5, seed = factor(300716)))
})



testthat::test_that("It throw an error if algorithm is different from 'kmeans' or 'kmedians'", {
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, algorithm = kmedia))
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, algorithm = list(kmeans)))
})


testthat::test_that("It throw an error if multiple is not an object of type logical", {
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             algorithm = "kmeans", multiple = c(true)))
})


testthat::test_that("It throw an error if multiple.method is not an object of type string", {
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             algorithm = "kmeans",multiple.method = TRUE))
})



testthat::test_that("It throw an error if kvector is not an object of type numeric.", {
  testthat::expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans",
                             kvector = "3:4"))
})


testthat::test_that( "It throw an error if argument y is missing", {
  testthat::expect_error(autoclustcurv(x = barnacle5$RC, z = barnacle5$F,
                            algorithm = "kmeans", nboot = 5, seed = 300716))
})

testthat::test_that( "It throw an error if argument x is missing", {

  testthat::expect_error(autoclustcurv(y = barnacle5$DW, z = barnacle5$F,
                             algorithm = "kmeans", nboot = 5, seed = 300716))
})
