context("test-regclustcurves")

data(veteran)

test_that("Regression fit computed correctly", {
  expected <- c(2,1,1,1,2)
  fit <- regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                       algorithm = "kmeans", nboot = 5, seed = 300716)
  expect_equal(fit$cluster, expected)
  expect_success(expect_equal(fit$num_groups, max(expected)))
  expect_s3_class(fit, "clustcurves")
  expect_match(mode(fit), "list")
})



test_that("It throw an error if seed argument is not an object of type numeric", {
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                              algorithm = "kmeans", nboot = 5, seed = "300716"))
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                              algorithm = "kmeans", nboot = 5, seed = factor(300716)))
})



test_that("It throw an error if algorithm is different from 'kmeans' or 'kmedians'", {
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, algorithm = kmedia))
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, algorithm = list(kmeans)))
})


test_that("It throw an error if multiple is not an object of type logical", {
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             algorithm = "kmeans", multiple = c(true)))
})


test_that("It throw an error if multiple.method is not an object of type string", {
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             algorithm = "kmeans",multiple.method = TRUE))
})



test_that("It throw an error if kvector is not an object of type numeric.", {
  expect_error(regclustcurves(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans",
                             kvector = "3:4"))
})


test_that( "It throw an error if argument y is missing", {
  expect_error(autoclustcurv(x = barnacle5$RC, z = barnacle5$F,
                            algorithm = "kmeans", nboot = 5, seed = 300716))
})

test_that( "It throw an error if argument x is missing", {

  expect_error(autoclustcurv(y = barnacle5$DW, z = barnacle5$F,
                             algorithm = "kmeans", nboot = 5, seed = 300716))
})
