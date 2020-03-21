context("test-autoclustcurv")

library(survival)

data(veteran)
data(barnacle5)

test_that( "Survival fit computed correctly", {
    expected <- c(2,1,1,2)
    fit <- autoclustcurv(y = veteran$time, weights = veteran$status,
                            z = veteran$celltype, method = 'survival',
                            algorithm = 'kmeans', seed = 300716, nboot = 5)
    expect_equal(fit$cluster, expected)
    expect_equal(fit$num_groups, max(expected))
    expect_s3_class(fit, "clustcurv")
    expect_match(mode(fit), "list")
  }
)


test_that("Regression fit computed correctly", {
  expected <- c(2,1,1,1,2)
  fit <- autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                       method = 'regression', algorithm = "kmeans", nboot = 5,
                       seed = 300716)
  expect_equal(fit$cluster, expected)
  expect_success(expect_equal(fit$num_groups, max(expected)))
  expect_s3_class(fit, "clustcurv")
  expect_match(mode(fit), "list")
})




test_that("It throw an error if seed argument is not an object of type numeric", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'kmeans', seed = "300716"))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'kmeans', seed = factor(300716)))
})


test_that("It throw an error if method is different from 'regression' and 'survival'", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'surv',
                             algorithm = 'kmeans', seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = NULL,
                             algorithm = 'kmeans', seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype,
                             algorithm = 'kmeans', seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = survival,
                             algorithm = 'kmeans', seed = 300716, nboot = 10))

  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                           nboot = 10, method = 'Reg', seed = 300716,
                           algorithm = 'kmeans', cluster = TRUE))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, seed = 300716,
                             algorithm = 'kmeans', cluster = TRUE))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, method = NULL, seed = 300716,
                             algorithm = 'kmeans', cluster = TRUE))
  })



test_that("It throw an error if algorithm is different from 'kmeans' or 'kmedians'", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'k-means', seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = kmeans, seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                           z = veteran$celltype, method = 'survival',
                           algorithm = c(k-means), seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = "kmedianas", seed = 300716, nboot = 10))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, method = 'regression', algorithm = kmedia))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             nboot = 10, method = 'regression', algorithm = list(kmeans)))
})


test_that("It throw an error if multiple is not an object of type logical", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'kmeans', multiple = "true"))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans", multiple = c(true)))
})


test_that("It throw an error if multiple.method is not an object of type string", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'kmeans', multiple.method = bonferroni))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans",
                             multiple.method = TRUE))
})



test_that("It throw an error if kvector is not an object of type numeric.", {
  expect_error(autoclustcurv(y = veteran$time, weights = veteran$status,
                             z = veteran$celltype, method = 'survival',
                             algorithm = 'kmeans',kvector = list(1,3)))
  expect_error(autoclustcurv(y = barnacle5$DW, x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans",
                             kvector = "3:4"))
})


test_that( "It throw an error if argument weights is missing for method = 'survival'", {
  expect_error(autoclustcurv(y = veteran$time, z = veteran$celltype,
                             method = 'survival',algorithm = 'kmeans'))
})

test_that( "Argument weights must be a vector of binary numbers", {
  expect_error(autoclustcurv(y = veteran$time, z = veteran$celltype,
                             weights = rbinom(length(veteran$status),4,0.5),
                             method = 'survival',algorithm = 'kmeans'))
})


test_that( "It throw an error if argument y is missing", {
  expect_error(autoclustcurv(z = veteran$celltype,
                             weights = veteran$status, nboot=5,
                             method = 'survival',algorithm = 'kmeans'))
  expect_error(autoclustcurv(x = barnacle5$RC, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans", nboot = 5,
                             seed = 300716))
})

test_that( "It throw an error if argument y is missing", {

  expect_error(autoclustcurv(y = barnacle5$DW, z = barnacle5$F,
                             method = 'regression', algorithm = "kmeans", nboot = 5,
                             seed = 300716))
})
