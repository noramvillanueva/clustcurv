#testthat::context("test-survclustcurves")

library(survival)


testthat::test_that( "Survival fit computed correctly", {
    expected <- c(2,1,1,2)
    fit <- survclustcurves(time = veteran$time, status = veteran$status,
                            x = veteran$celltype, algorithm = 'kmeans',
                           seed = 300716, nboot = 5)
    testthat::expect_equal(fit$cluster, expected)
    testthat::expect_equal(fit$num_groups, max(expected))
    testthat::expect_s3_class(fit, "clustcurves")
    testthat::expect_match(mode(fit), "list")
  }
)

testthat::test_that("It throw an error if seed argument is not an object of type numeric", {
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans',
                               seed = "300716", nboot = 5))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans'
                               , seed = factor(300716), nboot = 5))
})


testthat::test_that("It throw an error if algorithm is different from 'kmeans' or 'kmedians'", {
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'k-means',
                             seed = 300716, nboot = 10))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = kmeans,
                               seed = 300716, nboot = 10))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = c(k-means),
                               seed = 300716, nboot = 10))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = "kmedianas",
                               seed = 300716, nboot = 10))
})


testthat::test_that("It throw an error if multiple is not an object of type logical", {
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'kmeans',
                             multiple = "true"))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = "kmeans",
                             multiple = c(true)))
})


testthat::test_that("It throw an error if multiple.method is not an object of type string", {
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans',
                               multiple.method = bonferroni))
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = "kmeans",
                               multiple.method = TRUE))
})



testthat::test_that("It throw an error if kvector is not an object of type numeric.", {
  testthat::expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'kmeans',
                             kvector = list(1,3)))
})


testthat::test_that( "It throw an error if argument status is missing for survival", {
  testthat::expect_error(survclustcurves(time = veteran$time, x = veteran$celltype,
                             algorithm = 'kmeans'))
})

testthat::test_that( "Argument status must be a vector of binary numbers", {
  testthat::expect_error(survclustcurves(time = veteran$time, x = veteran$celltype,
                             status = rbinom(length(veteran$status),4,0.5),
                             algorithm = 'kmeans'))
})


testthat::test_that( "It throw an error if argument time is missing", {
  testthat::expect_error(survclustcurves(x = veteran$celltype, status = veteran$status,
                               nboot=5, algorithm = 'kmeans'))
})
