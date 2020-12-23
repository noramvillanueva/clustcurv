context("test-survclustcurves")

library(survival)

data(veteran)

test_that( "Survival fit computed correctly", {
    expected <- c(2,1,1,2)
    fit <- survclustcurves(time = veteran$time, status = veteran$status,
                            x = veteran$celltype, algorithm = 'kmeans',
                           seed = 300716, nboot = 5)
    expect_equal(fit$cluster, expected)
    expect_equal(fit$num_groups, max(expected))
    expect_s3_class(fit, "clustcurves")
    expect_match(mode(fit), "list")
  }
)

test_that("It throw an error if seed argument is not an object of type numeric", {
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans',
                               seed = "300716", nboot = 5))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans'
                               , seed = factor(300716), nboot = 5))
})


test_that("It throw an error if algorithm is different from 'kmeans' or 'kmedians'", {
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'k-means',
                             seed = 300716, nboot = 10))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = kmeans,
                               seed = 300716, nboot = 10))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = c(k-means),
                               seed = 300716, nboot = 10))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = "kmedianas",
                               seed = 300716, nboot = 10))
})


test_that("It throw an error if multiple is not an object of type logical", {
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'kmeans',
                             multiple = "true"))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = "kmeans",
                             multiple = c(true)))
})


test_that("It throw an error if multiple.method is not an object of type string", {
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = 'kmeans',
                               multiple.method = bonferroni))
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                               x = veteran$celltype, algorithm = "kmeans",
                               multiple.method = TRUE))
})



test_that("It throw an error if kvector is not an object of type numeric.", {
  expect_error(survclustcurves(time = veteran$time, status = veteran$status,
                             x = veteran$celltype, algorithm = 'kmeans',
                             kvector = list(1,3)))
})


test_that( "It throw an error if argument status is missing for survival", {
  expect_error(survclustcurves(time = veteran$time, x = veteran$celltype,
                             algorithm = 'kmeans'))
})

test_that( "Argument status must be a vector of binary numbers", {
  expect_error(survclustcurves(time = veteran$time, x = veteran$celltype,
                             status = rbinom(length(veteran$status),4,0.5),
                             algorithm = 'kmeans'))
})


test_that( "It throw an error if argument time is missing", {
  expect_error(survclustcurves(x = veteran$celltype, status = veteran$status,
                               nboot=5, algorithm = 'kmeans'))
})
