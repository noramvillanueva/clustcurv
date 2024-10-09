#testthat::context("test-kregcurves")


library(survival)

data("barnacle5")

testthat::test_that("Output is correct in kroups_surv function",
  {
    #expected <- c("laxe",  "lens",  "barca", "boy","alba")
    fit <- kregcurves(y = barnacle5$DW, x = barnacle5$RC,
                      z = barnacle5$F, k = 2, algorithm = "kmeans",
                      seed = 300716)

    actual <- data.frame(level = fit$level, cluster = fit$cluster)
    testthat::expect_named(actual, c("level", "cluster"))
   # testthat::expect_type(actual, "data.frame")
  }
)


