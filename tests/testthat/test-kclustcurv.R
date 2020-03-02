context("test-kgroups")


library(survival)

data(veteran)

test_that(
  "Output is correct in kroups_surv function",
  {
    #expected <- c("squamous", "smallcell", "adeno", "large" )
    fit <- kclustcurv(y = veteran$time, weights = veteran$status,
                          z = veteran$celltype, k = 2, method = 'survival',
                         algorithm = 'kmeans', seed = 300716)

    actual <- data.frame(level = fit$level, cluster = fit$cluster)
    expect_named(actual, c("level", "cluster"))
    expect_is(actual, "data.frame")
  }
)


