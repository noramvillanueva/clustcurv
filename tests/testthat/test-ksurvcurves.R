context("test-ksurvcurves")


library(survival)

data(veteran)

test_that(
  "Output is correct in kroups_surv function",
  {
    #expected <- c("squamous", "smallcell", "adeno", "large" )
    fit <- ksurvcurves(time = veteran$time, status = veteran$status,
                          x = veteran$celltype, k = 2,
                         algorithm = 'kmeans', seed = 300716)

    actual <- data.frame(level = fit$level, cluster = fit$cluster)
    expect_named(actual, c("level", "cluster"))
    expect_is(actual, "data.frame")
  }
)


