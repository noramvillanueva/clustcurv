context("clustcurv_surv")

library(survival)

data(veteran)

test_that(
  "Total number of groups are OK",
  {
    expected <- c(2,1,1,2)
    actual <- clustcurv_surv(time = veteran$time, status = veteran$status,
                  fac = veteran$celltype, algorithm = "kmeans", seed = 300716)
    expect_equal(actual$cluster, expected)
  }
)
