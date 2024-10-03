#library(testthat)
library(clustcurv)
if (requireNamespace("testhat", quietly = TRUE)) {
testthat::test_check("clustcurv")
}else{
  #nothing
}
