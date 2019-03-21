# clustcurv: Determining Groups in Multiple Curves


[![Build Status](https://travis-ci.org/noramvillanueva/clustcurv.svg?branch=master)](https://travis-ci.org/noramvillanueva/clustcurv)
[![Coverage status](https://codecov.io/gh/noramvillanueva/clustcurv/branch/master/graph/badge.svg)](https://codecov.io/github/noramvillanueva/clustcurv?branch=master)




```clustcurv``` is an R package that provides a method for determining groups in multiple survival 
curves with an automatic selection of their number based on k-means or
k-medians algorithms. The selection of the optimal number is provided by
bootstrap methods.



## Installation
```clustcurv``` is available through both CRAN and GitHub.

Get the released version from CRAN:
```
install.packages("clustcurv")
```

Or the development version from GitHub:
```
# install.packages("devtools")
devtools::install_github("noramvillanueva/clustcurv")
```

