# clustcurv: Determining Groups in Multiple Curves

[![DOI](https://zenodo.org/badge/98645393.svg)](https://doi.org/10.5281/zenodo.13889531)
[![Build Status](https://travis-ci.org/noramvillanueva/clustcurv.svg?branch=master)](https://travis-ci.org/noramvillanueva/clustcurv)
[![Coverage status](https://codecov.io/gh/noramvillanueva/clustcurv/branch/master/graph/badge.svg)](https://codecov.io/github/noramvillanueva/clustcurv?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/clustcurv)](https://cran.r-project.org/package=clustcurv)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/clustcurv)](https://cran.r-project.org/package=clustcurv)



```clustcurv``` is an R package that provides a method for determining groups in multiple 
curves with an automatic selection of their number based on k-means or
k-medians algorithms. The selection of the optimal number is provided by
bootstrap methods. The methodology can be applied both in regression and survival framework.



## Installation
```clustcurv``` is available through both [CRAN](https://cran.r-project.org/) and GitHub.

Get the released version from CRAN:
```
install.packages("clustcurv")
```

Or the development version from GitHub:
```
# install.packages("devtools")
devtools::install_github("noramvillanueva/clustcurv")
```

