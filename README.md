# clustcurv: Determining Groups in Multiple Curves

[![DOI](https://zenodo.org/badge/98645393.svg)](https://doi.org/10.5281/zenodo.13889531)
[![R-CMD-check](https://github.com/noramvillanueva/clustcurv/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/noramvillanueva/clustcurv/actions/workflows/R-CMD-check.yml)
[![test-coverage](https://github.com/noramvillanueva/clustcurv/actions/workflows/test-coverage.yml/badge.svg)](https://github.com/noramvillanueva/clustcurv/actions/workflows/test-coverage.yml)
[![Coverage status](https://codecov.io/gh/noramvillanueva/clustcurv/branch/master/graph/badge.svg)](https://codecov.io/github/noramvillanueva/clustcurv?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/clustcurv)](https://cran.r-project.org/package=clustcurv)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/clustcurv)](https://cran.r-project.org/package=clustcurv)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/last-month/clustcurv?color=ff69b4)](https://cran.r-project.org/package=clustcurv)



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




## Citation

If you use `clustcurv` in your research, please cite the following paper:


> Villanueva, N.M., Sestelo, M., Meira-Machado, L., Roca-Pardiñas, J. (2021).
> *clustcurv: An R Package for Determining Groups in Multiple Curves*. 
> The R Journal, 13(1), 164–183. 
> <https://doi.org/10.32614/RJ-2021-032>


```         
  @Article{,
    title = {{clustcurv}: An {R} Package for Determining Groups in
      Multiple Curves},
    author = {Nora M. Villanueva and Marta Sestelo and Luis
      Meira-Machado and Javier Roca-Pardi{\~n}as},
    journal = {The R Journal},
    year = {2021},
    volume = {13},
    number = {1},
    pages = {164--183},
    doi = {10.32614/RJ-2021-032},
  }


## Acknowledgment

Marta Sestelo and Nora M. Villanueva acknowledges financial support from
Grant PID2020-118801GB-I00 and Grant PID2023-148811NB-I00 funded by Ministerio de Ciencia e Innovación
(MCIN/ AEI /10.13039/501100011033).





