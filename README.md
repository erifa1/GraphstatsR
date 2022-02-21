
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GraphstatsR

<!-- badges: start -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->
<!-- badges: end -->

A shiny app allowing users to generate advanced interactive graphics and statistical tests. 


## Prerequisites


R3.6.3 or upper is required (https://pbil.univ-lyon1.fr/CRAN/bin/)


* Linux

```bash
sudo apt-get install r-base
sudo apt-get install git
```

* Windows

[Rtools](https://cran.r-project.org/bin/windows/Rtools/) and [git](https://git-scm.com/download/win) are required.


## Installation

* In R console: 
You can install the released version of graphstats from [this
repository](https://forgemia.inra.fr/etienne.rifa/graphstats) with:

``` r
install.packages("remotes")
remotes::install_gitlab(repo = "etienne.rifa/graphstats", host = "forgemia.inra.fr")
```

## To run Shiny app in R

``` r
# To update app 
remotes::install_gitlab(repo = "etienne.rifa/graphstats", host = "forgemia.inra.fr")

# To run app
library(graphstatsr)
graphstatsr::run_app()
```
