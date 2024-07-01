
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GraphstatsR

<!-- badges: start -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->
<!-- badges: end -->

A shiny app allowing users to generate advanced interactive graphics and statistical tests. 

Online instance here: https://graphstatsr.sk8.inrae.fr/

## Prerequisites

R4.3.3 or upper is required (https://pbil.univ-lyon1.fr/CRAN/bin/)


* Linux

```bash
sudo apt-get install r-base git libssl-dev cmake libcurl4-openssl-dev libgmp3-dev libmpfr-dev zlib1g-dev
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
remotes::install_gitlab(repo = "etienne.rifa/graphstats", host = "forgemia.inra.fr", upgrade = FALSE)

# To run app
library(graphstatsr)
graphstatsr::run_app()
```

## Toy dataset

Features table and metadata files are available in the `dataset` folder to test Graphstats. (`inst/dataset` in the git repo)

```r
dir( system.file("dataset/", package = "graphstatsr") )
# "features_quanti_data.csv" "metadata_file.csv" # for Easy Stats

# "isoplot_quantification_table.csv" "isoplot_metadata.csv" # for IsoPlot

```

