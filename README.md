
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# GraphstatsR <a href="https://forge.inrae.fr/etienne.rifa/graphstats"><img src="man/figures/graphstatsr_150px.png" alt="ispickr" align="right" width="150" style="margin-top: 10px; margin-left: 20px;"/></a>

<!-- badges: start -->
<!-- [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental) -->
<!-- badges: end -->

GraphstatsR is an R package containing a Shiny application that enables users to generate advanced interactive graphs and statistical tests. It is tailored for metabolomics data analysis and comprises three modules:

Easy Stats: allows users to generate PCA plots, boxplots, and non-parametric statistical analyses.
ISOPLOT: a visualization module for mass spectrometry (MS) data from 13C or other isotope labeling experiments. It uses as input the corrected MS data provided by IsoCor.
MSPT: a submodule of IsoPlot designed to analyze "Pascal triangle" samples for experiment validation (Millard, Pierre, et al. 2014).

**Online instance here: https://graphstatsr.sk8.inrae.fr/**

## Prerequisites

R4.4.3 or upper is required (https://pbil.univ-lyon1.fr/CRAN/bin/)


* Linux

```bash
sudo apt-get install r-base git libssl-dev cmake libcurl4-openssl-dev libgmp3-dev libmpfr-dev zlib1g-dev
```

* Windows

[Rtools](https://cran.r-project.org/bin/windows/Rtools/) and [git](https://git-scm.com/download/win) are required.


## Installation

* In R console: 
You can install the released version of graphstats from [this
repository](https://forge.inrae.fr/etienne.rifa/graphstats) with:

``` r
install.packages("renv")
options(renv.config.gitlab.host = "https://forge.inrae.fr")

renv::install("gitlab::etienne.rifa/graphstats@master")

```

## To run Shiny app in R

``` r
library(graphstatsr)
graphstatsr::run_app()
```

## Toy dataset

Features table and metadata files are available in the `dataset` folder to test Graphstats. (`inst/dataset` in the git repo)

```r
dir( system.file("dataset/", package = "graphstatsr") )
```

