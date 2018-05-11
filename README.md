
<!-- README.md is generated from README.Rmd. Please edit that file -->
isoprocessor
============

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoprocessor)](https://cran.r-project.org/package=isoprocessor) [![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.2.1.9000-orange.svg?style=flat-square)](/commits) [![Last-Update](https://img.shields.io/badge/updated-2018--05--11-yellowgreen.svg)](/commits) [![Documentation](https://img.shields.io/badge/docs-online-green.svg)](https://kopflab.github.io/isoprocessor/) [![Build Status](https://travis-ci.org/KopfLab/turnoveR.svg?branch=master)](https://travis-ci.org/KopfLab/isoprocessor) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/KopfLab/isoprocessor?branch=master&svg=true)](https://ci.appveyor.com/project/KopfLab/isoprocessor)

About
-----

This package provides broad functionality for IRMS data processing and reduction pipelines.

Existing functionality includes easy-to-use, highly flexible data calibration and visualization pipelines for continuous flow data with additional tools on H3 factor calculation, peak detection, baseline correction and dual inlet pipelines forthcoming. All implemented functions are well documented and ready for use. However, since this package is still in active development some syntax and function names may still change.

Installation
------------

You can install the dependency [isoreader](https://kopflab.github.io/isoreader/) and [isoprocessor](https://kopflab.github.io/isoprocessor/) itself both from GitHub using the `devtools` package. Note that while [isoprocessor](https://kopflab.github.io/isoprocessor/) uses some functions from [isoreader](https://kopflab.github.io/isoreader/), it does NOT require IRMS data to be read with [isoreader](https://kopflab.github.io/isoreader/), it can be used standalone with raw data obtained differently.

``` r
install.packages("devtools") # only if you don't have this installed yet
devtools::install_github("KopfLab/isoreader")
devtools::install_github("KopfLab/isoprocessor")
```
