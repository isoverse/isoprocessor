
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isoprocessor <a href='http://isoprocessor.isoverse.org'><img src='man/figures/isoprocessor_logo_thumb.png' align="right" height="138.5"/></a>

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/isoprocessor)](https://cran.r-project.org/package=isoprocessor)
[![Git\_Hub\_Version](https://img.shields.io/badge/GitHub-0.6.11-orange.svg?style=flat-square)](https://github.com/isoverse/isoprocessor/commits)
[![Documentation](https://img.shields.io/badge/docs-online-green.svg)](http://isoprocessor.isoverse.org/)
[![R build
status](https://github.com/isoverse/isoprocessor/workflows/R-CMD-check/badge.svg)](https://github.com/isoverse/isoprocessor/actions?workflow=R-CMD-check)
[![Binder](https://img.shields.io/badge/launch-RStudio-blue.svg)](https://mybinder.org/v2/gh/isoverse/isoprocessor/binder?urlpath=rstudio)
[![Binder](https://img.shields.io/badge/launch-Jupyter-orange.svg)](https://mybinder.org/v2/gh/isoverse/isoprocessor/binder?urlpath=lab)

## About

This package provides broad functionality for IRMS data processing and
reduction pipelines.

Existing functionality includes signal conversion (voltage to current
and back), time scaling (continuous flow chromatograms), isotope ratio
calculations, delta value calculations, as well as easy-to-use highly
flexible data calibration and visualization pipelines for continuous
flow data. Additional tools on O17 corrections, H3 factor calculation,
peak detection, baseline correction, etc are in the works. All
implemented functions are well documented and ready for use. However,
since this package is still in active development some syntax and
function names may still change.

## Installation

You can install the [isoprocessor](http://isoprocessor.isoverse.org/)
package from GitHub using the `devtools` package. Note that while
[isoprocessor](http://isoprocessor.isoverse.org/) uses some functions
from [isoreader](http://isoreader.isoverse.org/), it does NOT require
IRMS data to be read with [isoreader](http://isoreader.isoverse.org/),
it can be used standalone with raw data obtained differently.

``` r
# installs the development tools package if not yet installed
if(!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools") 

# installs the newest version of isoprocessor
devtools::install_github("isoverse/isoprocessor")
```

## Update

To update to a newer version of isoprocessor:

``` r
# installs the newest version of isoprocessor
devtools::install_github("isoverse/isoprocessor")
```

Troubleshooting note: depending on your workspace and operating system,
you may have to re-start your R session, delete previous versions of
these packages (`remove.packages("isoprocessor")`,
`remove.packages("isoreader")`), and/or manually install some
dependencies (e.g. the `digest` package tends to cause trouble:
`remove.packages("digest"); install.packages("digest")`).

## Functionality

-   for a full reference of all available functions, see the **[Function
    Reference](http://isoprocessor.isoverse.org/reference/)**
-   for an example of how to work with continuos flow data, see the
    vignette on **[Continuous
    Flow](http://isoprocessor.isoverse.org/articles/continuous_flow.html)**
-   for an example of how to work with dual inlet data, see the vignette
    on **[Dual
    Inlet](http://isoprocessor.isoverse.org/articles/dual_inlet.html)**
-   for an example of how Thermo Isodat in particular calculates isotope
    ratios for standard continuous flow and dual inlet data, see the
    **[Isodat calculations
    notebook](http://isoprocessor.isoverse.org/articles/isodat_calculations.html)**
-   additional vignettes on data reduction and calibration are in the
    works:
    -   example 1: **[bulk carbon isotope
        analysis](http://isoprocessor.isoverse.org/articles/ea_irms_example_carbon.html)**
    -   example 2: **[compound specific carbon isotope
        analysis](http://isoprocessor.isoverse.org/articles/gc_irms_example_carbon.html)**

## Open Source

[isoprocessor](http://isoprocessor.isoverse.org/) is and will always be
fully open-source (i.e. free as in ‘freedom’ and free as in ‘free beer’)
and is provided as is. The source code is released under GPL-2.

## isoverse <a href='http://www.isoverse.org'><img src='man/figures/isoverse_logo_thumb.png' align="right" height="138.5"/></a>

This package is part of the isoverse suite of data tools for stable
isotopes. If you like the functionality that isoverse packages provide
to the geochemical community, please help us spread the word and include
an isoverse or individual package logo on one of your posters or slides.
All logos are posted in high resolution in the main [isoverse
repository](https://github.com/isoverse/isoverse/tree/master/man/figures).
