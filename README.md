
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![deprecated](http://badges.github.io/stability-badges/dist/deprecated.svg)](http://github.com/badges/stability-badges)

# RJDemetra3 (Old Repository) \[DEPRECATED\]

**Important:** This repository is now deprecated and no longer
maintained. Please refer to the [new
repository](https://github.com/rjdemetra/rjdemetra3) for the latest
developments, issues, and pull requests.

## New Repository

The development of this project has been moved to a new repository that
offers an improved structure, additional features, and regular updates.
Please follow the link below to access the new repository:

🔗 [New Repository](https://github.com/rjdemetra/rjdemetra3) :
<https://github.com/rjdemetra/rjdemetra3>

## Overview

RJDemetra3 is an R interface to JDemetra+, the seasonal adjustment
software [officially
recommended](https://ec.europa.eu/eurostat/cros/system/files/Jdemetra_%20release.pdf)
to the members of the European Statistical System (ESS) and the European
System of Central Banks. JDemetra+ implements the two leading seasonal
adjustment methods
[TRAMO/SEATS+](http://gretl.sourceforge.net/tramo/tramo-seats.html) and
[X-12ARIMA/X-13ARIMA-SEATS](https://www.census.gov/srd/www/x13as/).

Besides seasonal adjustment, JDemetra+ bundles other time series models
that are useful in the production or analysis of economic statistics,
including for instance outlier detection, nowcasting, temporal
disaggregation or benchmarking.

For more details on the JDemetra+ software see
<https://github.com/jdemetra/jdemetra-app>.

RJDemetra3 offers full access to all options and outputs of JDemetra+.

## Installation

RJDemetra3 relies on the
[rJava](https://CRAN.R-project.org/package=rJava) package and Java SE 17
or later version is required.

``` r
# Install development version from GitHub
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit")
remotes::install_github("rjdemetra/rjd3x13")
remotes::install_github("rjdemetra/rjd3tramoseats")
remotes::install_github("rjdemetra/rjdemetra3")
```

If you have troubles with the installation, check the [installation
manual](https://github.com/jdemetra/rjdemetra/wiki/Installation-manual).
