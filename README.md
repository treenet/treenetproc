
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treenetproc

<!-- badges: start -->

<!-- badges: end -->

The package `treenetproc` cleans raw dendrometer data. The cleaning
process aligns measurements at regular time intervals, removes jumps and
outliers, flags all changes and plots them as well.

## Installation

You can install the development version of the package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("treenet/treenetproc")
```

## Example

Below you can see a basic example of a processing result.

<img src="man/figures/README-Fig_1-1.png" width="100%" />

For more information on the functionality of the package please see the
package vignettes:

``` r
browseVignettes("treenetproc")
```

If you want to access the vignettes you need to build them when
installing the package:

``` r
# install.packages("devtools")
devtools::install_github("treenet/treenetproc", build_vignettes = TRUE)
```
