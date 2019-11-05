
<!-- README.md is generated from README.Rmd. Please edit that file -->

# treenetproc

<!-- badges: start -->

<!-- badges: end -->

The package `treenetproc` cleans raw dendrometer data. The cleaning
process aligns measurements at regular time intervals and removes jumps
and outliers. All changes are flagged can be visually examined in the
graphical outputs.

## Installation

You can install the development version of the package from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("treenet/treenetproc")
```

<br>

## Example

Below you can see a basic example of a processing result.

<img src="man/figures/README-Fig_1-1.png" width="100%" /> <br>

For more information on the functionality of the package please see the
package vignettes:

``` r
browseVignettes("treenetproc")
```

If you want to access the vignettes you need to build them when
installing the package:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("treenet/treenetproc", build_vignettes = TRUE)
```

<br>

## Citation

To cite `treenetproc` in a publication use:

``` r
citation("treenetproc")
#> 
#> To cite package 'treenetproc' in publications use:
#> 
#>   Simon Knüsel and Matthias Haeni (2019). treenetproc: Process Raw
#>   Dendrometer Data. R package version 0.1.3.9000.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {treenetproc: Process Raw Dendrometer Data},
#>     author = {Simon Knüsel and Matthias Haeni},
#>     year = {2019},
#>     note = {R package version 0.1.3.9000},
#>   }
```

Please consider citing all required packages as well. The package
[`grateful`](https://github.com/Pakillo/grateful) is a helpful resource
for this.

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("Pakillo/grateful")
```
