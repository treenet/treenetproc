#' treenetproc - Clean, process and visualize dendrometer data
#'
#' The \code{treenetproc} package contains three main functions:
#'   \code{proc_L1}, \code{proc_dendro_L2}, \code{corr_dendro_L3}
#'
#' @section \code{proc_L1()}:
#'   The function \code{proc_L1} aligns the dendrometer measurements to
#'   regular time intervals.
#'
#' @section \code{proc_dendro_L2()}:
#'   The function \code{proc_dendro_L2} removes jumps and outliers.
#'
#' @section \code{corr_dendro_L3()}:
#'   The function \code{corr_dendro_L3} can be used to manually correct
#'   remaining errors in data cleaning.
#'
#' @section Vignettes:
#'   More information and some examples on how to use the functions can be
#'   found in the vignettes of the package. To see the vignettes use:\cr
#'   \code{browseVignettes("treenetproc")}\cr
#'   If \code{browseVignettes} does not work, the package may need to be
#'   reinstalled along with the vignettes:\cr
#'   \code{library(devtools)}\cr
#'   \code{devtools::install_github("treenet/treenetproc",
#'   build_vignettes = TRUE))}
#'
#' @docType package
#' @name treenetproc
NULL
