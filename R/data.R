#' Raw Dendrometer Data in Long Format
#'
#' A dataset containing raw dendrometer data that is formatted in
#' long format.
#'
#' @docType data
#'
#' @usage data(dendro_data_L0)
#'
#' @format A data frame with 19814 rows and 3 variables:
#' \describe{
#'   \item{series}{name of the data series}
#'   \item{ts}{timestamp of the measurements}
#'   \item{value}{measured values}
#' }
#'
#' @keywords datasets
#'
"dendro_data_L0"


#' Raw Dendrometer Data in Wide Format
#'
#' A dataset containing raw dendrometer data that is formatted in
#' wide format.
#'
#' @docType data
#'
#' @usage data(dendro_data_L0_wide)
#'
#' @format A data frame with 147 rows and 4 variables:
#' \describe{
#'   \item{ts}{timestamp of the measurements}
#'   \item{site-1_dendro-1}{measurements of first dendrometer series}
#'   \item{site-1_dendro-2}{measurements of second dendrometer series.}
#'   \item{site-1_temperature}{temperature measurements}
#' }
#'
#' @keywords datasets
#'
"dendro_data_L0_wide"


#' Time-aligned Dendrometer Data
#'
#' A dataset containing time aligned (\code{L1}) dendrometer data.
#'
#' @docType data
#'
#' @usage data(dendro_data_L1)
#'
#' @format A data frame with 13642 rows and 4 variables.
#'
#' @inherit proc_L1 return
#'
#' @keywords datasets
#'
"dendro_data_L1"


#' Processed Dendrometer Data
#'
#' A dataset containing processed (\code{L2}) dendrometer data.
#'
#' @docType data
#'
#' @usage data(dendro_data_L2)
#'
#' @format A data frame with 13642 rows and 4 variables.
#'
#' @inherit proc_dendro_L2 return
#'
#' @keywords datasets
#'
"dendro_data_L2"


#' Raw Temperature data
#'
#' A dataset containing raw temperature data that is formatted in long
#' format.
#'
#' @docType data
#'
#' @usage data(temp_data_L0)
#'
#' @format A data frame with 3886 rows and 3 variables.
#' \describe{
#'   \item{series}{name of the data series}
#'   \item{ts}{timestamp of the measurements}
#'   \item{value}{measured values}
#' }
#'
#' @keywords datasets
#'
"temp_data_L0"


#' Time-aligned Temperature Data
#'
#' A dataset containing time aligned (\code{L1}) temperature data.
#'
#' @docType data
#'
#' @usage data(temp_data_L1)
#'
#' @format A data frame with 2879 rows and 4 variables.
#'
#' @inherit proc_L1 return
#'
#' @keywords datasets
#'
"temp_data_L1"
