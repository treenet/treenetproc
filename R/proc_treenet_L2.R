#' Process Treenet Data to L2
#'
#' \code{proc_treenet_L2()} processes \code{L0} dendrometer data from the
#'   treenet server directly to \code{L2}.
#'
#' @param plot logical, specify whether a comparison of \code{L1} and \code{L2}
#'   data should be plotted.
#' @inheritParams select_data
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_dendro
#'
#' @return The function returns:
#'  a \code{data.frame} with processed dendrometer data containing the
#'  following columns
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{max}{highest measured value up to this timestamp.}
#'    \item{twd}{tree water deficit, i.e. the amount of stem shrinkage
#'      expressed as the difference between \code{max} and \code{value}.}
#'    \item{mds}{maximum daily shrinkage, calculated as the difference between
#'      a local maximum that occurs before a local minimum during one day. If
#'      there is no local maximum or minimum or if the minimum occurs prior to
#'      the maximum, then \code{mds = NA}. This may occur on days with rain or
#'      in winter.}
#'    \item{gro_yr}{growth since the beginning of the year. Also calculated if
#'      for years with missing data.}
#'    \item{flags}{number specifying whether and which changes occurred during
#'      the processing.}
#'    \item{version}{processing version.}
#'
#' @seealso \code{\link{proc_L1}} to process dendrometer or climate data to
#'   \code{L1} only (i.e. time-aligned data), \code{\link{proc_dendro_L2}}
#'   to process dendrometer data from \code{L1} to \code{L2}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' proc_treenet_L2(site = "sagno", year = "full", period = "yearly")
#'
#' proc_treenet_L2(site = "lens", add = FALSE)
#'
#' proc_treenet_L2(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                 "Alvaneu-4.dendrometer.ch0"))
#'
#' # throws error
#' proc_treenet_L2(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                 "Alvaneu-4.dendrometer.ch0", "Bachtel-2.dendrometer.ch0"))
#' }
proc_treenet_L2 <- function(site = NULL, sensor_name = NULL,
                            from = NULL, to = NULL, path_cred = NULL,
                            temp_name = NULL, reso = 10, year = "asis",
                            val_range = c(0, 20000), diffwin = 2000,
                            diffsum = 1000, lowtemp = 5, tz = "Etc/GMT-1",
                            plot = TRUE, period = "full", add = TRUE) {

  # Check input variables -----------------------------------------------------
  if (plot != TRUE & plot != FALSE) {
    stop("provide 'plot' with either TRUE or FALSE")
  }


  # Process data to L2 --------------------------------------------------------
  print("download data from server...")
  df_L0 <- select_data(site = site, sensor_name = sensor_name,
                       from = from, to = to, path_cred = path_cred,
                       temp_name = temp_name)

  print("process data to L1 (time-aligned data)...")
  df_L1 <- proc_L1(data = df_L0, reso = reso, year = year, tz = tz)

  print("process data to L2...")
  df_L2 <- proc_dendro_L2(dendro_data = df_L1, val_range = val_range,
                          diffwin = diffwin, diffsum = diffsum, tz = tz)

  print("plot data...")
  if (plot) {
    plot_dendro(data_L1 = df_L1, data_L2 = df_L2, period = period, tz = tz,
                add = add)
  }
  print("Done!")
  return(df_L2)
}
