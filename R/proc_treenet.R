#' Process Treenet Data to L1 or L2
#'
#' \code{proc_treenet()} processes \code{L0} dendrometer data from the
#'   treenet server directly to \code{L1} (i.e. time-aligned data) or
#'   \code{L2} (i.e. cleaned \code{L1} data).
#'
#' @param to_L1 logical, process dendrometer data only to \code{L1}.
#' @inheritParams select_data
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
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
#'   \code{L1}.\cr
#'   \code{\link{proc_dendro_L2}} to process dendrometer data from \code{L1}
#'   to \code{L2}.\cr
#'   \code{\link{corr_dendro_L3}} to correct errors in processing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' proc_treenet(site = "sagno", year = "full", plot_period = "yearly")
#'
#' proc_treenet(site = "lens", plot_period = "monthly")
#'
#' proc_treenet(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                 "Alvaneu-4.dendrometer.ch0"))
#'
#' # throws error
#' proc_treenet(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                 "Alvaneu-4.dendrometer.ch0", "Bachtel-2.dendrometer.ch0"))
#' }
proc_treenet <- function(site = NULL, sensor_name = NULL,
                         from = NULL, to = NULL, path_cred = NULL,
                         temp_name = NULL, reso = 10, year = "asis",
                         n_mad = 9, iter_clean = 3, lowtemp = 5,
                         tz = "Etc/GMT-1", to_L1 = FALSE,
                         plot = TRUE, plot_period = "full",
                         plot_name = "proc_L2_plot", plot_show = "all",
                         plot_mds = FALSE) {

  # Check input variables -----------------------------------------------------
  check_logical(var = plot, var_name = "plot")


  # Process data to L2 --------------------------------------------------------
  print("download data from server...")
  df_L0 <- select_data(site = site, sensor_name = sensor_name,
                       from = from, to = to, path_cred = path_cred,
                       temp_name = temp_name)

  print("process data to L1 (time-aligned data)...")
  df_L1 <- proc_L1(data = df_L0, reso = reso, year = year, tz = tz)

  if (to_L1) {
    return(df_L1)
  } else {
    print("process data to L2...")
    df_L2 <- proc_dendro_L2(dendro_data = df_L1, n_mad = n_mad,
                            iter_clean = iter_clean, lowtemp = lowtemp,
                            plot = plot, plot_period = plot_period,
                            plot_show = plot_show, plot_name = plot_name,
                            plot_mds = plot_mds, tz = tz)

    print("Done!")
    return(df_L2)
  }
}
