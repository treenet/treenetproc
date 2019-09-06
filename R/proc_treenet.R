#' Process Treenet Data to L1 or L2
#'
#' \code{proc_treenet} processes \code{L0} dendrometer data from the
#'   treenet server directly to \code{L1} (i.e. time-aligned data) or
#'   \code{L2} (i.e. cleaned \code{L1} data).
#'
#' @param version character, specify to which level the data should be
#'   processed. Can be one of \code{L0, L1} or \code{L2}.
#' @inheritParams select_data
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @inherit proc_dendro_L2 return
#'
#' @inherit proc_dendro_L2 details
#'
#' @seealso \code{\link{proc_L1}} to process dendrometer or climate data to
#'   \code{L1}.\cr
#'   \code{\link{proc_dendro_L2}} to process dendrometer data from \code{L1}
#'   to \code{L2}.\cr
#'   \code{\link{corr_dendro_L1}} to correct errors before processing.\cr
#'   \code{\link{corr_dendro_L3}} to correct errors after processing.
#'
#' @export
#'
#' @keywords treenet
#'
#' @examples
#' \dontrun{
#' proc_treenet(site = "sagno", year = "full", plot_period = "yearly")
#'
#' proc_treenet(site = "lens", plot_period = "monthly")
#'
#' proc_treenet(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                              "Alvaneu-4.dendrometer.ch0"))
#'
#' # throws error
#' proc_treenet(sensor_name = c("Alvaneu-2.dendrometer.ch0",
#'                              "Alvaneu-4.dendrometer.ch0",
#'                              "Bachtel-2.dendrometer.ch0"))
#' }
proc_treenet <- function(site = NULL, sensor_name = NULL,
                         from = NULL, to = NULL,
                         temp_name = NULL, reso = 10, year = "asis",
                         tol = 10, iter_clean = 2, jump_corr = TRUE,
                         alpha_jump = 0.001, alpha_out = 0.01,
                         frost_thresh = 10,
                         interpol = 120, lowtemp = 5,  version = "L2",
                         plot = TRUE, plot_period = "full",
                         plot_export = TRUE, plot_name = "proc_L2_plot",
                         plot_show = "all", plot_mds = FALSE,
                         path_cred = NULL, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  check_logical(var = plot, var_name = "plot")
  if (!(version %in% c("L1", "L2"))) {
    stop("'version' needs to be 'L1' or 'L2'.")
  }


  # Process data to L2 --------------------------------------------------------
  print("download data from server...")
  df_L0 <- select_data(site = site, sensor_name = sensor_name,
                       from = from, to = to, path_cred = path_cred,
                       temp_name = temp_name, sensor_class = "dendrometer",
                       select_temp = TRUE, data_format = "L0", bind_df = TRUE,
                       server = "treenet")

  print("process data to L1 (time-aligned data)...")
  df_L1 <- proc_L1(data = df_L0, reso = reso, year = year, tz = tz)
  if (version == "L1") {
    return(df_L1)
  }

  print("process data to L2...")
  df_L2 <- proc_dendro_L2(dendro_data = df_L1, tol = tol,
                          iter_clean = iter_clean, jump_corr = jump_corr,
                          lowtemp = lowtemp, plot = plot,
                          plot_period = plot_period, plot_show = plot_show,
                          plot_export = plot_export, plot_name = plot_name,
                          plot_mds = plot_mds, tz = tz)

  maxdiff(df = df_L2, tz = tz)

  print("Done!")
  return(df_L2)
}
