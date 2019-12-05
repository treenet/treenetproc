#' Process Treenet Data to L1 or L2
#'
#' \code{proc_treenet} processes \code{L0} dendrometer data from the
#'   treenet server directly to \code{L1} (i.e. time-aligned data) or
#'   \code{L2} (i.e. cleaned \code{L1} data).
#'
#' @param proc_to character, specify to which level the data should be
#'   processed. Can be one of \code{L0, L1} or \code{L2}.
#' @inheritParams select_series
#' @inheritParams download_series
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
#'   \code{\link{corr_dendro_L2}} to correct errors after processing.
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
#'}
#'
proc_treenet <- function(site = NULL, sensor_name = NULL,
                         from = NULL, to = NULL, last = NULL, reso = 10,
                         frost_thr = 5, lowtemp = 5,
                         tol_jump = 50, tol_out = 10,
                         interpol = NULL, frag_len = NULL,
                         proc_to = "L2", plot = TRUE, plot_period = "full",
                         plot_export = TRUE, plot_name = "proc_L2_plot",
                         plot_show = "all", plot_phase = FALSE,
                         path_cred = NULL, year = "asis", iter_clean = 1,
                         tz = "Etc/GMT-1", use_intl = FALSE) {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Download L0 data from server ----------------------------------------------
  print("download data from server...")

  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # select series and reference temperature for download
  meta_series <- select_series(site = site, sensor_class = "dendrometer",
                               sensor_name = sensor_name,
                               path_cred = path_cred)

  # download selected series
  df_L0 <- download_series(meta_series = meta_series,
                           data_format = "L0", data_version = NULL,
                           from = from, to = to, last = last, bind_df = TRUE,
                           reso = reso, path_cred = path_cred,
                           server = "treenet", temp_ref = TRUE, tz = tz,
                           use_intl = use_intl)


  # Process data to L1 --------------------------------------------------------
  print("process data to L1 (time-aligned data)...")
  df_L1 <- proc_L1(data_L0 = df_L0, reso = reso, year = year, input = "long",
                   date_format = "%Y-%m-%d %H:%M:%S", tz = tz)
  if (proc_to == "L1") {
    return(df_L1)
  }


  # Process data to L2 --------------------------------------------------------
  print("process data to L2...")

  # add reference temperature column
  df_L1 <- df_L1 %>%
    dplyr::left_join(., meta_series, by = "series")

  if (use_intl) {
    suppressMessages(
      df_L2 <- proc_dendro_L2(dendro_L1 = df_L1, temp_L1 = NULL,
                              tol_jump = tol_jump, tol_out = tol_out,
                              frost_thr = frost_thr, lowtemp = lowtemp,
                              interpol = interpol, frag_len = frag_len,
                              plot = plot, plot_period = plot_period,
                              plot_show = plot_show, plot_export = plot_export,
                              plot_name = plot_name, iter_clean = iter_clean,
                              tz = tz)
    )
  } else {
    df_L2 <- proc_dendro_L2(dendro_L1 = df_L1, temp_L1 = NULL,
                            tol_jump = tol_jump, tol_out = tol_out,
                            frost_thr = frost_thr, lowtemp = lowtemp,
                            interpol = interpol, frag_len = frag_len,
                            plot = plot, plot_period = plot_period,
                            plot_show = plot_show, plot_export = plot_export,
                            plot_name = plot_name, iter_clean = iter_clean,
                            tz = tz)
  }

  df_L2 <- grow_seas(df = df_L2, tz = tz)
  df_L2 <- phase_stats(df = df_L2, tz = tz, plot_phase = plot_phase,
                       plot_export = plot_export)

  print("Done!")
  return(df_L2)
}
