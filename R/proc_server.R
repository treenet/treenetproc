#' Process Treenet Data to L1 or L2 on Server
#'
#' \code{proc_server} processes \code{L0} dendrometer data on the
#'   treenet server directly to \code{L1} (i.e. time-aligned data) or
#'   \code{L2} (i.e. cleaned \code{L1} data).
#'
#' @param on_server logical, specify whether the function is used on the
#'   server. If \code{on_server = TRUE}, all messages are suppressed.
#' @inheritParams select_series
#' @inheritParams download_series
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
#' @inheritParams proc_treenet
#'
#' @inherit proc_dendro_L2 return
#'
#' @export
#'
#' @keywords treenet, server
#'
proc_server <- function(from = NULL, to = NULL, last = NULL, reso = 10,
                        frost_thr = 5, lowtemp = 5, tol_jump = 50,
                        tol_out = 10, interpol = NULL, frag_len = NULL,
                        proc_to = "L2", path_cred = NULL, year = "asis",
                        iter_clean = 1, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (proc_to == "L1") {
    data_format <- "L0"
    sensor_class <- "all"
  }
  if (proc_to == "L2") {
    data_format <- "L1"
    sensor_class <- "dendrometer"
  }

  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Download L0 data from server ----------------------------------------------
  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # select series and reference temperature for download
  meta_series <- select_series(site = NULL, sensor_class = sensor_class,
                               sensor_name = NULL, path_cred = path_cred)

  message("remove the subsetting code")
  meta_series <- meta_series %>%
    dplyr::slice(1:10)

  # download selected series
  print("download data...")
  df_down <- download_series(meta_series = meta_series,
                             data_format = data_format, data_version = NULL,
                             from = from, to = to, last = last, bind_df = TRUE,
                             reso = reso, path_cred = path_cred,
                             server = "treenet", temp_ref = TRUE, tz = tz,
                             use_intl = TRUE)


  # Process data to L1 --------------------------------------------------------
  print("process to L1...")
  if (data_format == "L2") {
    df_L1 <- df_down
  } else {
    df_L1 <- proc_L1(data = df_down, reso = reso, year = year, input = "long",
                     date_format = "%Y-%m-%d %H:%M:%S", tz = tz)
  }
  if (proc_to == "L1") {
    return(df_L1)
  }


  # Process data to L2 --------------------------------------------------------
  print("process to L2...")
  # add reference temperature column
  df_L1 <- df_L1 %>%
    dplyr::left_join(., meta_series, by = "series")

  #suppressMessages(
    df_L2 <- proc_dendro_L2(dendro_data = df_L1, temp_data = NULL,
                            tol_jump = tol_jump, tol_out = tol_out,
                            frost_thr = frost_thr, lowtemp = lowtemp,
                            interpol = interpol, frag_len = frag_len,
                            plot = FALSE, iter_clean = iter_clean, tz = tz)
    #)

  return(df_L2)
}
