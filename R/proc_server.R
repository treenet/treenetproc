#' Process Treenet Data to L1 or L2 on Server
#'
#' \code{proc_server} processes \code{L0} dendrometer data on the
#'   treenet server to \code{L1} or \code{L1} data to \code{L2}.
#'   For the processing to \code{L1}, all available series are processed.
#'   For the processing to \code{L2}, all dendrometer series are processed
#'   with the respective reference temperature datasets.
#'
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
proc_server <- function(from = NULL, to = NULL, last = NULL,
                        data_version = NULL,
                        frost_thr = 5, lowtemp = 5, tol_jump = 50,
                        tol_out = 10, interpol = NULL, frag_len = NULL,
                        proc_to = "L2", path_cred = NULL, year = "asis",
                        iter_clean = 1, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (proc_to == "L1") {
    data_format <- "L0"
    sensor_class <- "all"
    temp_ref <- FALSE
  }
  if (proc_to == "L2") {
    data_format <- "L1"
    sensor_class <- "dendrometer"
    temp_ref <- TRUE
  }

  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Download L0 data from server ----------------------------------------------
  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # select series and reference temperature for download
  meta_series <- select_series(site = NULL, sensor_class = sensor_class,
                               sensor_name = NULL, path_cred = path_cred)

  # download selected series
  list_down <- download_series(meta_series = meta_series,
                               data_format = data_format,
                               data_version = data_version, from = from,
                               to = to, last = last, bind_df = FALSE,
                               reso = 10, path_cred = path_cred,
                               server = "treenet", temp_ref = temp_ref,
                               tz = tz, use_intl = TRUE)


  # Process data to L1 --------------------------------------------------------
  if (proc_to == "L1") {
    list_L1 <- vector("list", length = length(list_down))
    for (s in 1:length(list_L1)) {
      df <- list_down[[s]]

      df <- proc_L1(data = df, reso = 10, year = year, input = "long",
                       date_format = "%Y-%m-%d %H:%M:%S", tz = tz)

      list_L1[[s]] <- df
    }
    df_L1 <- dplyr::bind_rows(list_L1)
    return(df_L1)
  }

  if (proc_to == "L2") {
    list_L1 <- vector("list", length = length(list_down))
    passenv$reso <- 10 # needed for fillintergaps in tsalign
    for (s in 1:length(list_L1)) {
      df <- list_down[[s]]

      if (nrow(df) >= 2) {
        df <- tsalign(df = df, reso = 10, year = "asis", tz = tz) %>%
          dplyr::mutate(series = fill_na(series))
      }
      list_L1[[s]] <- df
    }
    df_L1 <- dplyr::bind_rows(list_L1)
  }


  # Process data to L2 --------------------------------------------------------
  # add reference temperature column
  df_L1 <- df_L1 %>%
    dplyr::left_join(., meta_series, by = "series") %>%
    # add version column (only necessary as long as L1 data is processed with
    # Matthias' scripts)
    dplyr::mutate(version = NA)

  suppressMessages(
    df_L2 <- proc_dendro_L2(dendro_data = df_L1, temp_data = NULL,
                            tol_jump = tol_jump, tol_out = tol_out,
                            frost_thr = frost_thr, lowtemp = lowtemp,
                            interpol = interpol, frag_len = frag_len,
                            plot = FALSE, iter_clean = iter_clean, tz = tz)
    )

  return(df_L2)
}
