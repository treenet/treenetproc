#' Download Dendrometer Data from TreeNet or Decentlab Server
#'
#' \code{download_treenet} loads \code{L0, L1 or L2} dendrometer data from
#'   the TreeNet or Decentlab server.
#'
#' @param export logical, indicate whether each sensor will be saved as
#'   an \code{.RData}-file in the current working directory
#'   (\code{export = TRUE}) or will be returned to the console.
#' @inheritParams select_series
#' @inheritParams download_series
#' @inheritParams proc_L1
#'
#' @return If \code{export = TRUE} data will be saved in a named list with
#'   each list element containing one sensor.
#'
#'   If \code{export = FALSE} data will be saved in the console as a
#'   \code{data.frame}.
#'
#' @export
#'
#' @keywords treenet
#'
#' @examples
#' \dontrun{
#' download_treenet(site = "lens", export = TRUE, bind_df = FALSE,
#'                  server = "decentlab")
#' }
download_treenet <- function(site = NULL, sensor_name = NULL,
                             sensor_class = NULL, from = NULL, to = NULL,
                             server = "treenet", data_format = "L0",
                             path_cred = NULL, export = FALSE, last = NULL,
                             temp_ref = FALSE, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Download data from server -------------------------------------------------
  print("download data from server...")

  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # select series and reference temperature for download
  meta_series <- select_series(site = site, sensor_class = sensor_class,
                               sensor_name = sensor_name,
                               path_cred = path_cred)

  # download selected series
  df_server <- download_series(meta_series = meta_series,
                               data_format = data_format, from = from,
                               to = to, last = last, bind_df = TRUE,
                               reso = reso, path_cred = path_cred,
                               server = server, temp_ref = temp_ref, tz = tz)


  # Time-align downloaded data ------------------------------------------------
  if (data_format %in% c("L1", "L2")) {
    # needed for fillintergaps inside tsalign
    passenv$reso <- 10
    options(warn = -1)
    df_server <- tsalign(df = df_server, reso = 10, year = "asis",
                         tz = tz) %>%
      dplyr::mutate(series = fill_na(series))
    options(warn = 0)
  }


  # Return dataset ------------------------------------------------------------
  # export each series as a .RData file
  if (export) {
    print("export data...")
    series_vec <- unique(df_server$series)
    for (s in 1:length(series_vec)) {
      df_single <- df_server %>%
        dplyr::filter(series == series_vec[s])
      save(df_single, file = paste0(series_vec[s], "_", data_format,
                                    ".RData"), compress = "xz")
    }
    print("Done!")
  }

  print("Done!")
  return(df_server)
}
