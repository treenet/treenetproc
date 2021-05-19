#' Download Dendrometer Data from TreeNet or Decentlab Server
#'
#' \code{download_treenet} loads \code{L0, L1, L2, LM or L2M} dendrometer data from
#'   the TreeNet or Decentlab server.
#'
#' @param export logical, indicate whether each sensor will be saved as
#'   an \code{.RData}-file to the current working directory
#'   (\code{export = TRUE}) or will be returned to the console.
#' @inheritParams select_series
#' @inheritParams download_series
#' @inheritParams proc_L1
#'
#' @return If \code{export = TRUE} each sensor is saved as an
#'   \code{.RData}-file to the current working directory. Else,
#'   \code{export = FALSE} data will be returned to the console either as a
#'   \code{data.frame} (\code{bind_df = TRUE}), or as a \code{list} with each
#'   sensor as a list element (\code{bind_df = FALSE}).
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
                             bind_df = TRUE, server = "treenet",
                             data_format = "L0", data_version = NULL,
                             path_cred = NULL, export = FALSE, last = NULL,
                             tz = "Etc/GMT-1", use_intl = FALSE, temp_ref = FALSE) {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Download data from server -------------------------------------------------
  print("download data from server...")

  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # select series and reference temperature for download
  if (data_format %in% c("L2","LM","L2M")) {
    sensor_class <- "dendrometer"
  }
  meta_series <- select_series(site = site, sensor_class = sensor_class,
                               sensor_name = sensor_name,
                               path_cred = path_cred)

  # download selected series
  list_server <- download_series(meta_series = meta_series,
                                 data_format = data_format,
                                 data_version = data_version, from = from,
                                 to = to, last = last, bind_df = bind_df,
                                 reso = 10, path_cred = path_cred,
                                 server = server, temp_ref = temp_ref, tz = tz,
                                 use_intl = use_intl)

  # return null if no data
  if (length(return(list_server)) == 0) return(NULL)

  # Time-align downloaded data ------------------------------------------------
  if (data_format %in% c("L1", "L2")) {
    list_align <- vector("list", length = length(list_server))
    passenv$reso <- 10 # needed for fillintergaps in tsalign
    for (s in 1:length(list_align)) {
      df <- list_server[[s]]

      if (nrow(df) >= 2) {
        df <- tsalign(df = df, reso = 10, year = "asis", tz = tz) %>%
          dplyr::mutate(series = fill_na(series))
      }

      list_align[[s]] <- df
    }
    list_server <- list_align
  }


  # Return dataset ------------------------------------------------------------
  # export each series as a .RData file
  if (export) {
    print("export data...")
    series_vec <- names(list_server)
    for (s in 1:length(list_server)) {
      df <- list_server[[s]]
      save(df, file = paste0(series_vec[s], "_", data_format,
                             ".RData"), compress = "xz")
    }
    print("Done!")
  }

  if (bind_df) {
    df_out <- dplyr::bind_rows(list_server) %>%
      dplyr::arrange(series, ts)

    print("Done!")
    return(df_out)
  }

  if (!bind_df) {
    return(list_server)
  }
}
