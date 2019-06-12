#' Download Dendrometer Data from TreeNet or Decentlab Server
#'
#' \code{download_treenet} loads \code{L0, L1 or L2} dendrometer data from
#'   the TreeNet or Decentlab server.
#'
#' @inheritParams select_data
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
                             from = NULL, to = NULL,
                             server = "treenet",
                             data_format = "L0", path_cred = NULL,
                             export = FALSE, bind_df = TRUE,
                             last = NULL, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (!(data_format %in% c("L0", "L1", "L2"))) {
    stop("'data_format' needs to be 'L0', 'L1' or 'L2'.")
  }
  if (!(server %in% c("treenet", "decentlab"))) {
    stop("'server' needs to be 'treenet' or 'decentlab'.")
  }
  check_logical(var = export, var_name = "export")
  check_logical(var = bind_df, var_name = "bind_df")
  if (export && bind_df) {
    stop("'export' and 'bind_df' cannot both be TRUE at the same time.")
  }


  # Download data from server -------------------------------------------------
  print("download data from server...")
  df_server <- select_data(site = site, sensor_name = sensor_name,
                           from = from, to = to, server = server,
                           data_format = data_format, path_cred = path_cred,
                           select_temp = FALSE, export = export,
                           bind_df = bind_df, last = last, tz = tz)

  print("Done!")
  return(df_server)
}