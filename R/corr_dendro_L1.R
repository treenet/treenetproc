#' Manually Correct Time-Aligned Dendrometer Data
#'
#' \code{corr_dendro_L1} deletes values in periods with erroneous data of
#'   time-aligned (\code{L1}) dendrometer or climate data. Values are
#'   overwritten with \code{NA} without further notice.
#'
#' @param plot logical, plots the corrected dataset with the original data
#'   in the back.
#' @inheritParams plot_proc_L2
#' @inheritParams corr_dendro_L3
#'
#' @return
#'
#' @export
#'
#' @examples
#'
corr_dendro_L1 <- function(data_L1, delete, series = NULL, plot = TRUE,
                           tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  check_data_L1(data_L1 = data_L1)
  delete <- check_datevec(var = delete, var_name = "delete", tz = tz)
  check_delete(delete = delete)
  check_series(df = data_L1, series = series)
  check_logical(var = plot, var_name = "plot")


  # Remove dendrometer data in specified periods ------------------------------
  series_select <- series
  n_series <- length(unique(data_L1$series))

  if (length(series_select) != 0) {
    df <- data_L1 %>%
      dplyr::filter(series == series_select)
    data_L1_orig <- data_L1 %>%
      dplyr::filter(series == series_select)
  } else {
    df <- data_L1
    data_L1_orig <- data_L1
  }
  if (n_series > 1) {
    data_L1_append <- data_L1 %>%
      dplyr::filter(series != series_select)
  }

  df <- deleteperiod(df = df, delete = delete) %>%
    dplyr::select(-flagdelete)


  # Plot changes --------------------------------------------------------------
  if (plot) {
    month_plot <- format(delete, format = "%Y-%m", tz = tz)
    month_plot <- paste0(unique(month_plot), "-01")

    df_plot <- df %>%
      dplyr::mutate(month = paste0(substr(ts, 1, 7), "-01")) %>%
      dplyr::filter(month %in% month_plot)

    data_L1_orig <- data_L1_orig %>%
      dplyr::mutate(month = paste0(substr(ts, 1, 7), "-01")) %>%
      dplyr::filter(month %in% month_plot)

    plot_L1(data_L1 = df_plot, period = "monthly", tz = tz,
            data_L1_orig = data_L1_orig, plot_name = "corr_L1_plot")
  }

  if (n_series > 1) {
    df <- df %>%
      dplyr::bind_rows(., data_L1_append)
  }

  return(df)
}
