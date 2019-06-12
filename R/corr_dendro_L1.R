#' Manually Correct Time-Aligned Dendrometer Data
#'
#' \code{corr_dendro_L1} deletes values in periods with erroneous data of
#'   time-aligned (\code{L1}) dendrometer or climate data. Values are
#'   overwritten with \code{NA} without further notice.
#'
#' @param plot logical, plots the corrected dataset with the original data
#'   in the background in grey.
#' @inheritParams plot_proc_L2
#' @inheritParams corr_dendro_L3
#'
#' @return The function returns a \code{data.frame} with corrected \code{L1}
#'   dendrometer data.
#'
#' @seealso \code{\link{corr_dendro_L3}} to correct \code{L2} data.
#'
#' @export
#'
#' @examples
#' corr_dendro_L1(data_L1 = dendro_data_L1,
#'                delete = c("2016-08-01", "2016-08-05"),
#'                series = "site-1_dendro-3", plot_export = FALSE)
#'
corr_dendro_L1 <- function(data_L1, delete, series = NULL, plot = TRUE,
                           plot_export = TRUE, tz = "UTC") {

  # Subset data to selected series --------------------------------------------
  check_series(df = data_L1, series = series)

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


  # Check input variables -----------------------------------------------------
  check_data_L1(data_L1 = data_L1)
  delete <- check_datevec(datevec = delete, tz = tz)
  check_delete(delete = delete)
  check_date_period(datevec = delete, datevec_name = "delete", df = df)
  check_logical(var = plot, var_name = "plot")


  # Remove dendrometer data in specified periods ------------------------------
  # remove leading and trailing NA's
  na_list <- remove_lead_trail_na(df = df)
  df <- na_list[[1]]
  lead_trail_na <- na_list[[2]]

  df <- deleteperiod(df = df, delete = delete) %>%
    dplyr::select(-flagdelete)

  # append leading and trailing NA's
  df <- append_lead_trail_na(df = df, na = lead_trail_na)


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

    plot_L1(data_L1 = df_plot, data_L1_orig = data_L1_orig,
            plot_period = "monthly", plot_export = plot_export,
            plot_name = "corr_L1_plot", tz = tz)
  }

  if (n_series > 1) {
    df <- df %>%
      dplyr::bind_rows(., data_L1_append)
  }

  return(df)
}
