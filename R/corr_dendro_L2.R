#' Manually Correct Processed Dendrometer Data
#'
#' \code{corr_dendro_L2} corrects remaining errors in cleaned \code{L2}
#'   dendrometer data. Can be used to manually correct errors that cannot
#'   be removed by changing input parameter values in
#'   \code{\link{proc_dendro_L2}} or to reverse errors that are introduced
#'   during data cleaning.
#'
#' @param dendro_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}. Optional, only needed for \code{reverse} and if
#'   \code{plot = TRUE}.
#' @param reverse numeric vector, specify numbers of the changes that should
#'   be reversed. Numbers are reported in the plots produced by
#'   \code{\link{proc_dendro_L2}} or \code{\link{plot_proc_L2}} with the
#'   argument \code{plot_period = "monthly"}.
#' @param force character vector, specify the dates after which jumps
#'   should be corrected. The largest value difference occurring in a
#'   period of \code{n_days} after the specified dates in \code{force} is
#'   corrected. Dates need to be in a standard date or
#'   datetime format (e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' @param delete character vector, specify pairs of dates between which
#'   all dendrometer data will be deleted (i.e. 4 dates will result in two
#'   periods: 1-2 and 3-4 in which data is deleted). Dates need to be in the
#'   same standard date or datetime format
#'   (e.g. \code{\%Y-\%m-\%d \%H:\%M:\%S}).
#' @param series character, specify the name of a single dendrometer series
#'   for which changes should be made. Data of other series is left unchanged.
#'   Not needed if only a single series is provided.
#' @param plot logical, specify whether changes between \code{L2} and
#'   \code{L3} should be plotted.
#' @param n_days numeric, length of the period (in days) following the dates
#'   specified in \code{force} in which a jump is corrected. Increase if
#'   the gap in data is longer than the default (\code{n_days = 5}).
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @return The function returns a \code{data.frame} with corrected \code{L2}
#'   dendrometer data. The corrections are documented in the column
#'   \code{flags}.
#'
#' @seealso \code{\link{corr_dendro_L1}} to correct \code{L1} data.
#'
#' @export
#'
#' @examples
#' corr_dendro_L2(dendro_L1 = dendro_data_L1, dendro_L2 = dendro_data_L2,
#'                reverse = 59:61, force = "2013-08-12",
#'                delete = c("2013-08-01", "2013-08-04"),
#'                series = "site-1_dendro-3", plot_export = FALSE)
#'
corr_dendro_L2 <- function(dendro_L1 = NULL, dendro_L2, reverse = NULL,
                           force = NULL, delete = NULL, series = NULL,
                           n_days = 5, plot = TRUE, plot_export = TRUE,
                           tz = "UTC") {

  # Subset data to selected series --------------------------------------------
  check_series(df = dendro_L2, series = series)

  series_select <- series
  n_series <- length(unique(dendro_L2$series))

  if (length(dendro_L1) != 0 && length(series_select) != 0) {
    dendro_L1 <- dendro_L1 %>%
      dplyr::filter(series == series_select)
  }
  if (length(series_select) != 0) {
    df <- dendro_L2 %>%
      dplyr::filter(series == series_select)
  } else {
    df <- dendro_L2
  }
  if (n_series > 1) {
    data_L2_append <- dendro_L2 %>%
      dplyr::filter(series != series_select)
  }


  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  if (length(reverse) != 0) {
    if (!(is.numeric(reverse))) {
      stop("'reverse' needs to be numeric.")
    }
  }
  if (length(force) != 0) {
    force <- check_datevec(datevec = force, datevec_name = "force", tz = tz)
    check_date_period(datevec = force, datevec_name = "force", df = df)
  }
  if (length(delete) != 0) {
    delete <- check_datevec(datevec = delete, datevec_name = "delete", tz = tz)
    check_date_period(datevec = delete, datevec_name = "delete", df = df)
    check_delete(delete)
  }
  if (length(reverse) == 0 & length(force) == 0 & length(delete) == 0) {
    stop("provide at least 'reverse', 'force' or 'delete'.")
  }
  if (length(dendro_L1) != 0) {
    check_data_L1(data_L1 = dendro_L1)
  }
  if (length(dendro_L1) == 0 && length(reverse) != 0) {
    stop("you need to provide 'dendro_L1' along with 'reverse'.")
  }
  check_data_L2(data_L2 = dendro_L2)
  if (plot & length(dendro_L1) == 0) {
    stop("'dendro_L1' needed for plotting. Set 'plot = FALSE' or provide ",
         "'dendro_L1'.")
  }
  data_L1 <- dendro_L1

  # Reverse errors in processing ----------------------------------------------
  # remove leading and trailing NA's
  na_list <- remove_lead_trail_na(df = df)
  df <- na_list[[1]]
  lead_trail_na <- na_list[[2]]

  if (length(reverse) != 0) {
    reverse_list <- reversecorr(data_L1 = data_L1, data_L2 = df,
                                reverse = reverse, tz = tz)
    df <- reverse_list[[1]]
    diff_old <- reverse_list[[2]]
  }
  if (length(force) != 0) {
    df <- forcejump(data_L2 = df, force = force, n_days = n_days)
  }
  if (length(delete) != 0) {
    df <- deleteperiod(df = df, delete = delete)
  }

  df <- calcmax(df = df)
  df <- calctwdgro(df = df, tz = tz)
  df <- summariseflagscorr(df = df, reverse = reverse, force = force,
                           delete = delete)

  df <- df %>%
    dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
    dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
    dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
    dplyr::select(series, ts, value, max, twd, gro_yr, frost, flags) %>%
    dplyr::mutate(
      version = utils::packageDescription("treenetproc",
                                          fields = "Version", drop = TRUE))

  # append leading and trailing NA's
  df <- append_lead_trail_na(df = df, na = lead_trail_na)

  if (plot) {
    data_L1 <- data_L1 %>%
      dplyr::mutate(month_plot = 0) %>%
      dplyr::mutate(month = paste0(substr(ts, 1, 7), "-01"))

    if (length(reverse) != 0) {
      data_L1 <- data_L1 %>%
        # add diff_old to plot reversed changes
        dplyr::left_join(., diff_old, by = "ts")
    }
    # add months in which delete and force were applied
    if (length(delete) != 0) {
      month_delete <- format(delete, format = "%Y-%m", tz = tz)
      month_delete <- paste0(unique(month_delete), "-01")
      data_L1 <- data_L1 %>%
        dplyr::mutate(month_del = ifelse(month %in% month_delete, 1, 0)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(month_plot = sum(month_plot, month_del)) %>%
        dplyr::ungroup() %>%
        dplyr::select(-month_del)
    }
    if (length(force) != 0) {
      month_force <- format(force, format = "%Y-%m", tz = tz)
      month_force <- paste0(unique(month_force), "-01")
      data_L1 <- data_L1 %>%
        dplyr::mutate(month_forc = ifelse(month %in% month_force, 1, 0)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(month_plot = sum(month_plot, month_forc)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(month_plot = ifelse(month_plot >= 1, 1, 0)) %>%
        dplyr::select(-month_forc)
    }

    plot_proc_L2(dendro_L1 = data_L1, dendro_L2 = df,
                 plot_period = "monthly", plot_show = "diff_corr",
                 plot_export = plot_export,
                 plot_name = "corr_L3_plot", tz = tz)
  }

  if (n_series > 1) {
    df <- df %>%
      dplyr::bind_rows(., data_L2_append)
  }

  return(df)
}
