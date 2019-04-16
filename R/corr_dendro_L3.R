#' Manually Correct Processed Dendrometer Data
#'
#' \code{corr_dendro_L3} corrects errors in processed \code{L2} dendrometer
#'   data that are easier to correct by hand than by changing parameter values
#'   in \code{\link{proc_dendro_L2}}.
#'
#' @param data_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}. Optional, only needed for \code{remove} and if
#'   \code{plot = TRUE}.
#' @param remove character vector, specify the dates after which jumps
#'   should be removed. The first jump that occurs after the date
#'   specified is removed. If multiple jumps occur within \code{10} timesteps,
#'   all of these jumps are removed. Dates can be formatted as
#'   \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD HH:MM:SS"}.
#' @param force character vector, specify the dates after which jumps
#'   should occur. The jump with the largest value difference occurring in a
#'   period of \code{n_days} after the specified dates in \code{force} is
#'   corrected. Dates can be formatted as \code{"YYYY-MM-DD"} or
#'   \code{"YYYY-MM-DD HH:MM:SS"}.
#' @param delete character vector, specify pairs of dates between which
#'   all dendrometer data will be deleted (i.e. 4 dates will result in two
#'   periods: 1-2 and 3-4 in which data is deleted). Dates can be formatted as
#'   \code{"YYYY-MM-DD"} or \code{"YYYY-MM-DD HH:MM:SS"}.
#' @param series character, specify the name of a single dendrometer series
#'   for which changes should be made. Data of other series is left unchanged.
#'   Not needed if only a single series is provided.
#' @param plot logical, specify whether changes between \code{L2} and
#'   \code{L3} should be plotted.
#' @param n_days numeric, length of the period (in days) following the dates
#'   specified in \code{force} in which a jump is looked for.
#' @inheritParams proc_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @return The function returns a \code{data.frame} with corrected \code{L2}
#'   dendrometer data.
#'
#' @export
#'
#' @examples
#'
corr_dendro_L3 <- function(data_L1 = NULL, data_L2, remove = NULL,
                           force = NULL, delete = NULL, series = NULL,
                           plot = TRUE, n_days = 5, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (length(remove) != 0) {
    remove <- check_datevec(var = remove, var_name = "remove", tz = tz)
  }
  if (length(force) != 0) {
    force <- check_datevec(var = force, var_name = "force", tz = tz)
  }
  if (length(delete) != 0) {
    delete <- check_datevec(var = delete, var_name = "delete", tz = tz)
    check_delete(delete)
  }
  if (length(remove) == 0 & length(force) == 0 & length(delete) == 0) {
    stop("provide at least 'remove', 'force' or 'delete'.")
  }
  if (length(data_L1) != 0) {
    check_data_L1(data_L1 = data_L1)
  }
  if (length(data_L1) == 0 & length(remove) != 0) {
    stop("you need to provide 'data_L1' along with 'remove'.")
  }
  check_data_L2(data_L2 = data_L2)
  check_series(df = data_L2, series = series)
  check_logical(var = plot, var_name = "plot")
  if (plot & length(data_L1) == 0) {
    stop("data_L1 needed for plotting. Set 'plot = FALSE' or provide ",
         "'data_L1'.")
  }


  # Remove errors in processing -----------------------------------------------
  series_select <- series
  n_series <- length(unique(data_L2$series))

  if (length(data_L1) != 0 & length(series_select) != 0) {
    data_L1 <- data_L1 %>%
      dplyr::filter(series == series_select)
  }
  if (length(series_select) != 0) {
    df <- data_L2 %>%
      dplyr::filter(series == series_select)
  } else {
    df <- data_L2
  }
  if (n_series > 1) {
    data_L2_append <- data_L2 %>%
      dplyr::filter(series != series_select)
  }

  if (length(remove) != 0) {
    df <- removejump(data_L1 = data_L1, data_L2 = df, remove = remove)
  }
  if (length(force) != 0) {
    df <- forcejump(data_L2 = df, force = force, n_days = n_days)
  }
  if (length(delete) != 0) {
    df <- deleteperiod(df = df, delete = delete)
  }

  df <- calcmax(df = df)
  df <- calctwdgro(df = df, tz = tz)
  df <- grostartend(df = df, tol = 0.05, tz = tz)
  passenv$reso <- reso_check(df = df)
  df <- calcmds(df = df, reso = passobj("reso"), tz = tz,
                plot_mds = FALSE)
  df <- summariseflagscorr(df = df, remove = remove, force = force,
                           delete = delete)

  df <- df %>%
    dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
    dplyr::mutate(mds = ifelse(is.na(value), NA, mds)) %>%
    dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
    dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
    dplyr::mutate(version = 3) %>%
    dplyr::select(series, ts, value, max, twd, mds, gro_yr, gro_start,
                  gro_end, flags, version)

  if (plot) {
    if (length(remove) != 0) {
      month_plot <- format(remove, format = "%Y-%m", tz = tz)
    }
    if (length(force) != 0) {
      month_plot <- c(month_plot, format(force, format = "%Y-%m", tz = tz))
    }
    if (length(delete) != 0) {
      month_plot <- c(month_plot, format(delete, format = "%Y-%m", tz = tz))
    }
    month_plot <- paste0(unique(month_plot), "-01")

    df_plot <- df %>%
      dplyr::mutate(month = paste0(substr(ts, 1, 7), "-01")) %>%
      dplyr::filter(month %in% month_plot)

    data_L1 <- data_L1 %>%
      dplyr::mutate(month = paste0(substr(ts, 1, 7), "-01")) %>%
      dplyr::filter(month %in% month_plot)

    plot_proc_L2(data_L1 = data_L1, data_L2 = df_plot, period = "monthly",
                 show = "all", tz = tz, add = TRUE,
                 plot_name = "corr_L3_plot")
  }

  if (n_series > 1) {
    df <- df %>%
      dplyr::bind_rows(., data_L2_append)
  }

  return(df)
}
