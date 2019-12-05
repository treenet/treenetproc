#' Calculate Phase Statistics
#'
#' \code{phase_stats} calculates different statistics of shrinkage and
#'   expansion phases. To identify the phases local maxima and minima are
#'   identified using two overlapping sets of time windows.
#'
#' @param dendro_L2 input \code{data.frame} containing cleaned \code{L2}
#'   dendrometer data (obtained from functions \code{\link{proc_dendro_L2}}
#'   or \code{\link{corr_dendro_L2}}).
#' @param phase_wnd numeric, specify the window length used to identify
#'   local maxima and minima. A shorter window length leads to the
#'   identification of more maxima and minima.
#' @param agg_daily logical, specify whether phase statistics are appended
#'   to the \code{L2} data or are exported as a daily aggregated
#'   \code{data.frame}.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @return
#'
#' @details The identification of local maxima and minima in the function
#'   \code{phase_stats} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'   Overlapping sets of time windows are used to identify 'true' maxima and
#'   minima, i.e. maxima or minima that appear in both overlapping time windows
#'   and are not only bound to the start or the end of the respective time
#'   windows.
#'
#' @export
#'
#' @examples
#'
phase_stats <- function(dendro_L2, phase_wnd = 8, plot_phase = FALSE,
                        plot_export = TRUE, agg_daily = FALSE, tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  df <- dendro_L2
  check_data_L2(data_L2 = df)
  reso <- reso_check_L1(df = df)
  if (reso > 360) {
    message("the time resolution of the dataset ('reso') is too coarse to
            calculate phase statistics")
    return(df)
  }


  # Calculate phase statistics ------------------------------------------------
  if ("phase_class" %in% colnames(df)) {
    df <- df %>%
      dplyr::select_if(!grepl("phase|shrink|exp|mds|mde", colnames(.)))
  }

  series_vec <- unique(df$series)
  list_phase <- vector("list", length = length(series_vec))
  df_phase <- df
  for (s in 1:length(series_vec)) {
    message(paste0("calculating phase statistics for ", series_vec[s], "..."))
    df <- df_phase %>%
      dplyr::filter(series == series_vec[s])

    maxmin1 <- findmaxmin(df = df, phase_wnd = phase_wnd, reso = reso, st = 1)
    maxmin2 <- findmaxmin(df = df, phase_wnd = phase_wnd, reso = reso, st = 2)

    max_wnd <- maxmin1[[1]] + maxmin2[[1]]
    min_wnd <- maxmin1[[2]] + maxmin2[[2]]

    max1 <- removeconsec(df = df, remove = max_wnd, notremove = min_wnd,
                         mode = "max") %>%
      dplyr::mutate(max1 = rem) %>%
      dplyr::select(-rem)
    min1 <- removeconsec(df = df, remove = min_wnd, notremove = max_wnd,
                         mode = "min") %>%
      dplyr::mutate(min1 = rem) %>%
      dplyr::select(-rem)

    maxmin <- dplyr::full_join(max1, min1, by = "ts") %>%
      dplyr::arrange(ts) %>%
      dplyr::mutate(extrema = ifelse(is.na(max1), "min", "max")) %>%
      dplyr::select(ts, extrema) %>%
      # set first and last row to a maximum
      dplyr::slice(which(extrema == "max")[1]:dplyr::n()) %>%
      dplyr::slice(1:dplyr::last(which(extrema == "max"))) %>%
      dplyr::mutate(shrink_group = c(rep(1:floor(dplyr::n() / 2), each = 2),
                                     NA)) %>%
      dplyr::mutate(exp_group = dplyr::lag(shrink_group))

    # delete extreme values that follow an NA or which are followed by an NA
    value_leadlag <- df %>%
      dplyr::mutate(value_lag = dplyr::lag(value)) %>%
      dplyr::mutate(value_lead = dplyr::lead(value)) %>%
      dplyr::select(ts, value, value_lag, value_lead)
    maxmin <- maxmin %>%
      dplyr::left_join(., value_leadlag, by = "ts") %>%
      dplyr::mutate(max_na = ifelse(is.na(value_lag), 1, 0)) %>%
      dplyr::mutate(max_na = max_na + ifelse(is.na(value_lead), 1, 0)) %>%
      dplyr::mutate(min_na = ifelse(is.na(value_lag), 1, 0)) %>%
      dplyr::mutate(min_na = min_na + ifelse(is.na(value_lead), 1, 0)) %>%
      dplyr::filter(min_na == 0 & max_na == 0) %>%
      dplyr::select(ts, extrema, shrink_group, exp_group)

    # calculate parameters for shrinkage and expansion
    shrink <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "shrink")
    exp <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "exp")

    shrink_exp_stats <- df %>%
      dplyr::full_join(., shrink, by = "ts") %>%
      dplyr::full_join(., exp, by = "ts") %>%
      dplyr::mutate(extrema = ifelse(!is.na(shrink_dur), "min",
                                     ifelse(!is.na(exp_dur), "max", NA)))

    # classify days as transpir (1) or inverted (-1)
    phase_class <- shrink_exp_stats %>%
      dplyr::filter(!is.na(shrink_start) | !is.na(exp_start)) %>%
      dplyr::mutate(day = substr(ts, 1, 10)) %>%
      dplyr::mutate(shrink_start_day = substr(shrink_start, 1, 10)) %>%
      dplyr::mutate(shrink_end_day = substr(shrink_end, 1, 10)) %>%
      dplyr::mutate(exp_start_day = substr(exp_start, 1, 10)) %>%
      dplyr::mutate(exp_end_day = substr(exp_end, 1, 10)) %>%
      dplyr::mutate(single_day_shrink = ifelse(day == shrink_start_day &
                                                 day == shrink_end_day,
                                               TRUE, FALSE)) %>%
      dplyr::mutate(single_day_exp = ifelse(day == exp_start_day &
                                              day == exp_end_day,
                                            TRUE, FALSE)) %>%
      dplyr::filter(single_day_shrink | single_day_exp) %>%
      dplyr::group_by(day) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(phase_class = dplyr::if_else(single_day_shrink, 1, 0,
                                                 missing = -1)) %>%
      dplyr::mutate(mds = shrink_amp) %>%
      dplyr::mutate(mde = exp_amp) %>%
      dplyr::mutate(ts = as.POSIXct(day, tz = tz)) %>%
      dplyr::select(ts, phase_class, mds, mde)

    shrink_exp_stats <- shrink_exp_stats %>%
      dplyr::full_join(., phase_class, by = "ts")

    if (plot_phase) {
      print("plot phases...")
      plot_phase(phase = shrink_exp_stats, plot_export = plot_export)
    }

    list_phase[[s]] <- shrink_exp_stats
  }

  df <- dplyr::bind_rows(list_phase)

  if (agg_daily) {
    df <- df %>%
      dplyr::mutate(day = as.POSIXct(substr(ts, 1, 10), tz = tz)) %>%
      dplyr::mutate(doy = as.numeric(strftime(day, format = "%j",
                                              tz = tz)))

    shrink <- df %>%
      dplyr::filter(!is.na(shrink_start)) %>%
      dplyr::select(series, day, doy, shrink_start, shrink_end, shrink_dur,
                    shrink_amp, shrink_slope)

    exp <- df %>%
      dplyr::filter(!is.na(exp_start)) %>%
      dplyr::select(series, day, doy, exp_start, exp_end, exp_dur, exp_amp,
                    exp_slope)

    mds_mde <- df %>%
      dplyr::filter(!is.na(phase_class)) %>%
      dplyr::select(series, day, doy, mds, mde, phase_class)

    df <- dplyr::full_join(shrink, exp, by = c("series", "day", "doy")) %>%
      dplyr::full_join(., mds_mde, by = c("series", "day", "doy")) %>%
      dplyr::arrange(series, day)
  }

  return(df)
}
