#' Calculate Phase Statistics
#'
#' \code{phase_stats} calculates different statistics of shrinkage and
#'   expansion phases. To identify the phases local maxima and minima are
#'   identified using a moving window.
#'
#' @param df input \code{data.frame}.
#' @param agg_daily logical, specify whether phase statistics are appended
#'   to the \code{L2} data or are exported as a daily aggregated
#'   \code{data.frame}.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @return
#'
#' @details \code{phase_stats} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'
#' @export
#'
#' @examples
#'
phase_stats <- function(df, plot_phase = FALSE, plot_export = TRUE,
                        agg_daily = FALSE, tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
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
    message(paste0("calculating phase statistics of ", series_vec[s], "..."))
    df <- df_phase %>%
      dplyr::filter(series == series_vec[s])

    maxmin1 <- findmaxmin(df = df, reso = reso, st = 1)
    maxmin2 <- findmaxmin(df = df, reso = reso, st = 2)

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
    shrink <- calcshrinkexpparam(df = df, mode = "shrink")
    exp <- calcshrinkexpparam(df = df, mode = "exp")

    shrink_exp_stats <- df %>%
      dplyr::full_join(., shrink, by = "ts") %>%
      dplyr::full_join(., exp, by = "ts") %>%
      dplyr::mutate(extrema = ifelse(!is.na(shrink_dur), "min",
                                     ifelse(!is.na(exp_dur), "max", NA)))

    # classify days as transpir (1) or inverted (-1)
    # day is transp, if there is a shrinkage occurring during the day
    # day is inv, if there is a expansion occurring during the day
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

  return(df)
}
