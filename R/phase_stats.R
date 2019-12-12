#' Calculate Phase Statistics
#'
#' \code{phase_stats} calculates different statistics of shrinkage and
#'   expansion phases. To identify the phases local maxima and minima are
#'   identified using two overlapping sets of time windows.
#'
#' @param dendro_L2 input \code{data.frame} containing cleaned \code{L2}
#'   dendrometer data (obtained from functions \code{\link{proc_dendro_L2}}
#'   or \code{\link{corr_dendro_L2}}).
#' @param phase_wnd numeric, specify the window length (in hours) used to
#'   identify local maxima and minima. A shorter window length leads to the
#'   identification of more local maxima and minima.
#' @param plot_phase logical, specify whether maxima and minima used for the
#'   calculation of the phase statistics should be plotted.
#' @param agg_daily logical, specify whether phase statistics are appended
#'   to the \code{L2} data or are exported as a daily aggregated
#'   \code{data.frame}.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @return The following additional variables are returned by
#'   \code{phase_stats}:
#'    \item{shrink_start}{timestamp of the start of the shrinkage phase.}
#'    \item{shrink_end}{timestamp of the end of the shrinkage phase.}
#'    \item{shrink_dur}{duration of the shrinkage phase in minutes.}
#'    \item{shrink_amp}{amplitude of the shrinkage phase.}
#'    \item{shrink_slope}{slope of the shrinkage phase.}
#'  The same variables are returned for the expansion (\code{exp}) phase.
#'    \item{mds}{maximum daily shrinkage, only returned on days on which a
#'      local maximum is followed by a local minimum and the shrinkage finishes
#'      on the same day.}
#'    \item{mde}{maximum daily expansion, only returned on days where a
#'      local minimum is followed by a local maximum and the exmpansion
#'      finishes on the same day.}
#'    \item{phase_class}{days are classified into days on which a shrinkage
#'      occurrs during the day \code{(1)}, i.e. where the stem radius is
#'      likely driven by transpiration; and days on which an expansion occurs
#'      \code{(-1)}, i.e. where the stem radius is likely driven by
#'      temperature.}
#'
#'   In case data is not aggregated to daily values
#'   (\code{agg_daily = FALSE}), all columns are appended to \code{dendro_L2}.
#'   All parameters related to shrinkage or expansion phases are pasted at
#'   the timestamp corresponding to the end of the respective phases
#'   (\code{shrink_end} or \code{exp_end}). All other values are set to \code{NA}.
#'   All daily statistics (\code{mds}, \code{mde} and \code{phase_class}) are
#'   pasted at the first timestamp of the day.
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
                        plot_export = TRUE, agg_daily = TRUE, tz = "UTC") {

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
      dplyr::select_if(!grepl("phase|shrink|exp", colnames(.)))
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
    shrink <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "shrink",
                                 phase_wnd = phase_wnd, tz = tz)
    exp <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "exp",
                              phase_wnd = phase_wnd, tz = tz)

    shrink_exp <- dplyr::full_join(shrink, exp, by = c("day", "doy")) %>%
      dplyr::mutate(series = series_vec[s]) %>%
      # overwrite duplicated shrinkage or expansion phases with NA
      dplyr::mutate(shrink_start =
                      replace(shrink_start, duplicated(shrink_start), NA)) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs = list(~ replace(., is.na(shrink_start), NA))) %>%
      dplyr::mutate(exp_start =
                      replace(exp_start, duplicated(exp_start), NA)) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ replace(., is.na(exp_start), NA))) %>%
      # rearrange rows to reduce rows with NAs
      dplyr::group_by(day) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs =
                         list(~ c(na.omit(.),
                                  rep(NA, sum(is.na(shrink_start)))))) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ c(na.omit(.),
                                        rep(NA, sum(is.na(exp_start)))))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!(is.na(shrink_start) & is.na(exp_start))) %>%
      # classify days as transpir (1) or inverted (-1)
      dplyr::mutate(shrink_start_day =
                      as.POSIXct(substr(shrink_start, 1, 10), tz = tz)) %>%
      dplyr::mutate(exp_start_day =
                      as.POSIXct(substr(exp_start, 1, 10), tz = tz)) %>%
      dplyr::mutate(day_shrink = dplyr::if_else(shrink_start_day == day, 1, 0,
                                                missing = 0)) %>%
      dplyr::mutate(day_exp = dplyr::if_else(exp_start_day == day, -1, 0,
                                             missing = 0)) %>%
      dplyr::group_by(day) %>%
      dplyr::mutate(phase_class = sum(max(day_shrink), min(day_exp))) %>%
      dplyr::mutate(phase_class = ifelse(phase_class == 0,
                                         NA, phase_class)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(day) %>%
      dplyr::select(series, day, doy, shrink_start, shrink_end, shrink_dur,
                    shrink_amp, shrink_slope, exp_start, exp_end, exp_dur,
                    exp_amp, exp_slope, phase_class)

    list_phase[[s]] <- shrink_exp

    if (plot_phase) {
      print("plot phases...")
      plot_phase(df = df, phase = shrink_exp, plot_export = plot_export)
    }
  }

  phase <- dplyr::bind_rows(list_phase)

  if (!agg_daily) {
  # merge phases with df
  shrink <- phase %>%
    dplyr::select(series, shrink_start, shrink_end, shrink_dur, shrink_amp,
                  shrink_slope) %>%
    dplyr::mutate(ts = shrink_end) %>%
    dplyr::filter(!is.na(shrink_start))

  exp <- phase %>%
    dplyr::select(series, exp_start, exp_end, exp_dur, exp_amp, exp_slope) %>%
    dplyr::mutate(ts = exp_end) %>%
    dplyr::filter(!is.na(exp_start))

  phase_class <- phase %>%
    dplyr::select(series, day, phase_class) %>%
    dplyr::select(series, ts = day, phase_class) %>%
    dplyr::distinct()

  phase <- dplyr::left_join(df, shrink, by = c("series", "ts")) %>%
    dplyr::left_join(., exp, by = c("series", "ts")) %>%
    dplyr::left_join(., phase_class, by = c("series", "ts")) %>%
    dplyr::arrange(series, ts)
  }

  return(phase)
}
