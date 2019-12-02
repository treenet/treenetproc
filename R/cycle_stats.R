#' Calculates Cycle Statistics
#'
#' \code{cycle_stats} calculates different statistics and characteristics of
#'   cycles. To identify the cycles local maxima and minima are identified
#'   using a moving window.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @details \code{cycle_stats} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'
#' @export
#'
#' @example
#'
cycle_stats <- function(df, reso, tz, plot_cycle = FALSE, plot_export) {

  if ("cycle" %in% colnames(df)) {
    df <- df %>%
      dplyr::select_if(!grepl("cycle|shrink|ref|mds", colnames(.)))
  }

  if (reso > 360) {
    message("the time resolution of the dataset ('reso') is too coarse to
            calculate cycle statistics")
    return(df)
  }

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
    dplyr::mutate(cycle_shrink = c(rep(1:floor(dplyr::n() / 2), each = 2),
                                   NA)) %>%
    dplyr::mutate(cycle_ref = dplyr::lag(cycle_shrink))

  # delete extreme values that follow an NA or which are followed by an NA
  value_leadlag <- df %>%
    dplyr::mutate(value_lag = dplyr::lag(value)) %>%
    dplyr::mutate(value_lead = dplyr::lead(value)) %>%
    dplyr::select(ts, value, value_lag, value_lead)
  maxmin <- maxmin %>%
    dplyr::left_join(., value_leadlag, by = "ts") %>%
    dplyr::mutate(max_na = ifelse(is.na(value_lag), 1, 0)) %>%
    dplyr::mutate(min_na = ifelse(is.na(value_lead), 1, 0)) %>%
    dplyr::group_by(cycle_shrink) %>%
    dplyr::mutate(cycle_na = ifelse(sum(max_na, min_na) > 0, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(cycle_na == 0) %>%
    dplyr::select(ts, value, extrema, cycle_shrink, cycle_ref)

  # calculate maximum daily shrinkage (mds)
  param_mds <- maxmin %>%
    dplyr::group_by(cycle_shrink) %>%
    dplyr::mutate(date_start = substr(ts[1], 1, 10)) %>%
    dplyr::mutate(date_end = substr(ts[2], 1, 10)) %>%
    dplyr::mutate(mds = ifelse(date_start == date_end,
                               value[2] - value[1], NA)) %>%
    dplyr::mutate(date_start = as.POSIXct(date_start, tz = tz)) %>%
    # remove erroneous cycles where shrinkage is positive
    dplyr::filter(mds < 0) %>%
    dplyr::summarise(date_start = date_start[1],
                     mds = mds[1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(date_start, mds)

  # remove incomplete cycles
  maxmin <- maxmin %>%
    dplyr::mutate(cycle_ref = dplyr::lead(cycle_ref)) %>%
    dplyr::mutate(ts_ref = dplyr::lead(ts)) %>%
    dplyr::mutate(value_ref = dplyr::lead(value)) %>%
    # delete incomplete cycles
    dplyr::group_by(cycle_ref) %>%
    dplyr::mutate(n_ref = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_ref == 2) %>%
    dplyr::select(cycle = cycle_shrink, ts_shrink = ts, value_shrink = value,
                  ts_ref, value_ref)

  # calculate shrinking and refilling parameters
  param_shrink <- calccycleparam(df = df, maxmin = maxmin,
                                 mode = "shrink")
  param_ref <- calccycleparam(df = df, maxmin = maxmin, mode = "ref")

  # calculate cycle statistics according to Turcotte et al. (2009)
  param_cycle <- dplyr::full_join(param_shrink, param_ref, by = "cycle") %>%
    # remove incomplete cycles
    dplyr::filter(!is.na(shrink_start)) %>%
    dplyr::filter(!is.na(ref_start)) %>%
    dplyr::group_by(cycle) %>%
    dplyr::mutate(cycle_dur = shrink_dur + ref_dur) %>%
    dplyr::mutate(
      cycle_dur_class = cut(cycle_dur,
                            breaks = c(1, 1260, 1620, Inf),
                            labels = c("SC", "DC", "LC"))) %>%
    dplyr::mutate(mid_shrink = shrink_start +
                    as.difftime(shrink_dur / 2, units = "mins")) %>%
    dplyr::mutate(
      mid_shrink_hour = as.numeric(format(mid_shrink, format = "%H"))) %>%
    dplyr::mutate(mid_ref = ref_start +
                    as.difftime(ref_dur / 2, units = "mins")) %>%
    dplyr::mutate(
      mid_ref_hour = as.numeric(format(mid_ref, format = "%H"))) %>%
    dplyr::mutate(
      shrink_nc = ifelse(mid_shrink_hour >= 8 & mid_shrink_hour < 16,
                         1, 0)) %>%
    dplyr::mutate(
      ref_nc = ifelse(mid_ref_hour >= 16 & mid_ref_hour <= 24 |
                        mid_ref_hour >= 0 & mid_ref_hour < 8, 1, 0)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cycle_nc = sum(shrink_nc, ref_nc)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cycle_nc = ifelse(cycle_nc == 2 & cycle_dur_class != "LC",
                                    1, 0)) %>%
    dplyr::mutate(
      shrink_ic = ifelse(mid_shrink_hour >= 16 & mid_shrink_hour <= 24 |
                           mid_shrink_hour >= 0 & mid_shrink_hour < 8,
                         1, 0)) %>%
    dplyr::mutate(
      ref_ic = ifelse(mid_ref_hour >= 8 & mid_ref_hour < 16, 1, 0)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(cycle_ic = sum(shrink_ic, ref_ic)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(cycle_ic = ifelse(cycle_ic == 2 & cycle_dur_class != "LC",
                                    1, 0)) %>%
    dplyr::mutate(cycle_class = ifelse(cycle_nc == 1, "NC",
                                       ifelse(cycle_ic == 1, "IC",
                                              NA))) %>%
    # add grouping variable ts
    dplyr::mutate(ts = shrink_start) %>%
    dplyr::select(ts, cycle, shrink_start, shrink_end, shrink_dur, shrink_amp,
                  shrink_slope, ref_start, ref_end, ref_dur, ref_amp,
                  ref_slope, cycle_dur, cycle_dur_class, cycle_class)

  if (plot_cycle) {
    print("plot cycles...")
    plot_cycle(df = df, cycle = param_cycle, plot_export = plot_export)
  }

  df <- df %>%
    dplyr::full_join(., param_mds, by = c("ts" = "date_start")) %>%
    dplyr::full_join(., param_cycle, by = "ts")

  return(df)
}
