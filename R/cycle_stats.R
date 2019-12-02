#' Find Maximum and Minimum in Time Window
#'
#' \code{findmaxmin} identifies maxima and minima in a specified time window.
#'   It is a helper function of \code{\link{calccycle}}.
#'
#' @param df input \code{data.frame}
#' @param span width of the time window. Relative to \code{reso}.
#' @param st starting row of the first time window.
#' @param en ending row of the last time window.
#'
#' @keywords internal
#'
findmaxmin <- function(df, reso, st) {

  span <- 60 / reso * 6
  by <- 2 * span
  if (st != 1) {
    st <- span
  }
  en <- nrow(df)
  steps <- seq(from = st, to = en, by = by)

  max1 <- as.vector(rep(0, nrow(df)), mode = "numeric")
  min1 <- as.vector(rep(0, nrow(df)), mode = "numeric")
  for (s in 1:(length(steps) - 1)) {
    ran <- steps[s]:steps[s + 1]
    max_row <- which.max(df$value[ran]) + ran[1] - 1
    if (length(max_row) > 0) {
      max1[max_row] <- 1
    }
    min_row <- which.min(df$value[ran]) + ran[1] - 1
    if (length(min_row) > 0) {
      min1[min_row] <- 1
    }
  }

  return(list(max1, min1))
}


#' Remove Consecutive Maxima or Minima
#'
#' \code{removeconsec} removes consecutive maxima or minima and keeps only
#'   the higher or lower value, respectively. The function makes sure no two
#'   maxima or minima appear consecutively. It is a helper function of
#'   \code{\link{calccycle}}.
#'
#' @param df input \code{data.frame}.
#' @param remove numeric, in which consecutive values are removed
#'   (i.e. maxima or minima).
#' @param notremove numeric, in which consecutive values are not removed
#'   (i.e. maxima or minima).
#' @param mode character, specifies whether consecutive maxima or minima
#'   are removed. Can either be \code{"max"} or \code{"min"}.
#'
#' @keywords internal
#'
removeconsec <- function(df, remove, notremove, mode) {

  options(warn = -1)
  rem <- df %>%
    dplyr::mutate(rem = remove) %>%
    dplyr::mutate(norem = notremove) %>%
    dplyr::filter(rem == 2 | norem == 2) %>%
    # identify consecutive rem
    dplyr::mutate(rem_cons = rep(rle(rem)[[1]], times = rle(rem)[[1]])) %>%
    dplyr::mutate(iscons = ifelse(rem_cons > 1 & rem == 2, TRUE, FALSE)) %>%
    dplyr::mutate(cons = cumsum(iscons)) %>%
    dplyr::mutate(y = c(ifelse(iscons[1], 1, 0), diff(cons, lag = 1))) %>%
    dplyr::mutate(z = c(ifelse(iscons[1], 1, 0), diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(cons_nr = cumsum(z))

  rem_noconsec <- rem %>%
    dplyr::filter(iscons == FALSE & rem == 2) %>%
    dplyr::mutate(rem = value) %>%
    dplyr::select(ts, rem)

  rem_cons <- rem %>%
    dplyr::filter(rem_cons > 1 & rem == 2) %>%
    dplyr::group_by(cons_nr) %>%
    # identify rem with lowest or highest value
    dplyr::filter(value == ifelse(mode == "max", max(value, na.rm = TRUE),
                                  min(value, na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rem = value) %>%
    # select first of consecutive identical values
    dplyr::group_by(cons_nr) %>%
    dplyr::summarise(ts = ts[1],
                     rem = rem[1]) %>%
    dplyr::ungroup() %>%
    dplyr::select(ts, rem)
  options(warn = 0)

  rem_noconsec <- dplyr::bind_rows(rem_noconsec, rem_cons) %>%
    dplyr::arrange(ts)

  return(rem_noconsec)
}


#' Calculate Cycle Parameters
#'
#' \code{calccycleparam} calculates parameters for cycles of shrinking and
#'   refilling.
#'
#' @param df \code{data.frame} with dendrometer data
#' @param maxmin \code{data.frame} with minima and maxima and the respective
#'   time stamps.
#' @param mode character, specify whether statistics for shrinkage
#'   (\code{mode = "shrink"}) or refilling \code{mode = "ref"} are
#'   calculated.
#'
#' @keywords internal
#'
calccycleparam <- function(df, maxmin, mode) {

  if (mode == "shrink") {
    param <- data.frame(cycle = NA, shrink_start = NA, shrink_end = NA,
                        shrink_dur = NA, shrink_amp = NA, shrink_slope = NA)
  }
  if (mode == "ref") {
    param <- data.frame(cycle = NA, ref_start = NA, ref_end = NA,
                        ref_dur = NA, ref_amp = NA, ref_slope = NA)
  }

  maxmin <- maxmin %>%
    dplyr::select(cycle, dplyr::matches(mode))

  seq <- seq(from = 1, to = 2 * floor(nrow(maxmin) / 2), by = 2)
  list_out <- vector("list", length = length(seq))

  for (c in 1:length(seq)) {

    # select single shrinkage or refill
    cycle_start <- dplyr::pull(dplyr::select(maxmin,
                                             dplyr::matches("ts")))[seq[c]]
    cycle_end <- dplyr::pull(dplyr::select(maxmin,
                                           dplyr::matches("ts")))[seq[c] + 1]
    df_param <- df %>%
      dplyr::filter(ts >= cycle_start & ts <= cycle_end)
    param_cycle <- param
    param_cycle$cycle <- maxmin$cycle[seq[c]]

    # set cycles to NA with too much missing data
    if (length(which(is.na(df_param$value))) > 0.5 * nrow(df_param)) {
      list_out[[c]] <- param_cycle
      next
    }

    param_cycle[, 2] <- cycle_start
    param_cycle[, 3] <- cycle_end
    param_cycle[, 4] <- as.numeric(difftime(cycle_end, cycle_start,
                                            units = "mins"))
    param_cycle[, 5] <- dplyr::last(df_param$value) - df_param$value[1]
    options(warn = -1)
    param_cycle[, 6] <- summary(stats::lm(data = df_param,
                                          value ~ ts))$coefficients[2, 1]
    options(warn = 0)

    # set cycles to NA with wrong amplitude (i.e. positive for shrinkage)
    if (mode == "shrink" & param_cycle[, 5] > 0) {
      param_cycle[, 2:6] <- NA
    }
    if (mode == "ref" & param_cycle[, 5] < 0) {
      param_cycle[, 2:6] <- NA
    }

    list_out[[c]] <- param_cycle
  }
  param_all <- dplyr::bind_rows(list_out)
  return(param_all)
}


#' Calculates Cycle Statistics
#'
#' \code{calccycle} calculates different statistics and characteristics of
#'   cycles. To identify the cycles local maxima and minima are identified
#'   using a moving window.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @details \code{calccycle} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'
#' @keywords internal
#'
calccycle <- function(df, reso, tz, plot_cycle = FALSE, plot_export) {

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
