#' Find Maximum and Minimum in Time Window
#'
#' \code{findmaxmin} identifies maxima and minima in a specified time window.
#'   It is a helper function of \code{\link{phase_stats}}.
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
#'   \code{\link{phase_stats}}.
#'
#' @param df input \code{data.frame}.
#' @param remove numeric, for which consecutive values are removed
#'   (i.e. maxima or minima).
#' @param notremove numeric, for which consecutive values are not removed
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
    dplyr::filter(!(rem == 2 & norem == 2)) %>%
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
calcshrinkexpparam <- function(df, mode) {

  if (mode == "shrink") {
    group <- "shrink_group"
    col_names <- c("ts", "shrink_start", "shrink_end", "shrink_dur",
                   "shrink_amp", "shrink_slope")
  }
  if (mode == "exp") {
    group <- "exp_group"
    col_names <- c("ts", "exp_start", "exp_end", "exp_dur", "exp_amp",
                   "exp_slope")
  }

  # delete incomplete shrinkages or refillings
  maxmin_complete <- maxmin %>%
    dplyr::group_by_at(group) %>%
    dplyr::filter(dplyr::n() == 2) %>%
    dplyr::ungroup() %>%
    dplyr::rename(group = group)

  param <- df %>%
    dplyr::full_join(., maxmin_complete, by = "ts") %>%
    dplyr::mutate(fill_forw = fill_na(group)) %>%
    dplyr::mutate(fill_rev = fill_na(group, from_last = TRUE)) %>%
    dplyr::mutate(group = ifelse(fill_forw == fill_rev,
                                        fill_forw, NA)) %>%
    dplyr::filter(!is.na(group)) %>%
    # remove shrinkages or refillings with more than 50% NA values
    dplyr::group_by(group) %>%
    dplyr::mutate(value_na = length(which(is.na(value)))) %>%
    dplyr::mutate(na_prop = value_na / dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(na_prop < 0.5) %>%
    # calculate shrinkage or refilling parameters
    dplyr::group_by(group) %>%
    dplyr::mutate(start = ts[1]) %>%
    dplyr::mutate(end = dplyr::last(ts)) %>%
    dplyr::mutate(dur = as.numeric(difftime(dplyr::last(ts), ts[1],
                                            units = "mins"))) %>%
    dplyr::mutate(amp = dplyr::last(value) - value[1]) %>%
    dplyr::mutate(slope = stats::lm(value ~ ts)$coefficients[2]) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::select(ts, start, end, dur, amp, slope) %>%
    stats::setNames(col_names)

  return(param)
}
