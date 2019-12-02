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
