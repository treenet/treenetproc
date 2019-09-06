#' Helper Function for Reso
#'
#' \code{passobj} passes an object to a function call that is saved in a
#'   separate environment accessible for all functions. Environment is
#'   saved outside function call to be accessible for all functions.
#'
#' @param value name of the variable as character string.
#'
#' @keywords internal
#'
passenv <- new.env(parent = emptyenv())
passobj <- function(value) {
  val <- get(x = value, envir = passenv, inherits = FALSE)
  return(val)
}


#' Creates Flag for Gaps
#'
#' \code{creategapflag} adds a flag to gaps that are longer than
#'   \code{gaple}. Used to be called \code{cleanaftergaps}.
#'
#' @param df input \code{data.frame}.
#' @param gaple specify minimum length of gap for flagging.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
creategapflag <- function(df, reso, gaple = 12 * (60 / reso)) {

  if ("gapflag" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gapflag)
  }

  wnd <- gaple
  nc <- ncol(df)

  df <- df %>%
    dplyr::arrange(ts) %>%
    dplyr::mutate(isgap = is.na(value)) %>%
    dplyr::mutate(gaps = cumsum(isgap)) %>%
    dplyr::mutate(y = c(0, diff(gaps, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(gapnr = cumsum(z)) %>%
    dplyr::mutate(diff_ts = as.numeric(difftime(ts, dplyr::lag(ts, 1),
                                                units = "mins"))) %>%
    dplyr::mutate(diff_ts = c(0, diff_ts[2:dplyr::n()])) %>%
    dplyr::group_by(gapnr) %>%
    dplyr::mutate(gaple_mins = sum(diff_ts) + reso) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gapflag = ifelse(isgap & gaple_mins > wnd, 1, 0)) %>%
    dplyr::select(1:nc, gapflag)

  return(df)
}


#' Fills NA's With Last Non-NA Value
#'
#' \code{fillna} fills NA's with previous non-NA value (function
#'   adapted from \code{\link[zoo]{na.locf}} of the \code{zoo} package).
#'
#' @param x input \code{vector}.
#'
#' @keywords internal
#'
fill_na <- function(x) {
  nonaid <- !is.na(x)
  val_nona <- c(NA, x[nonaid])
  fillid <- cumsum(nonaid) + 1
  x <- val_nona[fillid]

  return(x)
}


#' Fills Trailing NA's With First Non-NA Value
#'
#' \code{fill_na_lead} fills leading NA's with first non-NA value.
#'
#' @param x input \code{vector}.
#'
#' @keywords internal
#'
fill_na_lead <- function(x) {
  if (is.na(x[1])) {
    nonaid <- which(!is.na(x))[1]
    val_nona <- x[nonaid]
    fillid <- 1:(nonaid - 1)
    x[fillid] <- rep(val_nona)
  }

  return(x)
}


#' Calculates Hourly Change in Data
#'
#' \code{calcdiff} calculates an hourly change in \code{value}. In case there
#'   are gaps, the difference is also calculated at an hourly rate respective
#'   to the length of the preceding gap.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
calcdiff <- function(df, reso) {
  if ("diff_val" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-diff_val)
  }

  nc <- ncol(df)
  df <- df %>%
    # calculate gaplenght in minutes
    dplyr::arrange(ts) %>%
    dplyr::mutate(isgap = is.na(value)) %>%
    dplyr::mutate(gaps = cumsum(isgap)) %>%
    dplyr::mutate(y = c(0, diff(gaps, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(gapnr = cumsum(z)) %>%
    dplyr::group_by(gapnr) %>%
    dplyr::mutate(gaple = dplyr::n()) %>%
    dplyr::mutate(gaple_mins = (gaple + 1) * reso) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gaple_mins = dplyr::lag(gaple_mins, n = 1)) %>%
    dplyr::select(1:nc, gaple_mins) %>%
    # calculate hourly change over gaps
    dplyr::mutate(diff_val = c(NA, diff(value)) * (60 / reso)) %>%
    dplyr::mutate(val_gap = fill_na(value)) %>%
    dplyr::mutate(diff_gap = c(NA, diff(val_gap))) %>%
    dplyr::mutate(diff_gap = ifelse(is.na(diff_val) & diff_gap != 0,
                  diff_gap, NA)) %>%
    dplyr::mutate(diff_gap = diff_gap / (gaple_mins / 60)) %>%
    dplyr::mutate(diff_val = ifelse(!is.na(diff_gap),
                                    diff_gap, diff_val)) %>%
    dplyr::select(1:nc, diff_val)

  return(df)
}


#' Creates Flag for Potential Frost
#'
#' \code{createfrostflag} adds a flag for potential frost. Gaps of temperature
#'   data are filled with previous non-NA value.
#'
#' @param df input \code{data.frame}.
#' @param sample_temp logical, specifying whether sample temperature dataset
#'   is used.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
createfrostflag <- function(df, tem, lowtemp = 5, sample_temp) {
  df <- tem %>%
    dplyr::mutate(frost = ifelse(value < lowtemp, TRUE, FALSE)) %>%
    dplyr::select(ts, frost) %>%
    dplyr::left_join(df, ., by = "ts") %>%
    dplyr::arrange(ts)

  # print amount of missing temperature data (not if sample temperature
  # dataset is used)
  if (!passobj("sample_temp")) {
    na_temp <- sum(is.na(df$frost))
    na_perc <- round(na_temp / nrow(df) * 100, 1)
    if (na_perc > 0.98) {
      message("No temperature data is missing.")
    } else {
      message(paste0(na_perc, "% of temperature data is missing."))
    }
  }

  df <- df %>%
    dplyr::mutate(frost = fill_na(frost))

  return(df)
}


#' Define Outliers Based on Value Distribution
#'
#' \code{calcflagmad} is a helper function of \code{\link{createflagmad}}.
#'
#' @param df input \code{data.frame}.
#' @param frost logical, defines whether outliers should be calculated for
#'   frost periods or other periods.
#' @param plot_density logical, defines whether density plots should be drawn
#'   in the console. Can be used to check outlier thresholds.
#' @param print_thresh logical, specifies whether the applied thresholds are
#'   printed to the console or not.
#' @inheritParams createflagmad
#'
#' @keywords internal
#'
calcflagmad <- function(df, reso, wnd = NULL, tol = 9, frost,
                        plot_density = FALSE, print_thresh = FALSE) {

  check_logical(var = frost, var_name = "frost")
  check_logical(var = print_thresh, var_name = "print_thresh")
  if (frost) {
    df <- df %>%
      dplyr::filter(frost == TRUE)
  }
  if (!frost) {
    df <- df %>%
      dplyr::filter(frost == FALSE)
  }

  if (nrow(df) == 0) {
    return(df)
  }

  if (length(wnd) == 0) {
    span <- trunc(nrow(df) / 2)
    } else {
      span <- 60 / reso * 24 * (wnd / 2)
      if (nrow(df) < 2 * span) {
        message("you don't have enough data for regular outlier detection! ",
                "Outlier detection may not work properly.")
        span <- trunc(nrow(df) / 2)
      }
    }

  st <- span + 1
  en <- nrow(df) - span + 1
  steps <- seq(st, en, by = span)

  flagqlow <- vector(length = nrow(df))
  flagqhigh <- vector(length = nrow(df))
  thresh_min <- -100000
  thresh_max <- 100000
  for (qq in steps) {
    b1 <- qq - span; b2 <- qq + span - 1; ran <- b1:b2
    q40 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.4,
                                      na.rm = TRUE))
    q60 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.6,
                                      na.rm = TRUE))
    df$diff_val[ran][df$diff_val[ran] > q40 &
                       df$diff_val[ran] < q60] <- NA

    q1 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.25,
                                     na.rm = TRUE))
    q3 <- as.numeric(stats::quantile(df$diff_val[ran], probs = 0.75,
                                     na.rm = TRUE))

    mad <- stats::mad(df$diff_val[ran], na.rm = TRUE)
    low <- q1 - tol * mad
    high <- q3 + tol * mad

    if (frost) {
      low <- low * 15
      high <- high * 15
    }

    if (plot_density) {
      graphics::plot(stats::density(x = df$diff_val[ran], na.rm = T),
                     main = df$ts[ran][1])
      graphics::abline(v = low, col = "red")
      graphics::abline(v = high, col = "red")
    }

    if (print_thresh) {
      if (!is.na(low) && low != 0) {
        if (low > thresh_min){
          thresh_min <- round(low, 2)
        }
      }
      if (!is.na(high) && high != 0) {
        if (high < thresh_max) {
          thresh_max <- round(high, 2)
        }
      }
    }

    if (!is.na(low)) {
      flagqlow[ran][df$diff_val[ran] < low] <- TRUE
    }
    if (!is.na(high)) {
      flagqhigh[ran][df$diff_val[ran] > high] <- TRUE
    }
  }

  df <- df %>%
    dplyr::mutate(flagoutlow = flagqlow) %>%
    dplyr::mutate(flagouthigh = flagqhigh)

  if (print_thresh) {
    message(paste0(df$series[1], " threshold low: ", thresh_min,
                   "; high: ", thresh_max))
  }

  return(df)
}


#' Creates Flag for Outliers Based on Median Absolute Deviation
#'
#' \code{createflagmad} creates flag for outliers that are above or below a
#'   threshold distant from the first or third quantile. The threshold is
#'   specified by the the median absolute deviation
#'   (\code{\link[stats]{mad}}).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#' @inheritParams calcflagmad
#'
#' @keywords internal
#'
createflagmad <- function(df, reso, wnd, tol, plot_density, print_thresh) {

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(diff_val = ifelse(diff_val < 0.001 & diff_val > -0.001,
                                    NA, diff_val))

  df_frost <- calcflagmad(df = df, reso = reso, wnd = wnd, tol = tol,
                          frost = TRUE, plot_density = FALSE)
  df <- calcflagmad(df = df, reso = reso, wnd = wnd, tol = tol,
                    frost = FALSE, plot_density = FALSE,
                    print_thresh = print_thresh)

  if (nrow(df_frost) > 0) {
    df <- dplyr::bind_rows(df, df_frost)
  }

  df <- df %>%
    dplyr::arrange(ts) %>%
    dplyr::select(1:nc, flagoutlow, flagouthigh)

  return(df)
}


#' Create Flag for Outliers
#'
#' \code{createflagout} creates flags for outlier values.
#'
#' @param df input \code{data.frame}.
#' @param flag name of column to which flagging is applied. One of
#'   \code{c("flagoutlow", "flagouthigh")} generated by
#'   \code{\link{createflagmad}}.
#' @param len specifies the minimal number of consecutive times the value
#'   difference threshold has to be exceeded for flagging.
#'
#' @keywords internal
#'
createflagout <- function(df, flag, len) {

  flagout <- df %>%
    dplyr::select(flag = grep(paste0("^", flag, "$"), colnames(.))) %>%
    dplyr::mutate(flag_group = cumsum(flag)) %>%
    dplyr::mutate(y = c(0, diff(flag_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flag_nr = cumsum(z)) %>%
    dplyr::group_by(flag_nr) %>%
    dplyr::mutate(flag_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flagout = ifelse(flag_le >= len & flag, TRUE, FALSE)) %>%
    dplyr::select(flagout) %>%
    unlist()

  return(flagout)
}


#' Remove Outliers
#'
#' \code{executeflagout} removes outlier values flagged by
#'   \code{\link{createflagout}}.
#'
#' @param df input \code{data.frame}.
#' @inheritParams createflagout
#'
#' @keywords internal
#'
executeflagout <- function(df, len) {

  nc <- ncol(df)
  flagout_nr <- length(grep("^flagout[0-9]", colnames(df)))
  if (flagout_nr > 0) {
    passenv$flagout_nr <-  passenv$flagout_nr + 1
  } else {
    passenv$flagout_nr <- 1
  }

  out_low <- createflagout(df = df, flag = "flagoutlow", len = len)
  out_high <- createflagout(df = df, flag = "flagouthigh", len = len)

  out <- out_low + out_high
  out <- ifelse(out > 0, TRUE, FALSE)

  df <- df %>%
    dplyr::mutate(flagout = out) %>%
    dplyr::mutate(!!paste0("flagout", passobj("flagout_nr")) := flagout) %>%
    dplyr::mutate(value = ifelse(flagout, NA, value)) %>%
    dplyr::select(1:nc, grep("^flagout[0-9]", colnames(.)))

  return(df)
}


#' Anomalize Dendrometer Differences
#'
#' \code{anomalizeseries} uses the package \code{\link{anomalize}} to
#'   detect outliers and data anomalies. The anomalies are calculated
#'   for the differences between two timesteps instead of the actual
#'   dendrometer values. In periods of probable frost (i.e. temperature <
#'   \code{lowtemp}), the thresholds to classify outliers are multiplied by
#'   \code{10}.
#'
#' @param df input \code{data.frame}.
#' @param method_decompose character, the time series decomposition method.
#'   For further information see description in
#'   \code{\link[anomalize]{time_decompose}}.
#' @param alpha numeric, controls the width of the "normal" range. For further
#'   information see description in \code{\link[anomalize]{anomalize}}.
#' @param method_anomalize character, the anomaly detection method. For
#'   further information see description in
#'   \code{\link[anomalize]{anomalize}}.
#' @param max_anoms numeric, the maximum percent of permitted anomalies.
#'   For further information see description in
#'   \code{\link[anomalize]{anomalize}}.
#' @param print_thresh logical, specifies whether the applied thresholds are
#'   printed to the console or not.
#'
#' @return a \code{vector} of timestamps (\code{POSIXct}) in which
#'   an anomaly was detected.
#'
#' @keywords internal
#'
anomalizeseries <- function(df, method = NULL, method_decompose = "stl",
                            alpha = NULL, method_anomalize = "iqr",
                            max_anoms = 0.2, frost_thresh = 10,
                            print_thresh = TRUE,
                            correction = NULL) {

  check_logical(var = print_thresh, var_name = "print_thresh")
  method <- check_method(var = method, var_name = "method")
  if (!(correction %in% c("outlier", "jump"))) {
      stop("correction needs to be either 'outlier' or 'jump'.")
  }
  if (length(alpha) == 0) {
    stop("please provide a value for 'alpha'.")
  }

  frost <- df %>%
    dplyr::filter(!is.na(!!method)) %>%
    dplyr::select(frost) %>%
    unlist(., use.names = FALSE)

  anomalize <- df %>%
    tibble::as_tibble() %>%
    dplyr::filter(!is.na(!!method)) %>%
    dplyr::arrange(ts) %>%
    anomalize::time_decompose(., target = !!method,
                              method = method_decompose) %>%
    anomalize::anomalize(target = remainder, method = method_anomalize,
                         alpha = alpha, max_anoms = max_anoms) %>%
    anomalize::time_recompose()

  if (print_thresh) {
    if (method == "diff_val") {
      thresh_low_med <- round(median(anomalize$recomposed_l1, na.rm = TRUE), 1)
      thresh_high_med <- round(median(anomalize$recomposed_l2, na.rm = TRUE), 1)
    }
    if (method == "value") {
      conf_band <- round(median(anomalize$recomposed_l2 -
                                  anomalize$recomposed_l1, na.rm = TRUE), 1)
    }
  }

  anomalize_frost <- anomalize %>%
    dplyr::mutate(frost = frost) %>%
    dplyr::mutate(low_thresh =
                    ifelse(frost, recomposed_l1 * frost_thresh, recomposed_l1)) %>%
    dplyr::mutate(high_thresh =
                    ifelse(frost, recomposed_l2 * frost_thresh, recomposed_l2)) %>%
    dplyr::mutate(
      anomaly = ifelse(observed < recomposed_l1 | observed > recomposed_l2,
                       TRUE, FALSE)) %>%
    dplyr::filter(anomaly == TRUE) %>%
    dplyr::select(ts)

  if (print_thresh) {
    if (method == "diff_val") {
      message(paste0(df$series[1], " ", correction, " threshold low: ",
                     thresh_low_med, "; high: ", thresh_high_med))
    }
    if (method == "value") {
      message(paste0(df$series[1], " ", correction, " confidence band: ",
                     conf_band))
    }
  }

  return(anomalize_frost)
}


#' Create Flag with Anomalize
#'
#' \code{createanomalyflag} flags outliers classified with the package
#'   \code{\link{anomalize}}.
#'
#' @param df input \code{data.frame}.
#' @inheritParams anomalizeseries
#'
#' @keywords internal
#'
createanomalyflag <- function(df, method, alpha = 0.05, print_thresh,
                              correction) {

  if (method == "both") {
    anomalize_diff_val <- anomalizeseries(df = df, method = "diff_val",
                                          alpha = alpha,
                                          print_thresh = print_thresh,
                                          correction = correction)
    anomalize_value <- anomalizeseries(df = df, method = "value",
                                       alpha = alpha,
                                       print_thresh = FALSE,
                                       correction = correction)
    anomalize <- dplyr::intersect(anomalize_diff_val, anomalize_value)
  }

  if (method %in% c("value", "diff_val")) {
    anomalize <- anomalizeseries(df = df, method = method,
                                 alpha = alpha,
                                 print_thresh = print_thresh,
                                 correction = correction)
  }

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(anomaly = ifelse(ts %in% anomalize$ts, TRUE, FALSE)) %>%
    dplyr::select(1:nc, anomaly)

  return(df)
}


#' Remove Outliers Classified with Anomalize
#'
#' \code{removeoutliers} removes outliers flagged with the package
#'   \code{\link{anomalize}}. To differentiate outlier points from jumps in
#'   the data, outlier points are only classified if they occur in groups of
#'   a specified minimum length (defined by \code{len}).
#'
#' @param df input \code{data.frame}.
#' @param len numeric, specifies the minimal number of consecutive outliers
#'   that is needed for removal (used to differentiate outliers from jumps).
#'
#' @keywords internal
#'
removeoutliers <- function(df, len) {

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(flag_group = cumsum(anomaly)) %>%
    dplyr::mutate(y = c(0, diff(flag_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flag_nr = cumsum(z)) %>%
    dplyr::group_by(flag_nr) %>%
    dplyr::mutate(flag_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flagout = ifelse(flag_le >= len & anomaly,
                                   TRUE, FALSE)) %>%
    dplyr::mutate(value = ifelse(flagout, NA, value)) %>%
    dplyr::mutate(anomaly = ifelse(is.na(value), FALSE, anomaly)) %>%
    dplyr::select(1:nc, flagout)

  return(df)
}


#' Creates Flag for Jumps in Data
#'
#' \code{createjumpflag_anomalize} creates flag for jumps.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
createjumpflag_anomalize <- function(df) {

  flagjump_nr <- length(grep("^flagjump[0-9]", colnames(df)))
  if (flagjump_nr > 0) {
    passenv$flagjump_nr <-  passenv$flagjump_nr + 1
  } else {
    passenv$flagjump_nr <- 1
  }

  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(flag_group = cumsum(anomaly)) %>%
    dplyr::mutate(y = c(0, diff(flag_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flag_nr = cumsum(z)) %>%
    dplyr::group_by(flag_nr) %>%
    dplyr::mutate(flag_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(flagjump = ifelse(flag_le == 1 & anomaly,
                                    TRUE, FALSE)) %>%
    dplyr::mutate(!!paste0("flagjump", passobj("flagjump_nr")) := flagjump) %>%
    dplyr::select(1:nc, grep("^(flagjump)", names(.), perl = TRUE)) %>%
    as.data.frame()

  return(df)
}


#' Remove Jumps
#'
#' \code{executejump_anomalize} removes jumps that occur after resetting the
#'   dendrometer needle.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
executejump_anomalize <- function(df) {

  nc <- ncol(df)
  le <- nrow(df)
  ran <- which(df$flagjump)

  diff_jump <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff_jump = c(NA, diff(value))) %>%
    dplyr::select(ts, diff_jump)
  df <- dplyr::left_join(df, diff_jump, by = "ts")

  val <- as.vector(df$value, mode = "numeric")
  if (length(ran) > 0) {
    for (uu in 1:length(ran)) {
      zz <- ran[uu]
      dx <- df$gapflag[(zz - 1)]
      dx <- dx + 1
      val[zz:le] <- val[zz:le] - (df$diff_jump[zz] * dx)
    }
  }

  df <- df %>%
    dplyr::mutate(value = val) %>%
    dplyr::select(1:nc)

  return(df)
}


#' Creates Flag for Jumps and Outliers in Data
#'
#' \code{createjumpflag} creates flag for jumps and ouliers. Outliers can be
#'   single points or errors before, after or in between jumps.
#'
#' @param df input \code{data.frame}.
#' @param thr specifies the threshold to discriminate between outliers and
#'     jumps due to adjustments of the dendrometer needle.
#'
#' @keywords internal
#'
createjumpflag <- function(df, thr = 0.2, anomalize) {

  check_logical(var = anomalize, var_name = "anomalize")

  nc <- ncol(df)
  flagjump_nr <- length(grep("^flagjump[0-9]", colnames(df)))
  if (flagjump_nr > 0) {
    passenv$flagjump_nr <-  passenv$flagjump_nr + 1
  } else {
    passenv$flagjump_nr <- 1
  }

  if (anomalize) {
    ran <- which(df$anomaly)
  } else {
    ran <- which(df$flagoutlow | df$flagouthigh)
  }
  outlier <- as.vector(rep(FALSE, nrow(df)), mode = "logical")
  jump <- as.vector(rep(FALSE, nrow(df)), mode = "logical")
  if (length(ran) != 0) {
    for (iu in ran) {
      hhj <- sum(df$diff_val[(iu + 1):(iu + 3)], na.rm = TRUE)
      if (abs(df$diff_val[iu] + hhj) < df$diff_val[iu] * thr) {
        outlier[iu] <- TRUE
      } else {
        jump[iu] <- TRUE
      }
    }
  }

  df <- df %>%
    dplyr::mutate(flagjump = jump) %>%
    dplyr::mutate(!!paste0("flagjump", passobj("flagjump_nr")) := flagjump) %>%
    dplyr::mutate(flagjumpout = outlier) %>%
    dplyr::mutate(
      !!paste0("flagjumpout", passobj("flagjump_nr")) := flagjumpout) %>%
    dplyr::select(1:nc, grep("^(flagjumpout|flagjump)(?!.*low)(?!.*high)",
                             names(.), perl = TRUE)) %>%
    as.data.frame()

  return(df)
}


#' Remove Jumps and Outliers
#'
#' \code{executejump} removes jumps that occur after resetting the dendrometer
#'   needle and removes outliers.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
executejump <- function(df) {

  nc <- ncol(df)
  le <- nrow(df)
  ran <- which(df$flagjump)

  diff_jump <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff_jump = c(NA, diff(value))) %>%
    dplyr::select(ts, diff_jump)
  df <- dplyr::left_join(df, diff_jump, by = "ts")

  val <- as.vector(df$value, mode = "numeric")
  if (length(ran) > 0) {
    for (uu in 1:length(ran)) {
      zz <- ran[uu]
      dx <- df$gapflag[(zz - 1)]
      dx <- dx + 1
      val[zz:le] <- val[zz:le] - (df$diff_jump[zz] * dx)
    }
  }

  df <- df %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(value = ifelse(flagjumpout, NA, value)) %>%
    dplyr::select(1:nc)

  return(df)
}


#' Calculate Maximum
#'
#' \code{calcmax} calculates the maximum of measured values.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
calcmax <- function(df) {

  if ("max" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-max)
  }
  nc <- ncol(df)
  first_val <- df$value[which(!is.na(df$value))[1]]

  df <- df %>%
    dplyr::mutate(val_nona = fill_na_lead(value)) %>%
    dplyr::mutate(val_nona = fill_na(val_nona)) %>%
    dplyr::mutate(diff_nona = c(0, diff(val_nona, lag = 1))) %>%
    dplyr::mutate(diff_sum = cumsum(diff_nona)) %>%
    dplyr::mutate(diff_sum = fill_na(diff_sum))

  max_sum <- df$diff_sum
  for (i in 2:nrow(df)) {
    if (max_sum[i - 1] < max_sum[i]) {
      next
    } else {
      max_sum[i] <- max_sum[i - 1]
    }
  }

  df <- df %>%
    dplyr::mutate(max = max_sum + first_val) %>%
    dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
    dplyr::select(1:nc, max)

  return(df)
}


#' Calculate TWD and GRO
#'
#' \code{calctwdgro} calculates the tree water deficit (twd) and the growth
#'   since the beginning of the year (gro_yr).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
calctwdgro  <- function(df, tz) {

  if ("twd" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-twd)
  }
  if ("gro_yr" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_yr)
  }
  nc <- ncol(df)
  df <- df %>%
    dplyr::mutate(twd = abs(value - max)) %>%
    dplyr::mutate(gro = c(0, diff(max))) %>%
    dplyr::mutate(gro = ifelse(is.na(gro), 0, gro)) %>%
    dplyr::mutate(year = substr(ts, 1, 4)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(gro_yr = cumsum(gro)) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:nc, twd, gro_yr)

  return(df)
}


#' Find Maximum and Minimum in Time Window
#'
#' \code{findmaxmin} identifies maxima and minima in a specified time window.
#'   It is a helper function of \code{\link{calcmds}}.
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
#'   \code{\link{calcmds}}.
#'
#' @param df input \code{data.frame}.
#' @param remove numeric, in which consecutive values are removed
#'   (i.e. maxima or minima).
#' @param notremove numeric, in which consecutive values are not removed
#'   (i.e. maxima or minima).
#'
#' @keywords internal
#'
removeconsec <- function(df, remove, notremove) {

  options(warn = -1)
  rem <- df %>%
    dplyr::mutate(rem = remove) %>%
    dplyr::mutate(norem = notremove) %>%
    dplyr::filter(rem == 2 | norem == 2) %>%
    dplyr::mutate(rem2 = rep(rle(rem)[[1]], times = rle(rem)[[1]])) %>%
    dplyr::mutate(rem = ifelse(rem == 2, value, NA)) %>%
    dplyr::mutate(iscons = ifelse(rem2 > 1 & !is.na(rem), TRUE, FALSE)) %>%
    dplyr::mutate(cons = cumsum(iscons)) %>%
    dplyr::mutate(y = c(0, diff(cons, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(cons_nr = cumsum(z)) %>%
    dplyr::group_by(cons_nr) %>%
    dplyr::mutate(rem2 = max(rem, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(rem = dplyr::case_when(iscons & rem == rem2 ~ rem,
                                         !iscons ~ rem)) %>%
    dplyr::group_by(cons_nr) %>%
    dplyr::mutate(rem_cons = length(unique(rem))) %>%
    dplyr::ungroup()
  options(warn = 0)

  rem_noconsec <- rem %>%
    dplyr::filter(!(iscons == TRUE & rem_cons == 1)) %>%
    dplyr::select(ts, rem)

  rem_cons <- rem %>%
    dplyr::filter(iscons == TRUE & rem_cons == 1)

  # select first of consecutive identical values
  if (nrow(rem_cons) > 0) {
    rem_cons <- rem_cons %>%
      dplyr::group_by(cons_nr, rem_cons) %>%
      dplyr::summarise(ts = ts[1],
                       rem = rem[1]) %>%
      dplyr::ungroup() %>%
      dplyr::select(ts, rem)

    rem_noconsec <-
      dplyr::full_join(rem_noconsec, rem_cons, by = c("ts", "rem"))
  }

  rem_noconsec <- rem_noconsec %>%
    dplyr::filter(!(is.na(rem)))

  return(rem_noconsec)
}


#' Calculate Maximum Daily Shrinkage (MDS)
#'
#' \code{calcmds} calculates the maximum daily shrinkage (\code{mds}).
#'   \code{mds} is defined as the largest daily shrinkage. First, local
#'   maxima and minima are identified using a moving window. \code{mds} is
#'   only calculated if a local maximum occurs before a local minimum
#'   (i.e. if the stem shrinks during the day).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @details \code{calcmds} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'
#' @keywords internal
#'
calcmds <- function(df, reso, tz, plot_mds = FALSE, plot_export) {

  if ("mds" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-mds)
  }
  nc <- ncol(df)

  maxmin1 <- findmaxmin(df = df, reso = reso, st = 1)
  maxmin2 <- findmaxmin(df = df, reso = reso, st = 2)

  max_wnd <- maxmin1[[1]] + maxmin2[[1]]
  min_wnd <- maxmin1[[2]] + maxmin2[[2]]

  max1 <- removeconsec(df = df, remove = max_wnd, notremove = min_wnd) %>%
    dplyr::mutate(max1 = rem) %>%
    dplyr::select(-rem)
  min1 <- removeconsec(df = df, remove = min_wnd, notremove = max_wnd) %>%
    dplyr::mutate(min1 = rem) %>%
    dplyr::select(-rem)

  maxmin <- dplyr::full_join(max1, min1, by = "ts") %>%
    dplyr::arrange(ts)

  if (plot_mds) {
    print("plot mds...")
    plot_mds(df = df, maxmin = maxmin, plot_export = plot_export)
  }

  maxmin <- maxmin %>%
    dplyr::mutate(min_lag = dplyr::lead(min1, n = 1)) %>%
    dplyr::mutate(day = as.POSIXct(substr(ts, 1, 10), format = "%Y-%m-%d",
                                   tz = tz)) %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(mds = max1 - min_lag) %>%
    dplyr::filter(!is.na(mds)) %>%
    dplyr::mutate(mds = max(mds, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mds = ifelse(mds >= 0, mds, NA)) %>%
    dplyr::group_by(day) %>%
    dplyr::summarise(mds = mds[1]) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(day = as.POSIXct(substr(ts, 1, 10), format = "%Y-%m-%d",
                                   tz = tz)) %>%
    dplyr::full_join(., maxmin, by = "day") %>%
    dplyr::arrange(ts) %>%
    dplyr::select(1:nc, mds)

  return(df)
}


#' Calculate Start and End of Yearly Growth
#'
#' \code{grostartend} returns the day of the year at which growth starts
#'   or ends.
#'
#' @param df input \code{data.frame}.
#' @param tol numeric, defines the amount of yearly growth that needs to be
#'   surpassed for \code{gro_start} to be defined. \code{1 - tol} is the
#'   amount of yearly growth at which \code{gro_end} is defined.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
grostartend <- function(df, tol = 0.05, tz) {

  nc <- ncol(df)

  df <- df %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(gro_tot = sum(gro_yr, na.rm = T)) %>%
    dplyr::mutate(gro_start_tol = tol * gro_tot) %>%
    dplyr::mutate(gro_end_tol = (1 - tol) * gro_tot) %>%
    dplyr::mutate(gro_sum = cumsum(ifelse(is.na(gro_yr), 0, gro_yr))) %>%
    dplyr::mutate(
      gro_start_ind = dplyr::first(which(gro_sum <= gro_start_tol))) %>%
    dplyr::mutate(
      gro_start = as.numeric(strftime(ts[gro_start_ind], format = "%j"))) %>%
    dplyr::mutate(
      gro_end_ind = dplyr::first(which(gro_sum >= gro_end_tol))) %>%
    dplyr::mutate(
      gro_end = as.numeric(strftime(ts[gro_end_ind], format = "%j"))) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:nc, gro_start, gro_end)

  return(df)
}


#' Summarise Flags
#'
#' \code{summariseflags} summarises all previously created flags in one column.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
summariseflags <- function(df, jump_corr) {

  list_flags <- vector("list", length = passenv$flagjump_nr)

  n_flags <- 1
  #for (out in n_flags:(passenv$flagout_nr + n_flags - 1)) {
  #  flagout_nr <- out - n_flags + 1
  #  flagout <- df[[paste0("flagout", flagout_nr)]]
  #  list_flags[[out]] <- ifelse(flagout, paste0("out", flagout_nr), NA)
  #}
  if (jump_corr) {
    #n_flags <- n_flags + passenv$flagout_nr
    for (out in n_flags:(passenv$flagjump_nr + n_flags - 1)) {
      flagjump_nr <- out - n_flags + 1
      flagjumpout <- df[[paste0("flagjumpout", flagjump_nr)]]
      list_flags[[out]] <- ifelse(flagjumpout, paste0("jumpout", flagjump_nr), NA)
    }
    n_flags <- n_flags + passenv$flagjump_nr
    for (jump in n_flags:(passenv$flagjump_nr + n_flags - 1)) {
      flagjump_nr <- jump - n_flags + 1
      flagjump <- df[[paste0("flagjump", flagjump_nr)]]
      list_flags[[jump]] <- ifelse(flagjump, paste0("jump", flagjump_nr), NA)
    }
  }
  if ("flagfill" %in% colnames(df)) {
    n_flags <- n_flags + 1
    list_flags[[n_flags]] <- ifelse(df$flagfill, "fill", NA)
  }

  flags <- do.call("paste", c(list_flags, sep = ", "))
  flags <- gsub(", NA", "", flags)
  flags <- gsub("NA, ", "", flags)
  flags <- ifelse(flags == "NA", NA, flags)

  df$flags <- flags

  return(df)
}
