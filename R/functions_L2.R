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
#' @examples
#'
passenv <- new.env(parent = emptyenv())
passobj <- function(value) {
  val <- get(x = value, envir = passenv, inherits = FALSE)
  return(val)
}


#' Remove Outliers
#'
#' \code{cleanoutofrange} removes implausible data points lower or higher
#'   than specified values in \code{val_range}.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
cleanoutofrange <- function(df, val_range) {
  df <- df %>%
    dplyr::mutate(flagrangemin = ifelse(!is.na(value) & value < val_range[1],
                                        TRUE, FALSE)) %>%
    dplyr::mutate(flagrangemax = ifelse(!is.na(value) & value > val_range[2],
                                        TRUE, FALSE)) %>%
    dplyr::mutate(value = ifelse(flagrangemin, NA, value)) %>%
    dplyr::mutate(value = ifelse(flagrangemax, NA, value))

  return(df)
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
#' @examples
#'
creategapflag <- function(df, reso, gaple = 12 * (60 / reso)) {
  # This function is Copyright
  if ("gapflag" %in% colnames(df)) {
    df <- dplyr::select(df, -gapflag)
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
#' \code{fillna} fills rows with NA with previous non-NA value (function
#'   adapted from \code{na.locf} of the \code{zoo} package).
#'
#' @param x input \code{vector}.
#'
#' @keywords internal
#'
#' @examples
#'
fill_na <- function(x) {
  nonaid <- !is.na(x)
  val_nona <- c(NA, x[nonaid])
  fillid <- cumsum(nonaid) + 1
  x <- val_nona[fillid]

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
#' @examples
#'
calcdiff <- function(df, reso) {
  if ("diff_val" %in% colnames(df)) {
    df <- dplyr::select(df, -diff_val)
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
#' \code{createfrostflag} adds a flag for potential frost.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
createfrostflag <- function(df, tem, lowtemp = 5) {
  df <- tem %>%
    dplyr::mutate(frost = ifelse(value < lowtemp, TRUE, FALSE)) %>%
    dplyr::select(-value, -series, -version) %>%
    dplyr::left_join(df, ., by = "ts") %>%
    dplyr::arrange(ts)

  na_temp <- sum(is.na(df$frost))
  if (na_temp > (0.5 * nrow(df))) {
    na_perc <- round(na_temp / nrow(df) * 100, 1)
    message(paste0(na_perc, "% of temperature data is missing!"))
  }

  return(df)
}


#' Creates Flag for Outliers Based on Median Absolute Deviation
#'
#' \code{createflagout} creates flag for outliers that are above or below a
#'   specified distance from the first or third quartile.
#'
#' @param df input \code{data.frame}.
#' @param wnd length of time window for which interquartile ranges are
#'   calculated. A second longer time window (\code{wnd * 5}) is automatically
#'   added. Unit of \code{wnd} is days.
#' @param n_mad number of mad (median absolute deviation) distances from the
#'   first and third quartile that are still trusted. \code{n_mad} is
#'   increased to \code{n_mad * 10} if there is a probability of frost.
#' @param tol tolerance added to iqr (interquartile rnage) to avoid too
#'   stringent error flagging. Unit of \code{tol} is \code{µm}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
createflagout <- function(df, reso, wnd = 3, n_mad = 5) {

  #df <- L1_removeoutliers[100000:150000, ]
  #reso <- 10
  #wnd <- 6
  #n_mad <- 5

  df <- df %>%
    dplyr::mutate(diff_val = ifelse(diff_val < 0.001 & diff_val > -0.001,
                                    NA, diff_val))
  nc <- ncol(df)
  wnds <- c(wnd, wnd * 5)
  for (w in 1:length(wnds)) {
    span <- 60 / reso * 24 * (wnds[w] / 2)

    if (nrow(df) < 2 * span) {
      message("you don't have enough data for window flag!")
      next
    }
    st <- span + 1
    en <- nrow(df) - span + 1
    steps <- seq(st, en, by = span)

    if (length(steps) == 0) {
      stop("something went terribly wrong with the timestamp.")
    }

    flagqlow <- vector(length = nrow(df))
    flagqhigh <- vector(length = nrow(df))
    for (qq in steps) {
      b1 <- qq - span; b2 <- qq + span - 1; ran <- b1:b2
      q40 <- as.numeric(quantile(df$diff_val[ran], probs = 0.4,
                                 na.rm = TRUE))
      q60 <- as.numeric(quantile(df$diff_val[ran], probs = 0.6,
                                 na.rm = TRUE))

      df$diff_val[ran][df$diff_val[ran] > q40 &
                         df$diff_val[ran] < q60] <- NA
      q1 <- as.numeric(quantile(df$diff_val[ran], probs = 0.25,
                                na.rm = TRUE))
      q3 <- as.numeric(quantile(df$diff_val[ran], probs = 0.75,
                                na.rm = TRUE))

      mad <- mad(df$diff_val[ran], na.rm = TRUE)
      low <- q1 - n_mad * mad
      low_frost <- q1 - n_mad * 10 * mad
      high <- q3 + n_mad * mad
      high_frost <- q3 + n_mad * 10 * mad

      #plot(density(x = df$diff_val[ran], na.rm = T), main = df$ts[ran][1])
      #abline(v = low, col = "red")
      #abline(v = high, col = "red")


      if (!is.na(low)) {
          flagqlow[ran][df$diff_val[ran] <
                          ifelse(df$frost[ran], low_frost, low)] <- TRUE
      }
      if (!is.na(high)) {
        flagqhigh[ran][df$diff_val[ran] >
                         ifelse(df$frost[ran], high_frost, high)] <- TRUE
      }
    }

    flagq_nr <- length(grep("flagqlow", colnames(df)))
    if (flagq_nr == 0) {
      flagq_nr <- 1
    } else {
      flagq_nr <- flagq_nr + 1
    }

    df <- df %>%
      dplyr::mutate(!!paste0("flagqlow", flagq_nr) := flagqlow) %>%
      dplyr::mutate(!!paste0("flagqhigh", flagq_nr) := flagqhigh)
  }

  df <- df %>%
    dplyr::mutate(flagqlow = rowSums(.[grep("flagqlow", names(.))])) %>%
    dplyr::mutate(flagqhigh = rowSums(.[grep("flagqhigh", names(.))])) %>%
    dplyr::mutate(
      flagoutlow = ifelse(flagqlow == flagq_nr, TRUE, FALSE)) %>%
    dplyr::mutate(
      flagouthigh = ifelse(flagqhigh == flagq_nr, TRUE, FALSE)) %>%
    dplyr::select(1:nc, flagoutlow, flagouthigh)

  message("createflagout is in development mode.")

  return(df)
}


#' Removes Outliers Defined Based on Median Absolute Deviation
#'
#' \code{executeflagout} removes outliers flagged by \code{createflagout}.
#'
#' @param df input \{data.frame}.
#' @param len minimal number of consecutive outliers for removal.
#'
#' @keywords internal
#'
#' @examples
#'
executeflagout <- function(df, len) {

  nc <- ncol(df)
  flagoutlow_pos <-
    as.numeric(which(names(df) ==
                       paste0("flagoutlow", passobj("flagout_nr"))))
  flagouthigh_pos <-
    as.numeric(which(names(df) ==
                       paste0("flagouthigh", passobj("flagout_nr"))))
  df$flagoutlow <- df[, flagoutlow_pos]
  df$flagouthigh <- df[, flagouthigh_pos]

  df <- df %>%
    # remove flagoutlow
    dplyr::mutate(flagoutlow_group = cumsum(flagoutlow)) %>%
    dplyr::mutate(y = c(0, diff(flagoutlow_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flagoutlow_nr = cumsum(z)) %>%
    dplyr::group_by(flagoutlow_nr) %>%
    dplyr::mutate(flagoutlow_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = ifelse(flagoutlow_le >= len & flagoutlow, NA, value)) %>%
    # remove flagouthigh
    dplyr::mutate(flagouthigh_group = cumsum(flagouthigh)) %>%
    dplyr::mutate(y = c(0, diff(flagouthigh_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flagouthigh_nr = cumsum(z)) %>%
    dplyr::group_by(flagouthigh_nr) %>%
    dplyr::mutate(flagouthigh_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = ifelse(flagouthigh_le >= len & flagouthigh, NA, value)) %>%
    dplyr::select(1:nc)
}


#' Create Flag for Abrubt Changes in Data
#'
#' \code{createflagdiff} creates a flag for large differences in value.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
createflagdiff <- function(df, reso, diffwin = 1000, diffsum = 150) {
  # This function is Copyright
  flagdiff_nr <- length(grep("flagdiff", colnames(df)))
  if (flagdiff_nr > 0) {
    df <- dplyr::select(df, -flagdiff)
  } else {
    flagdiff_nr <- 1
  }

  df <- df %>%
    dplyr::mutate(flagdiff = dplyr::case_when(
      abs(diff_val) > diffsum / (60 / reso) & !frost ~ TRUE,
      abs(diff_val) > diffwin / (60 / reso) & frost ~ TRUE)) %>%
    dplyr::mutate(flagdiff = ifelse(is.na(flagdiff), FALSE, flagdiff)) %>%
    dplyr::mutate(!!paste0("flagdiff", flagdiff_nr) := flagdiff)

  return(df)
}


#' Remove Values With Diff Flag
#'
#' \code{executeflagdiff} removes consecutive values with large differences.
#'
#' @param df input \code{data.frame}.
#' @param length specifies the minimal number of consecutive flags above which
#'   values are overwritten with NA.
#'
#' @keywords internal
#'
#' @examples
#'
executeflagdiff <- function(df, length = 2) {
  # This function is Copyright
  wnd <- length
  nc <- ncol(df)

  df <- df %>%
    dplyr::mutate(flagdiff_group = cumsum(flagdiff)) %>%
    dplyr::mutate(y = c(0, diff(flagdiff_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(flagdiff_nr = cumsum(z)) %>%
    dplyr::group_by(flagdiff_nr) %>%
    dplyr::mutate(flagdiff_le = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = ifelse(flagdiff_le >= wnd & flagdiff, NA, value)) %>%
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
#' @examples
#'
createjumpflag <- function(df, thr = 0.2) {

  nc <- ncol(df)
  flagout_nr <- length(grep("^flagout[0-9]", colnames(df)))
  if (flagout_nr > 0) {
    passenv$flagout_nr <-  passenv$flagout_nr + 1
  } else {
    passenv$flagout_nr <- 1
  }

  ran <- which(df$flagoutlow | df$flagouthigh)
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
    dplyr::mutate(!!paste0("flagjump", passobj("flagout_nr")) := flagjump) %>%
    dplyr::mutate(flagout = outlier) %>%
    dplyr::mutate(!!paste0("flagout", passobj("flagout_nr")) := flagout) %>%
    dplyr::select(1:nc, grep("^(flagout|flagjump)(?!.*low)(?!.*high)",
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
#' @examples
#'
executejump <- function(df) {
  # This function is Copyright
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
    dplyr::mutate(value = ifelse(flagout, NA, value)) %>%
    dplyr::select(1:nc)

  message("executejump is in development mode.")

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
#' @examples
#'
calcmax <- function(df) {
  # This function is Copyright
  nc <- ncol(df)
  first_val <- df$value[which(!is.na(df$value))[1]]

  df <- df %>%
    dplyr::mutate(val_nona = fill_na(value)) %>%
    dplyr::mutate(diff_nona = c(0, diff(val_nona, lag = 1))) %>%
    dplyr::mutate(diff_sum = cumsum(diff_nona))

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


#' Calculates TWD and GRO
#'
#' \code{calctwdgro} calculates the tree water deficit (twd) and the growth
#'   since the beginning of the year (gro_yr).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
calctwdgro  <- function(df, tz) {
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
#' @examples
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


#' Remove Consecutive Values
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
#' @examples
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
#' \code{calcmds} calculates the maximum daily shrinkage (mds). Mds is defined
#'   as the largest daily shrinkage. First, local maxima and minima are
#'   identified using a moving window. Mds is only calculated if a local
#'   maximum occurs before a local minimum (i.e. if the stem shrinks during
#'   the day).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @details \code{calcmds} is inspired by the function
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'
#' @keywords internal
#'
#' @examples
#'
calcmds <- function(df, tz, reso, plot_mds = FALSE) {

  #df <- lens_L2 %>%
  #  dplyr::filter(series == series[1]) %>%
  #  #dplyr::slice(1:20000) %>%
  #  dplyr::select(-mds)
  #tz <- "Etc/GMT-1"
  #reso <- 10
  #plot_mds <- TRUE

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
    plot_mds(df = df, maxmin = maxmin)
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

  message("calcmds is in development...")

  return(df)
}


#' Summarise Flags
#'
#' \code{summariseflags} summarises all previously created flags in one column.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
#' @examples
#'
summariseflags <- function(df) {

  list_flags <- vector("list", length = 2 + (passenv$flagout_nr * 2))

  list_flags[[1]] <- ifelse(df$flagrangemax, "rmax", NA)
  list_flags[[2]] <- ifelse(df$flagrangemin, "rmin", NA)

  n_flags <- 3
  for (out in n_flags:(passenv$flagout_nr + n_flags - 1)) {
    flagout_nr <- out - n_flags + 1
    flagout <- df[[paste0("flagout", flagout_nr)]]
    list_flags[[out]] <- ifelse(flagout, paste0("out", flagout_nr), NA)
  }
  n_flags <- n_flags + passenv$flagout_nr
  for (jump in n_flags:(passenv$flagout_nr + n_flags - 1)) {
    flagjump_nr <- jump - n_flags + 1
    flagjump <- df[[paste0("flagjump", flagjump_nr)]]
    list_flags[[jump]] <- ifelse(flagjump, paste0("jump", flagjump_nr), NA)
  }

  flags <- do.call("paste", c(list_flags, sep = ", "))
  flags <- gsub(", NA", "", flags)
  flags <- gsub("NA, ", "", flags)
  flags <- ifelse(flags == "NA", "", flags)

  df$flags <- flags

  message("summariseflags is in development mode.")

  return(df)
}
