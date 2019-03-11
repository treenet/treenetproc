#' Process Dendrometer Data
#'
#' \code{cleanoutofrange} removes implausible data points lower or higher
#' than specified values in \code{val_range}.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_dendro_L2
#'
#' @examples
#'
cleanoutofrange <- function(df, val_range) {
  df <- df %>%
    dplyr::mutate(flagrangemin = ifelse(value < val_range[1],
                                        TRUE, FALSE)) %>%
    dplyr::mutate(flagrangemax = ifelse(value > val_range[2],
                                        TRUE, FALSE)) %>%
    dplyr::mutate(value = ifelse(value < val_range[1], NA, value)) %>%
    dplyr::mutate(value = ifelse(value > val_range[2], NA, value))

  return(df)
}


#' Creates Flag for Gaps
#'
#' \code{creategapflag} adds a flag to gaps that are longer than
#' \code{gaplength}. Used to be called \code{cleanaftergaps}.
#'
#' @param df input \code{data.frame}.
#'
#' @param gaplength specify minimum length of gap for flagging.
#'
#' @inheritParams proc_dendro_L2
#'
#' @examples
#'
creategapflag <- function(df, reso, gaplength = 12 * (60 / reso)) {
  # This function is Copyright
  if ("gapflag" %in% colnames(df)) {
    df <- dplyr::select(df, -gapflag)
  }

  wnd <- gaplength
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
#' adapted from \code{na.locf} of the \code{zoo} package).
#'
#' @param x input \code{vector}.
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
#' are gaps, the difference is also calculated at an hourly rate respective to
#' the length of the preceding gap.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_dendro_L2
#'
#' @examples
#'
calcdiff <- function(df, reso) {
  ### Should gaplength be the number of missing timestamps or the number
  ### + 1 (to include also timestep to next nonna value)?
  ### at the moment it is the number of missing timestamps!!!
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
    # calculate hourly change in value
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
#'
#' @param lowtemp specifies temperature below which shrinkage in stem diameter
#' due to frost is expected.
#'
#' @inheritParams proc_dendro_L2
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


#' Removes Outliers Based on Quantile
#'
#' \code{removeoutliers} removes outliers that are above or below a specified
#' quantile within a defined window. (used to be implemented in createflags1)
#'
#' @param df input \code{data.frame}.
#'
#' @param quan quantile that identifies outliers.
#'
#' @param span size of window for which quantiles are calculated.
#'
#' @param by spacing between time windows for quantile calculation.
#'
#' @inheritParams proc_dendro_L2
#'
#' @examples
#'
removeoutliers <- function(df, quan = 0.001, wnd = 3, reso,
                           span = 60 / reso * 24 * (wnd / 2),
                           by = 60 / reso * 24) {
  # This function is Copyright
  if (nrow(df) < 2 * span) {
    message("you don't have enough data for window flag!")
  }
  st <- span + 1
  en <- nrow(df) - span + 1
  steps <- seq(st, en, by = by)
  if (length(steps) == 0) {
    stop("something went terribly wrong with the timestamp")
  }

  df$flagqlow <- FALSE
  df$flagqhigh <- FALSE
  for (qq in steps) {
    b1 <- qq - span; b2 <- qq + span - 1; ran <- b1:b2
    low <- quantile(df$diff_val[ran], quan, na.rm = TRUE)
    hig <- quantile(df$diff_val[ran], 1 - quan, na.rm = TRUE)
    if (!is.na(low)) {
      df$flagqlow[ran][df$diff_val[ran] < low] <- TRUE
    }
    if (!is.na(hig)) {
      df$flagqhigh[ran][df$diff_val[ran] > hig] <- TRUE
    }
  }

  df <- df %>%
    dplyr::mutate(value = ifelse(flagqlow, NA, value)) %>%
    dplyr::mutate(value = ifelse(flagqhigh, NA, value))

  return(df)
}


#' Create Flag for Abrubt Changes in Data
#'
#' \code{createflagdiff} creates a flag for large differences in value.
#'
#' @param df input \{data.frame}.
#'
#' @inheritParams proc_dendro_L2
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
      abs(diff_val) > diffwin / (60 / reso) & frost ~ TRUE
    )) %>%
    dplyr::mutate(flagdiff = ifelse(is.na(flagdiff), FALSE, flagdiff)) %>%
    dplyr::mutate(!!paste0("flagdiff", flagdiff_nr) := flagdiff)

  return(df)
}


#' Remove Values With Diff Flag
#'
#' \code{executeflagdiff} removes consecutive values with large differences.
#'
#' @param df input \code{data.frame}.
#'
#' @param length specifies the minimal number of consecutive flags above which
#' values are overwritten with NA.
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


#' Creates Flag for Outliers and Jumps in Data
#'
#' \code{createjumpoutflag} creates flag for jumps and ouliers.
#'
#' @param df input \code{data.frame}.
#'
#' @param thr specifies the threshold to discriminate between outliers and
#' jumps due to adjustments of the dendrometer needle.
#'
#' @examples
#'
createjumpoutflag <- function(df, thr = 0.2) {
  # This function is Copyright
  df$flagout <- FALSE
  df$flagjump <- FALSE
  ran <- which(df$flagdiff)

  if (length(ran) != 0) {
    for (iu in ran) {
      hhj <- sum(df$diff_val[(iu + 1):(iu + 3)], na.rm = TRUE)
      if (abs(df$diff_val[iu] + hhj) < df$diff_val[iu] * thr) {
        df$flagout[iu] <- TRUE
      } else {
        df$flagjump[iu] <- TRUE
      }
    }
  }
  return(df)
}


#' Remove Jumps and Outliers
#'
#' \code{executejumpout} removes jumps that occur after resetting the dendrometer
#' needle and removes outliers.
#'
#' @param df input \code{data.frame}.
#'
#' @examples
#'
executejumpout <- function(df) {
  # This function is Copyright
  nc <- ncol(df)
  le <- nrow(df)
  ran <- which(df$flagjump)

  diff_jump <- df %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff_jump = c(NA, diff(value))) %>%
    dplyr::select(ts, diff_jump)
  df <- dplyr::left_join(df, diff_jump, by = "ts")

  if (length(ran) > 0) {
    for (uu in c(1:length(ran))) {
      zz <- ran[uu]
      dx <- df$gapflag[(zz - 1)]
      dx <- dx + 1
      df$value[zz:le] <- df$value[zz:le] - (df$diff_jump[zz] * dx)
    }
  }

  df <- df %>%
    dplyr::mutate(value = ifelse(flagout, NA, value)) %>%
    dplyr::select(1:nc)

  return(df)
}


#' Calculate Maximum
#'
#' \code{calcmax} calculates the maximum of measured values.
#'
#' @param df input \code{data.frame}.
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
#' since the beginning of the year (gro_year).
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_dendro_L2
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
    dplyr::mutate(gro_year = cumsum(gro)) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:nc, twd, gro_year)

  return(df)
}


#' Calculate Maximum Daily Shrinkage (MDS)
#'
#' \code{calcmds} calculates the maximum daily shrinkage (mds). Mds is defined
#' as the largest daily shrinkage.
#'
#' @param df input \code{data.frame}.
#'
#' @examples
#'
calcmds <- function(df, tz) {
  nc <- ncol(df)

  maxmin <- df %>%
    dplyr::mutate(day = as.POSIXct(substr(ts, 1, 10), format = "%Y-%m-%d",
                                   tz = tz)) %>%
    dplyr::select(ts, day, value) %>%
    dplyr::mutate(diff_sign = c(sign(diff(value)), NA)) %>%
    dplyr::mutate(diff_sign_lag = dplyr::lag(diff_sign, n = 1)) %>%
    dplyr::mutate(sign_change = diff_sign * diff_sign_lag) %>%
    dplyr::mutate(min1 = ifelse(sign_change == -1 & diff_sign_lag == -1,
                               value, NA)) %>%
    dplyr::mutate(max1 = ifelse(sign_change == -1 & diff_sign_lag == 1,
                               value, NA))

  span <- 60 / reso * 6
  by <- 60 / reso * 6
  st <- span + 1
  en <- nrow(maxmin) - span + 1
  steps <- seq(st, en, by = by)

  maxmin$max2 <- 0
  maxmin$min2 <- 0
  for (qq in steps) {
    b1 <- qq - span; b2 <- qq + span - 1; ran <- b1:b2
    max_row <- which.max(maxmin$max1[ran]) + ran[1] - 1
    if (length(max_row) > 0) {
      maxmin$max2[max_row] <- maxmin$max2[max_row] + 1
    }
    min_row <- which.min(maxmin$min1[ran]) + ran[1] - 1
    if (length(min_row) > 0) {
      maxmin$min2[min_row] <- maxmin$min2[min_row] + 1
    }
  }

  maxmin <- maxmin %>%
    dplyr::filter(max2 == 2 | min2 == 2) %>%
    dplyr::mutate(max2 = rep(rle(max2)[[1]],
                                   times = rle(max2)[[1]])) %>%
    dplyr::mutate(iscons = ifelse(max2 == 2 & !is.na(max2), TRUE, FALSE)) %>%
    dplyr::mutate(cons = cumsum(iscons)) %>%
    dplyr::mutate(y = c(0, diff(cons, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(cons_nr = cumsum(z)) %>%
    dplyr::group_by(cons_nr) %>%
    dplyr::mutate(max2 = max(max1, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(max1 = dplyr::case_when(iscons & max1 == max2 ~ max1,
                  !iscons ~ max1)) %>%
    dplyr::mutate(min2 = rep(rle(min2)[[1]],
                             times = rle(min2)[[1]])) %>%
    dplyr::mutate(iscons = ifelse(min2 == 2 & !is.na(min2), TRUE, FALSE)) %>%
    dplyr::mutate(cons = cumsum(iscons)) %>%
    dplyr::mutate(y = c(0, diff(cons, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(cons_nr = cumsum(z)) %>%
    dplyr::group_by(cons_nr) %>%
    dplyr::mutate(min2 = min(min1, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(min1 = dplyr::case_when(iscons & min1 == min2 ~ min1,
                                         !iscons ~ min1)) %>%
    dplyr::filter(!(is.na(max1) & is.na(min1))) %>%
    dplyr::select(ts, day, max1, min1) %>%
    dplyr::group_by(day) %>%
    dplyr::mutate(min_lag = dplyr::lead(min1, n = 1)) %>%
    dplyr::mutate(mds = max1 - min_lag) %>%
    dplyr::mutate(mds = max(mds, na.rm = T)) %>%
    dplyr::mutate(mds = ifelse(mds >= 0, mds, NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(day) %>%
    dplyr::summarise(mds = mds[1]) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(day = as.POSIXct(substr(ts, 1, 10), format = "%Y-%m-%d",
                                   tz = tz)) %>%
    dplyr::full_join(., maxmin, by = "day") %>%
    dplyr::select(1:nc, mds)

  return(df)
}


#' Summarise Flags
#'
#' \code{summariseflags} summarises all previously created flags in one column.
#'
#' @param df input \code{data.frame}.
#'
#' @examples
#'
summariseflags <- function(df) {
  df <- df %>%
    dplyr::mutate(flagrangemax = ifelse(flagrangemax, 2 ^ 0, 0)) %>%
    dplyr::mutate(flagrangemin = ifelse(flagrangemin, 2 ^ 1, 0)) %>%
    dplyr::mutate(flagqlow = ifelse(flagqlow, 2 ^ 2, 0)) %>%
    dplyr::mutate(flagqhigh = ifelse(flagqhigh, 2 ^ 3, 0)) %>%
    dplyr::mutate(flagdiff1 = ifelse(flagdiff1, 2 ^ 4, 0)) %>%
    dplyr::mutate(flagdiff2 = ifelse(flagdiff2, 2 ^ 5, 0)) %>%
    dplyr::mutate(flagout = ifelse(flagout, 2 ^ 6, 0)) %>%
    dplyr::mutate(flagjump = ifelse(flagjump, 2 ^ 7, 0)) %>%
    dplyr::mutate(
      flags = rowSums(dplyr::select(., "flagrangemax", "flagrangemin",
                                    "flagqlow", "flagqhigh",
                                    "flagdiff1", "flagdiff2",
                                    "flagout", "flagjump"))) %>%
    dplyr::select(-flagrangemax, -flagrangemin, -flagqlow, -flagqhigh,
                  -flagdiff, -flagdiff1, -flagdiff2, -flagout, -flagjump)

  return(df)
}
